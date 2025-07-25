library(shiny)
library(shinythemes)
library(shinyWidgets)
library(DT)
library(ggplot2)
library(plotly)
library(Rtsne)
library(uwot)
library(cluster)
library(dplyr)
library(tidyr)
library(readr)
library(openxlsx)
library(flowCore)
library(data.table)
library(shinycssloaders)

options(shiny.maxRequestSize = 100 * 1024^2)

asinhTransform <- function(x, cofactor = 5) {
  asinh(x / cofactor)
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Flow Cytometry Analysis Tool"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataMode", "Select Data Type", choices = c("Cleaned Data", "Raw FCS/CSV/TSV")),
      conditionalPanel(
        condition = "input.dataMode == 'Cleaned Data'",
        fileInput("cleanedFile", "Upload CSV/TSV/Excel File", accept = c(".csv", ".tsv", ".xlsx"))
      ),
      conditionalPanel(
        condition = "input.dataMode == 'Raw FCS/CSV/TSV'",
        fileInput("rawFile", "Upload FCS/CSV/TSV File", accept = c(".fcs", ".csv", ".tsv"))
      ),
      hr(),
      uiOutput("markerSelectUI"),
      checkboxInput("transform", "Apply arcsinh transformation", value = TRUE),
      numericInput("cofactor", "Transformation cofactor", value = 5, min = 1, max = 10),
      numericInput("nEvents", "Number of events to sample", value = 5000, min = 100, step = 1000),
      selectInput("dimredMethod", "Dimensionality Reduction Method", choices = c("t-SNE", "UMAP")),
      numericInput("perplexity", "t-SNE: Perplexity", value = 30, min = 5, max = 50),
      numericInput("n_neighbors", "UMAP: n_neighbors", value = 15, min = 2, max = 100),
      numericInput("min_dist", "UMAP: min_dist", value = 0.1, min = 0, max = 1, step = 0.05),
      numericInput("n_clusters", "Number of Clusters (k-means)", value = 3, min = 1),
      selectInput("colorBy", "Color points by:", choices = c("None")),
      sliderInput("plotHeight", "Plot height (px)", min = 300, max = 1200, value = 600),
      sliderInput("plotWidth", "Plot width (px)", min = 300, max = 1200, value = 600),
      actionButton("run", "Run Analysis", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Preview", DTOutput("preview")),
        tabPanel("Structure Info", verbatimTextOutput("structure")),
        tabPanel("Dimensionality Reduction", withSpinner(plotlyOutput("dimredPlot", height = "auto", width = "auto"))),
        tabPanel("Summary Stats", DTOutput("summaryTable"))
      )
    )
  )
)

server <- function(input, output, session) {
  rawData <- reactive({
    if (input$dataMode == "Raw FCS/CSV/TSV") {
      req(input$rawFile)
      ext <- tools::file_ext(input$rawFile$name)
      switch(ext,
             "fcs" = read.FCS(input$rawFile$datapath, transformation = FALSE),
             "csv" = fread(input$rawFile$datapath),
             "tsv" = fread(input$rawFile$datapath, sep = "\t"),
             stop("Unsupported file format"))
    } else {
      req(input$cleanedFile)
      ext <- tools::file_ext(input$cleanedFile$name)
      switch(ext,
             "csv" = read_csv(input$cleanedFile$datapath),
             "tsv" = read_tsv(input$cleanedFile$datapath),
             "xlsx" = read.xlsx(input$cleanedFile$datapath),
             stop("Unsupported file format"))
    }
  })
  
  output$markerSelectUI <- renderUI({
    df <- rawData()
    if (inherits(df, "flowFrame")) {
      exprs_data <- exprs(df)
      params <- parameters(df)
      choices <- setNames(colnames(exprs_data), paste0(colnames(exprs_data), " - ", params$desc))
    } else {
      exprs_data <- df
      choices <- colnames(exprs_data)
    }
    selectInput("selectedMarkers", "Select Markers/Channels", choices = choices, multiple = TRUE,
                selected = choices[1:min(5, length(choices))])
  })
  
  observe({
    req(input$selectedMarkers)
    updateSelectInput(session, "colorBy", choices = c("None", input$selectedMarkers))
  })
  
  output$preview <- renderDT({
    df <- rawData()
    if (inherits(df, "flowFrame")) {
      datatable(head(exprs(df), 10))
    } else {
      datatable(head(df, 10))
    }
  })
  
  output$structure <- renderPrint({
    df <- rawData()
    if (inherits(df, "flowFrame")) {
      cat("FCS File Info:\n")
      print(head(parameters(df)[, c("name", "desc")], 10))
    } else {
      cat("Data Frame Structure:\n")
      str(df)
    }
  })
  
  output$summaryTable <- renderDT({
    req(input$dataMode == "Cleaned Data", input$selectedMarkers)
    df <- rawData()
    treatment_col <- names(df)[sapply(df, is.character)][1]
    df_long <- pivot_longer(df, cols = all_of(input$selectedMarkers), names_to = "Marker", values_to = "Value")
    summary_df <- df_long %>%
      group_by(across(all_of(treatment_col)), Marker) %>%
      summarise(Mean = mean(Value, na.rm = TRUE), SD = sd(Value, na.rm = TRUE), .groups = "drop")
    datatable(summary_df)
  })
  
  output$dimredPlot <- renderPlotly({
    req(input$selectedMarkers)
    df <- rawData()
    
    if (inherits(df, "flowFrame")) {
      exprs_data <- exprs(df)[, input$selectedMarkers, drop = FALSE]
    } else {
      exprs_data <- df[, input$selectedMarkers, drop = FALSE]
    }
    
    exprs_data <- as.data.frame(exprs_data)
    exprs_data <- exprs_data %>%
      mutate(across(everything(), as.numeric)) %>%
      na.omit()
    
    if (nrow(exprs_data) < 3) {
      showNotification("Not enough data for dimensionality reduction.", type = "error")
      return(NULL)
    }
    
    set.seed(123)
    sampled <- exprs_data
    
    if (input$transform) {
      sampled <- apply(sampled, 2, asinhTransform, cofactor = input$cofactor)
      sampled <- as.data.frame(sampled)
    }
    
    sampled <- scale(sampled)
    
    dimred <- switch(input$dimredMethod,
                     "t-SNE" = Rtsne(sampled, dims = 2, perplexity = input$perplexity, verbose = FALSE)$Y,
                     "UMAP" = umap(sampled, n_neighbors = input$n_neighbors, min_dist = input$min_dist))
    
    cluster_labels <- as.factor(kmeans(sampled, centers = input$n_clusters, nstart = 10)$cluster)
    
    plot_df <- as.data.frame(sampled)
    colnames(plot_df)[1:ncol(sampled)] <- input$selectedMarkers
    plot_df$Dim1 <- dimred[,1]
    plot_df$Dim2 <- dimred[,2]
    plot_df$Cluster <- cluster_labels
    
    if (!is.null(input$colorBy) && input$colorBy != "None") {
      plot_df$ColorBy <- exprs_data[1:nrow(plot_df), input$colorBy]
    }
    
    p <- ggplot(plot_df, aes(x = Dim1, y = Dim2)) +
      geom_point(aes_string(color = ifelse(input$colorBy != "None", "ColorBy", "Cluster")), alpha = 0.7) +
      labs(title = paste(input$dimredMethod, "Projection"),
           x = "Dim 1", y = "Dim 2") +
      theme_minimal()
    
    ggplotly(p) %>% layout(height = input$plotHeight, width = input$plotWidth)
  })
}

shinyApp(ui = ui, server = server)
