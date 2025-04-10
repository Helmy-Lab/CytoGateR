library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(openxlsx)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(Rtsne)
library(uwot)
library(cluster)

options(shiny.maxRequestSize = 100 * 1024^2)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Flow Cytometry Analysis Tool for Cleaned Data"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV/TSV/Excel File", accept = c(".csv", ".tsv", ".xlsx")),
      uiOutput("analysis_type_ui"),
      uiOutput("marker_ui"),
      uiOutput("treatment_ui"),
      selectInput("dimred_method", "Dimensionality Reduction Method", choices = c("t-SNE", "UMAP")),
      numericInput("perplexity", "t-SNE: Perplexity", value = 5, min = 2, max = 50),
      numericInput("neighbors", "UMAP: n_neighbors", value = 5, min = 2, max = 100),
      numericInput("min_dist", "UMAP: min_dist", value = 0.1, min = 0, max = 1, step = 0.05),
      numericInput("n_clusters", "Number of Clusters (k-means)", value = 3, min = 1),
      sliderInput("plot_height", "Plot Height (px)", min = 300, max = 1200, value = 600, step = 50),
      sliderInput("plot_width", "Plot Width (px)", min = 300, max = 1200, value = 600, step = 50),
      actionButton("run", "Run Analysis", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", DTOutput("preview")),
        tabPanel("Structure Detection", verbatimTextOutput("structure")),
        tabPanel("Results Plot", plotlyOutput("plot")),
        tabPanel("t-SNE / UMAP", plotlyOutput("dimred_plot", width = "auto", height = "auto")),
        tabPanel("Summary Table", DTOutput("summary_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = read_csv(input$file$datapath),
           tsv = read_tsv(input$file$datapath),
           xlsx = read.xlsx(input$file$datapath),
           validate("Unsupported file format"))
  })
  
  markers <- reactive({
    req(data())
    grep("^[48]", colnames(data()), value = TRUE)
  })
  
  output$preview <- renderDT({
    req(data())
    datatable(head(data(), 10), options = list(scrollX = TRUE))
  })
  
  output$structure <- renderPrint({
    req(data())
    list(
      Columns = colnames(data()),
      CD4 = grep("^4", colnames(data()), value = TRUE),
      CD8 = grep("^8", colnames(data()), value = TRUE)
    )
  })
  
  output$analysis_type_ui <- renderUI({
    selectInput("analysis_type", "Select Analysis Type",
                choices = c("Marker Comparison", "Treatment Comparison",
                            "Fold Change", "Summary Statistics"))
  })
  
  output$marker_ui <- renderUI({
    req(markers())
    pickerInput("selected_markers", "Select Markers", choices = markers(),
                multiple = TRUE, selected = markers()[1], options = list(`actions-box` = TRUE))
  })
  
  output$treatment_ui <- renderUI({
    req(data())
    selectInput("selected_treatment", "Select Treatment Column",
                choices = names(data())[sapply(data(), is.character)])
  })
  
  output$plot <- renderPlotly({
    req(input$run, data(), input$selected_markers, input$selected_treatment)
    df <- data()
    df_long <- pivot_longer(df, cols = all_of(input$selected_markers),
                            names_to = "Marker", values_to = "Value")
    
    p <- ggplot(df_long, aes_string(x = input$selected_treatment, y = "Value", color = "Marker")) +
      geom_point(position = position_jitter(width = 0.2), alpha = 0.7) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$dimred_plot <- renderPlotly({
    req(input$run, data(), input$selected_markers, input$selected_treatment)
    df <- data()
    df_sel <- df[, input$selected_markers, drop = FALSE] %>%
      mutate(across(everything(), as.numeric)) %>%
      na.omit()
    
    if (nrow(df_sel) < 3) {
      showNotification("Not enough data for dimensionality reduction.", type = "error")
      return(NULL)
    }
    
    treatment <- data()[as.numeric(rownames(df_sel)), input$selected_treatment]
    sample_ids <- if ("Sample" %in% colnames(data())) data()[as.numeric(rownames(df_sel)), "Sample"] else paste("Sample", seq_len(nrow(df_sel)))
    
    set.seed(42)  # reproducibility
    dimred <- switch(input$dimred_method,
                     "t-SNE" = Rtsne(df_sel, perplexity = input$perplexity, verbose = FALSE, check_duplicates = FALSE)$Y,
                     "UMAP" = umap(df_sel, n_neighbors = input$neighbors, min_dist = input$min_dist))
    
    cluster_labels <- as.factor(kmeans(df_sel, centers = input$n_clusters, nstart = 10)$cluster)
    
    dimred_df <- data.frame(Dim1 = dimred[, 1], Dim2 = dimred[, 2],
                            Treatment = treatment,
                            Cluster = cluster_labels,
                            Sample = sample_ids)
    
    plot_ly(dimred_df,
            x = ~Dim1, y = ~Dim2,
            color = ~Treatment,
            text = ~paste("Sample:", Sample, "<br>Cluster:", Cluster),
            colors = "Set1",
            type = "scatter", mode = "markers",
            width = input$plot_width, height = input$plot_height) %>%
      layout(
        title  = list(text = paste(input$dimred_method, "Projection"), font = list(size = 18)),
        xaxis  = list(title = "Dim 1", titlefont = list(size = 14), tickfont = list(size = 12), scaleanchor = "y", scaleratio = 1),
        yaxis  = list(title = "Dim 2", titlefont = list(size = 14), tickfont = list(size = 12)),
        legend = list(x = 1.02, y = 0.5),   # move legend outside plot area
        margin = list(l = 50, r = 120, b = 50, t = 50)
      )
  })
  
  output$summary_table <- renderDT({
    req(input$run, data(), input$selected_markers, input$selected_treatment)
    df <- data()
    df_long <- pivot_longer(df, cols = all_of(input$selected_markers),
                            names_to = "Marker", values_to = "Value")
    
    summary_df <- df_long %>%
      group_by(across(all_of(input$selected_treatment)), Marker) %>%
      summarise(Mean = mean(Value, na.rm = TRUE), SD = sd(Value, na.rm = TRUE), .groups = "drop")
    
    datatable(summary_df)
  })
}

shinyApp(ui, server)