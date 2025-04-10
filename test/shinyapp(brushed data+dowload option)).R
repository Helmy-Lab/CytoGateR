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

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Flow Cytometry Analysis Tool"),
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
      sliderInput("plot_width", "Plot Width (px)", min = 300, max = 1200, value = 600, step = 50),
      sliderInput("plot_height", "Plot Height (px)", min = 300, max = 1200, value = 600, step = 50),
      actionButton("run", "Run Analysis", class = "btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", DTOutput("preview")),
        tabPanel("Structure Detection", verbatimTextOutput("structure")),
        tabPanel("Results Plot", plotlyOutput("plot")),
        tabPanel("t-SNE / UMAP", 
                 plotlyOutput("dimred_plot", width = "auto", height = "auto"),
                 br(), 
                 DTOutput("brushedData"),
                 downloadButton("download", "Download Brushed Data")),
        tabPanel("Summary Table", DTOutput("summary_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive data upload logic
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    switch(ext,
           csv = read_csv(input$file$datapath),
           tsv = read_tsv(input$file$datapath),
           xlsx = read.xlsx(input$file$datapath),
           validate("Unsupported file format"))
  })
  
  # Processed data for t-SNE/UMAP
  processedData <- reactive({
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
    
    set.seed(42)  # Ensure reproducibility
    dimred <- switch(input$dimred_method,
                     "t-SNE" = Rtsne(df_sel, perplexity = input$perplexity, verbose = FALSE, check_duplicates = FALSE)$Y,
                     "UMAP" = umap(df_sel, n_neighbors = input$neighbors, min_dist = input$min_dist))
    
    cluster_labels <- as.factor(kmeans(df_sel, centers = input$n_clusters, nstart = 10)$cluster)
    
    dimred_df <- data.frame(Dim1 = dimred[,1], Dim2 = dimred[,2], 
                            Treatment = treatment, 
                            Cluster = cluster_labels,
                            Sample = sample_ids)
    
    list(plot_data = dimred_df, raw_data = df)
  })
  
  # Render brushed data (t-SNE and UMAP)
  output$brushedData <- renderTable({
    req(processedData())
    plot_data <- processedData()$plot_data
    
    # Capture brushed points from t-SNE or UMAP
    brushed_tsne <- brushedPoints(plot_data, input$dimred_plot_brush)  # Assuming you are brushing on dimred_plot
    brushed_umap <- brushedPoints(plot_data, input$dimred_plot_brush)  # Same for UMAP
    
    # Return non-empty brushed data
    if (nrow(brushed_tsne) > 0) return(brushed_tsne)
    if (nrow(brushed_umap) > 0) return(brushed_umap)
    NULL
  })
  
  # Download Handler for Brushed Data
  output$download <- downloadHandler(
    filename = function() {
      paste0("brushed_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      brushed_data <- output$brushedData()  # Get the brushed data
      if (!is.null(brushed_data)) {
        write.csv(brushed_data, file, row.names = FALSE)
      }
    }
  )
  
  
}

shinyApp(ui, server)
