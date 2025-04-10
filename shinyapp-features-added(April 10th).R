library(shiny)
library(flowCore)
library(ggplot2)
library(Rtsne)
library(uwot)
library(data.table)
library(shinycssloaders)

options(shiny.maxRequestSize = 100 * 1024^2)

asinhTransform <- function(x, cofactor = 5) {
  asinh(x / cofactor)
}

ui <- fluidPage(
  titlePanel("Flow Cytometry: t-SNE & UMAP Dimensionality Reduction"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("dataFile", "Upload FCS/CSV/TSV File", accept = c(".fcs", ".csv", ".tsv")),
      uiOutput("markerSelectUI"),
      
      checkboxInput("transform", "Apply arcsinh transformation", value = TRUE),
      numericInput("cofactor", "Transformation cofactor", value = 5, min = 1, max = 10),
      
      numericInput("nEvents", "Number of events to sample", value = 5000, min = 100, step = 1000),
      
      sliderInput("perplexity", "t-SNE perplexity", min = 5, max = 50, value = 30),
      sliderInput("n_neighbors", "UMAP n_neighbors", min = 2, max = 100, value = 15),
      
      checkboxGroupInput("methods", "Select Dimensionality Reduction Methods",
                         choices = c("t-SNE", "UMAP"), selected = c("t-SNE", "UMAP")),
      
      selectInput("colorBy", "Color points by:", choices = c("None")),
      
      actionButton("run", "Run Analysis", class = "btn-primary"),
      downloadButton("download", "Download Brushed Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("t-SNE", withSpinner(plotOutput("tsnePlot", brush = "tsnePlot_brush", height = "400px"))),
        tabPanel("UMAP", withSpinner(plotOutput("umapPlot", brush = "umapPlot_brush", height = "400px"))),
        tabPanel("Data Info", verbatimTextOutput("fcsInfo")),
        tabPanel("Brushed Points", tableOutput("brushedData"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  rawData <- reactive({
    req(input$dataFile)
    file_path <- input$dataFile$datapath
    ext <- tools::file_ext(file_path)
    
    if (ext == "fcs") {
      data <- read.FCS(file_path, transformation = FALSE)
      exprs(data)
    } else if (ext == "csv") {
      fread(file_path)
    } else if (ext == "tsv") {
      fread(file_path, sep = "\t")
    } else {
      validate("Unsupported file format. Please upload a .fcs, .csv, or .tsv file.")
    }
  })
  
  output$fcsInfo <- renderPrint({
    req(input$dataFile)
    ext <- tools::file_ext(input$dataFile$name)
    data <- rawData()
    
    cat("File name: ", input$dataFile$name, "\n")
    cat("File type: ", ext, "\n")
    cat("Number of events: ", nrow(data), "\n")
    cat("Number of parameters: ", ncol(data), "\n\n")
    cat("Column names:\n")
    print(head(colnames(data), 20))
  })
  
  output$markerSelectUI <- renderUI({
    req(rawData())
    data <- rawData()
    
    choices <- setNames(colnames(data), colnames(data))
    
    exclude <- c("Time", "Event", "CellID")
    channels <- colnames(data)
    channels <- channels[!channels %in% exclude]
    
    selectInput("selectedMarkers", "Select Markers/Channels", 
                choices = choices, selected = channels[1:min(5, length(channels))], multiple = TRUE)
  })
  
  observe({
    req(input$selectedMarkers)
    updateSelectInput(session, "colorBy", choices = c("None", input$selectedMarkers))
  })
  
  processedData <- reactiveVal(NULL)
  
  observeEvent(input$run, {
    req(input$selectedMarkers)
    
    withProgress(message = 'Processing data...', value = 0, {
      data <- rawData()
      exprs_data <- data[, input$selectedMarkers, drop = FALSE]
      
      set.seed(123)
      sampled_indices <- sample(nrow(exprs_data), min(input$nEvents, nrow(exprs_data)))
      sampled_data <- exprs_data[sampled_indices, ]
      
      incProgress(0.2, detail = "Transforming data...")
      if (input$transform) {
        sampled_data <- apply(sampled_data, 2, asinhTransform, cofactor = input$cofactor)
      }
      
      scaled_data <- scale(sampled_data)
      
      results <- list(
        raw_data = sampled_data,
        scaled_data = scaled_data
      )
      
      if ("t-SNE" %in% input$methods) {
        incProgress(0.4, detail = "Running t-SNE...")
        tsne_result <- Rtsne(scaled_data, dims = 2, perplexity = input$perplexity, verbose = FALSE)
        results$tsne <- data.frame(tsne1 = tsne_result$Y[,1], tsne2 = tsne_result$Y[,2])
      }
      
      if ("UMAP" %in% input$methods) {
        incProgress(0.7, detail = "Running UMAP...")
        umap_result <- umap(scaled_data, n_neighbors = input$n_neighbors)
        results$umap <- data.frame(umap1 = umap_result[,1], umap2 = umap_result[,2])
      }
      
      plot_data <- as.data.frame(sampled_data)
      colnames(plot_data) <- input$selectedMarkers
      
      if (!is.null(results$tsne)) {
        plot_data$tsne1 <- results$tsne$tsne1
        plot_data$tsne2 <- results$tsne$tsne2
      }
      
      if (!is.null(results$umap)) {
        plot_data$umap1 <- results$umap$umap1
        plot_data$umap2 <- results$umap$umap2
      }
      
      results$plot_data <- plot_data
      processedData(results)
    })
  })
  
  output$tsnePlot <- renderPlot({
    req(processedData(), "t-SNE" %in% input$methods)
    req("tsne1" %in% colnames(processedData()$plot_data))
    
    plot_data <- processedData()$plot_data
    p <- ggplot(plot_data, aes(x = tsne1, y = tsne2))
    
    if (!is.null(input$colorBy) && input$colorBy != "None") {
      p <- p + geom_point(aes(color = .data[[input$colorBy]]), alpha = 0.7) +
        scale_color_viridis_c() +
        labs(color = input$colorBy)
    } else {
      p <- p + geom_point(alpha = 0.7)
    }
    
    p + labs(title = "t-SNE Projection", x = "t-SNE 1", y = "t-SNE 2") +
      theme_minimal()
  }, res = 96, height = 400, width = 400)
  
  output$umapPlot <- renderPlot({
    req(processedData(), "UMAP" %in% input$methods)
    req("umap1" %in% colnames(processedData()$plot_data))
    
    plot_data <- processedData()$plot_data
    p <- ggplot(plot_data, aes(x = umap1, y = umap2))
    
    if (!is.null(input$colorBy) && input$colorBy != "None") {
      p <- p + geom_point(aes(color = .data[[input$colorBy]]), alpha = 0.7) +
        scale_color_viridis_c() +
        labs(color = input$colorBy)
    } else {
      p <- p + geom_point(alpha = 0.7)
    }
    
    p + labs(title = "UMAP Projection", x = "UMAP 1", y = "UMAP 2") +
      theme_minimal()
  }, res = 96, height = 400, width = 400)
  
  output$brushedData <- renderTable({
    req(processedData())
    plot_data <- processedData()$plot_data
    
    brushed_tsne <- brushedPoints(plot_data, input$tsnePlot_brush)
    brushed_umap <- brushedPoints(plot_data, input$umapPlot_brush)
    
    # Return non-empty brush
    if (nrow(brushed_tsne) > 0) return(brushed_tsne)
    if (nrow(brushed_umap) > 0) return(brushed_umap)
    NULL
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("brushed_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      data <- output$brushedData()
      if (!is.null(data)) {
        write.csv(data, file, row.names = FALSE)
      }
    }
  )
}

shinyApp(ui = ui, server = server)

