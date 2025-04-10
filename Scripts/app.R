library(shiny)
library(flowCore)
library(ggplot2)
library(Rtsne)
library(uwot)
library(data.table)
library(plotly)
library(shinycssloaders)

options(shiny.maxRequestSize = 100*1024^2)

asinhTransform <- function(x, cofactor = 5) {
  asinh(x / cofactor)
}

ui <- fluidPage(
  titlePanel("Flow Cytometry: Dimensionality Reduction for Raw Data"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fcsFile", "Upload FCS/CSV/TSV File", accept = c(".fcs", ".csv", ".tsv")),
      uiOutput("markerSelectUI"),
      checkboxInput("transform", "Apply arcsinh transformation", value = TRUE),
      numericInput("cofactor", "Transformation cofactor", value = 5, min = 1, max = 10),
      numericInput("nEvents", "Number of events to sample", value = 5000, min = 100, step = 1000),
      sliderInput("perplexity", "t-SNE perplexity", min = 5, max = 50, value = 30),
      sliderInput("n_neighbors", "UMAP n_neighbors", min = 2, max = 100, value = 15),
      checkboxGroupInput("methods", "Select Dimensionality Reduction Methods", choices = c("t-SNE", "UMAP"), selected = c("t-SNE", "UMAP")),
      selectInput("colorBy", "Color points by:", choices = c("None")),
      sliderInput("plotHeight", "Plot height (px)", min = 300, max = 1200, value = 600, step = 50),
      sliderInput("plotWidth", "Plot width (px)", min = 300, max = 1200, value = 600, step = 50),
      actionButton("run", "Run Analysis", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("t-SNE", withSpinner(plotlyOutput("tsnePlot", height = "600px"))),
        tabPanel("UMAP", withSpinner(plotlyOutput("umapPlot", height = "600px"))),
        tabPanel("Data Info", verbatimTextOutput("fcsInfo"))
      )
    )
  )
)

server <- function(input, output, session) {
  rawFCS <- reactive({
    req(input$fcsFile)
    ext <- tools::file_ext(input$fcsFile$name)
    switch(ext,
           "fcs" = read.FCS(input$fcsFile$datapath, transformation = FALSE),
           "csv" = fread(input$fcsFile$datapath),
           "tsv" = fread(input$fcsFile$datapath, sep = "\t"),
           stop("Unsupported file format")
    )
  })
  
  output$fcsInfo <- renderPrint({
    fcs <- rawFCS()
    cat("File name: ", input$fcsFile$name, "\n")
    cat("Dimensions: ", paste(dim(fcs), collapse = " x "), "\n")
    if (inherits(fcs, "flowFrame")) {
      cat("Parameters:\n")
      print(head(parameters(fcs)[, c("name", "desc")], 20))
    } else {
      cat("Column names:\n")
      print(colnames(fcs))
    }
  })
  
  output$markerSelectUI <- renderUI({
    data <- rawFCS()
    if (inherits(data, "flowFrame")) {
      exprs_data <- exprs(data)
      params <- parameters(data)
      choices <- setNames(colnames(exprs_data), paste0(colnames(exprs_data), " - ", params$desc))
    } else {
      exprs_data <- data
      choices <- colnames(exprs_data)
    }
    
    selectInput("selectedMarkers", "Select Markers/Channels", choices = choices, selected = choices[1:min(5, length(choices))], multiple = TRUE)
  })
  
  observe({
    req(input$selectedMarkers)
    markers <- input$selectedMarkers
    updateSelectInput(session, "colorBy", choices = c("None", markers))
  })
  
  processedData <- reactiveVal(NULL)
  
  observeEvent(input$run, {
    req(input$selectedMarkers)
    
    withProgress(message = 'Processing data...', value = 0, {
      data <- rawFCS()
      if (inherits(data, "flowFrame")) {
        exprs_data <- exprs(data)[, input$selectedMarkers, drop = FALSE]
      } else {
        exprs_data <- data[, input$selectedMarkers, drop = FALSE]
      }
      
      set.seed(123)
      sampled_indices <- sample(nrow(exprs_data), min(input$nEvents, nrow(exprs_data)))
      sampled_data <- exprs_data[sampled_indices, ]
      
      incProgress(0.2, detail = "Transforming data...")
      if (input$transform) {
        sampled_data <- apply(sampled_data, 2, asinhTransform, cofactor = input$cofactor)
      }
      
      scaled_data <- scale(sampled_data)
      
      results <- list(raw_data = sampled_data, scaled_data = scaled_data)
      
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
  
  output$tsnePlot <- renderPlotly({
    req(processedData(), "t-SNE" %in% input$methods)
    plot_data <- processedData()$plot_data
    req("tsne1" %in% colnames(plot_data))
    
    p <- ggplot(plot_data, aes(x = tsne1, y = tsne2))
    if (!is.null(input$colorBy) && input$colorBy != "None") {
      p <- p + geom_point(aes(color = .data[[input$colorBy]], text = paste("Value:", .data[[input$colorBy]])), alpha = 0.7) +
        scale_color_viridis_c() +
        labs(color = input$colorBy)
    } else {
      p <- p + geom_point(aes(text = rownames(plot_data)), alpha = 0.7)
    }
    p <- p + labs(title = "t-SNE Projection", x = "t-SNE 1", y = "t-SNE 2") +
      theme_minimal(base_size = 16) +
      theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))
    ggplotly(p, tooltip = "text") %>% layout(height = input$plotHeight, width = input$plotWidth)
  })
  
  output$umapPlot <- renderPlotly({
    req(processedData(), "UMAP" %in% input$methods)
    plot_data <- processedData()$plot_data
    req("umap1" %in% colnames(plot_data))
    
    p <- ggplot(plot_data, aes(x = umap1, y = umap2))
    if (!is.null(input$colorBy) && input$colorBy != "None") {
      p <- p + geom_point(aes(color = .data[[input$colorBy]], text = paste("Value:", .data[[input$colorBy]])), alpha = 0.7) +
        scale_color_viridis_c() +
        labs(color = input$colorBy)
    } else {
      p <- p + geom_point(aes(text = rownames(plot_data)), alpha = 0.7)
    }
    p <- p + labs(title = "UMAP Projection", x = "UMAP 1", y = "UMAP 2") +
      theme_minimal(base_size = 16) +
      theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))
    ggplotly(p, tooltip = "text") %>% layout(height = input$plotHeight, width = input$plotWidth)
  })
}

shinyApp(ui = ui, server = server)