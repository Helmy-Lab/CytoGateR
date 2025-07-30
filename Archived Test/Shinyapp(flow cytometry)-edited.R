library(shiny)
library(flowCore)
library(ggplot2)
library(Rtsne)
library(uwot)
library(data.table)
library(shinycssloaders)  # For loading spinners

# Increase file upload size to 100 MB
options(shiny.maxRequestSize = 100*1024^2)

# Simple arcsinh transformation function
asinhTransform <- function(x, cofactor = 5) {
  asinh(x / cofactor)
}

ui <- fluidPage(
  titlePanel("Flow Cytometry: t-SNE & UMAP Dimensionality Reduction"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fcsFile", "Upload FCS File", accept = ".fcs"),
      uiOutput("markerSelectUI"),
      
      # Simple data transformation option
      checkboxInput("transform", "Apply arcsinh transformation", value = TRUE),
      numericInput("cofactor", "Transformation cofactor", value = 5, min = 1, max = 10),
      
      numericInput("nEvents", "Number of events to sample", value = 5000, min = 100, step = 1000),
      
      # Add perplexity parameter for t-SNE
      sliderInput("perplexity", "t-SNE perplexity", min = 5, max = 50, value = 30),
      sliderInput("n_neighbors", "UMAP n_neighbors", min = 2, max = 100, value = 15),
      
      
      checkboxGroupInput("methods", "Select Dimensionality Reduction Methods",
                         choices = c("t-SNE", "UMAP"), selected = c("t-SNE", "UMAP")),
      
      # Add color by option
      selectInput("colorBy", "Color points by:", choices = c("None")),
      
      actionButton("run", "Run Analysis", class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("t-SNE", withSpinner(plotOutput("tsnePlot", height = "600px"))),
        tabPanel("UMAP", withSpinner(plotOutput("umapPlot", height = "600px"))),
        tabPanel("Data Info", verbatimTextOutput("fcsInfo"))
      )
    )
  )
)

server <- function(input, output, session) {
  rawFCS <- reactive({
    req(input$fcsFile)
    read.FCS(input$fcsFile$datapath, transformation = FALSE)
  })
  
  # Display FCS file information
  output$fcsInfo <- renderPrint({
    fcs <- rawFCS()
    cat("FCS File Information:\n")
    cat("---------------------\n")
    cat("File name: ", input$fcsFile$name, "\n")
    cat("Number of events: ", nrow(exprs(fcs)), "\n")
    cat("Number of parameters: ", ncol(exprs(fcs)), "\n\n")
    cat("Parameters:\n")
    print(head(parameters(fcs)[, c("name", "desc")], 20))
  })
  
  output$markerSelectUI <- renderUI({
    fcs <- rawFCS()
    params <- parameters(fcs)
    
    # Create selection with parameter name and description
    choices <- setNames(
      colnames(exprs(fcs)),
      paste0(colnames(exprs(fcs)), " - ", params$desc)
    )
    
    # Filter out common metadata columns
    exclude <- c("Time", "Event", "CellID")
    channels <- colnames(exprs(fcs))
    channels <- channels[!channels %in% exclude]
    
    selectInput("selectedMarkers", "Select Markers/Channels", 
                choices = choices, selected = choices[1:min(5, length(choices))], multiple = TRUE)
  })
  
  # Update color selection options
  observe({
    req(input$selectedMarkers)
    # Get marker names
    markers <- input$selectedMarkers
    
    # Set choices for color dropdown
    color_choices <- c("None", markers)
    updateSelectInput(session, "colorBy", choices = color_choices)
  })
  
  # Create a reactive for processed data
  processedData <- reactiveVal(NULL)
  
  # Process data when run button is clicked
  observeEvent(input$run, {
    req(input$selectedMarkers)
    
    withProgress(message = 'Processing data...', value = 0, {
      fcs <- rawFCS()
      exprs_data <- exprs(fcs)[, input$selectedMarkers, drop = FALSE]
      
      # Sample data
      set.seed(123)  # For reproducibility
      sampled_indices <- sample(nrow(exprs_data), min(input$nEvents, nrow(exprs_data)))
      sampled_data <- exprs_data[sampled_indices, ]
      
      # Apply transformation if requested
      incProgress(0.2, detail = "Transforming data...")
      if (input$transform) {
        sampled_data <- apply(sampled_data, 2, asinhTransform, cofactor = input$cofactor)
      }
      
      # Scale data
      scaled_data <- scale(sampled_data)
      
      # Create a combined dataframe for plotting
      results <- list(
        raw_data = sampled_data,
        scaled_data = scaled_data
      )
      
      # Perform t-SNE if selected
      if ("t-SNE" %in% input$methods) {
        incProgress(0.4, detail = "Running t-SNE...")
        tsne_result <- Rtsne(scaled_data, dims = 2, perplexity = input$perplexity, verbose = FALSE)
        results$tsne <- data.frame(tsne1 = tsne_result$Y[,1], tsne2 = tsne_result$Y[,2])
      }
      
      # Perform UMAP if selected
      if ("UMAP" %in% input$methods) {
        incProgress(0.7, detail = "Running UMAP...")
        umap_result <- umap(scaled_data, n_neighbors = input$n_neighbors)
        results$umap <- data.frame(umap1 = umap_result[,1], umap2 = umap_result[,2])
      }
      
      # Create the complete plot data
      plot_data <- as.data.frame(sampled_data)
      colnames(plot_data) <- input$selectedMarkers
      
      # Add dimensionality reduction results if available
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
  
  # t-SNE Plot
  output$tsnePlot <- renderPlot({
    req(processedData(), "t-SNE" %in% input$methods)
    req("tsne1" %in% colnames(processedData()$plot_data))
    
    plot_data <- processedData()$plot_data
    
    p <- ggplot(plot_data, aes(x = tsne1, y = tsne2))
    
    # Color by selected marker if specified
    if (!is.null(input$colorBy) && input$colorBy != "None") {
      p <- p + geom_point(aes(color = .data[[input$colorBy]]), alpha = 0.7) +
        scale_color_viridis_c() +
        labs(color = input$colorBy)
    } else {
      p <- p + geom_point(alpha = 0.7)
    }
    
    p + labs(title = "t-SNE Projection", x = "t-SNE 1", y = "t-SNE 2") +
      theme_minimal()
  })
  
  # UMAP Plot
  output$umapPlot <- renderPlot({
    req(processedData(), "UMAP" %in% input$methods)
    req("umap1" %in% colnames(processedData()$plot_data))
    
    plot_data <- processedData()$plot_data
    
    p <- ggplot(plot_data, aes(x = umap1, y = umap2))
    
    # Color by selected marker if specified
    if (!is.null(input$colorBy) && input$colorBy != "None") {
      p <- p + geom_point(aes(color = .data[[input$colorBy]]), alpha = 0.7) +
        scale_color_viridis_c() +
        labs(color = input$colorBy)
    } else {
      p <- p + geom_point(alpha = 0.7)
    }
    
    p + labs(title = "UMAP Projection", x = "UMAP 1", y = "UMAP 2") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)