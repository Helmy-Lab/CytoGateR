library(shiny)
library(flowCore)
library(ggplot2)
library(Rtsne)
library(uwot)
library(data.table)

ui <- fluidPage(
  titlePanel("Flow Cytometry: t-SNE & UMAP Dimensionality Reduction"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("fcsFile", "Upload FCS File", accept = ".fcs"),
      uiOutput("markerSelectUI"),
      numericInput("nEvents", "Number of events to sample", value = 1000, min = 100, step = 100),
      checkboxGroupInput("methods", "Select Dimensionality Reduction Methods",
                         choices = c("t-SNE", "UMAP"), selected = c("t-SNE", "UMAP")),
      actionButton("run", "Run Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("t-SNE", plotOutput("tsnePlot")),
        tabPanel("UMAP", plotOutput("umapPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  rawFCS <- reactive({
    req(input$fcsFile)
    read.FCS(input$fcsFile$datapath, transformation = FALSE)
  })
  
  output$markerSelectUI <- renderUI({
    fcs <- rawFCS()
    channels <- colnames(exprs(fcs))
    
    # Filter out common metadata columns
    exclude <- c("Time", "Event", "CellID")
    channels <- channels[!channels %in% exclude]
    
    selectInput("selectedMarkers", "Select Markers/Channels", 
                choices = channels, selected = channels, multiple = TRUE)
  })
  
  dataReactive <- eventReactive(input$run, {
    req(input$selectedMarkers)
    fcs <- rawFCS()
    exprs_data <- exprs(fcs)[, input$selectedMarkers, drop = FALSE]
    
    # Sample data
    sampled_data <- exprs_data[sample(nrow(exprs_data), min(input$nEvents, nrow(exprs_data))), ]
    scale(sampled_data)
  })
  
  output$tsnePlot <- renderPlot({
    req("t-SNE" %in% input$methods)
    data <- dataReactive()
    
    tsne_result <- Rtsne(data, dims = 2, perplexity = 30, verbose = FALSE)
    df <- data.frame(tsne1 = tsne_result$Y[,1], tsne2 = tsne_result$Y[,2])
    
    ggplot(df, aes(x = tsne1, y = tsne2)) +
      geom_point(alpha = 0.6) +
      labs(title = "t-SNE Projection", x = "Dim 1", y = "Dim 2") +
      theme_minimal()
  })
  
  output$umapPlot <- renderPlot({
    req("UMAP" %in% input$methods)
    data <- dataReactive()
    
    umap_result <- umap(data)
    df <- data.frame(umap1 = umap_result[,1], umap2 = umap_result[,2])
    
    ggplot(df, aes(x = umap1, y = umap2)) +
      geom_point(alpha = 0.6) +
      labs(title = "UMAP Projection", x = "Dim 1", y = "Dim 2") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
