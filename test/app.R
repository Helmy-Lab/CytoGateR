library(shiny)
library(flowCore)
library(ggplot2)
library(Rtsne)
library(uwot)
library(data.table)
library(plotly)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(openxlsx)
library(shinythemes)
library(shinyWidgets)
library(cluster)
# libraries for clustering
library(dbscan)
library(FlowSOM)
library(Rphenograph)
library(igraph)
# install.packages("devtools")
# devtools::install_github("JinmiaoChenLab/Rphenograph")
# install.packages("FlowSOM")


options(shiny.maxRequestSize = 250*1024^2)

asinhTransform <- function(x, cofactor = 5) {
  asinh(x / cofactor)
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Flow Cytometry Analysis Tool"),
  
  # --- Raw Data Analysis Tabs ---
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Raw Data Analysis", 
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
                 
                 # New clustering controls
                 hr(),
                 h4("Clustering Options"),
                 checkboxInput("showClusteringOptions", "Show Clustering Tools", FALSE),
                 
                 conditionalPanel(
                   condition = "input.showClusteringOptions == true",
                   
                   # --- clustering methods ---
                   selectInput("clusterMethod", "Clustering Method",
                               choices = c("K-means", "DBSCAN", "FlowSOM", "Phenograph")),
                   
                   conditionalPanel(
                     condition = "input.clusterMethod == 'K-means'",
                     numericInput("numClusters", "Number of Clusters", value = 5, min = 2, max = 20)
                   ),
                   conditionalPanel(
                     condition = "input.clusterMethod == 'DBSCAN'",
                     numericInput("dbscan_eps", "DBSCAN epsilon", value = 0.5, min = 0.01, step = 0.1),
                     numericInput("dbscan_minPts", "DBSCAN minPts", value = 5, min = 1, step = 1)
                   ),
                   conditionalPanel(
                     condition = "input.clusterMethod == 'FlowSOM'",
                     numericInput("som_xdim", "SOM Grid X dimension", value = 5, min = 2, max = 20),
                     numericInput("som_ydim", "SOM Grid Y dimension", value = 5, min = 2, max = 20),
                     numericInput("som_clusters", "Number of metaclusters", value = 10, min = 2, max = 30)
                   ),
                   conditionalPanel(
                     condition = "input.clusterMethod == 'Phenograph'",
                     numericInput("pheno_k", "Phenograph k nearest neighbors", value = 30, min = 5, max = 100)
                   ),
                   
                   hr(),
                   h4("Cell Population Identification"),
                   actionButton("identifyPopulations", "Identify Cell Populations", class = "btn-success"),
                   checkboxInput("showPopulationLabels", "Show Population Labels on Plot", TRUE),
                   
                   # Add advanced configuration options for cell identification
                   checkboxInput("showAdvancedIdentOptions", "Show Advanced Identification Options", FALSE),
                   conditionalPanel(
                     condition = "input.showAdvancedIdentOptions == true",
                     sliderInput("highExpressionThreshold", "High Expression Threshold", 
                                 min = 0.1, max = 1.5, value = 0.5, step = 0.1),
                     sliderInput("lowExpressionThreshold", "Low Expression Threshold", 
                                 min = -1.5, max = -0.1, value = -0.5, step = 0.1),
                     sliderInput("minConfidenceThreshold", "Minimum Confidence Threshold (%)", 
                                 min = 10, max = 90, value = 30, step = 5)
                   ),
                   
                   checkboxInput("showClusterProfiles", "Show Cluster Profiles", TRUE),
                   actionButton("runClustering", "Run Clustering", class = "btn-info"),
                   
                   hr(),
                   h4("Heatmap Size"),
                   sliderInput("hm_width", "Heatmap Width (px)", min = 300, max = 1200, value = 800, step = 50),
                   sliderInput("hm_height", "Heatmap Height (px)", min = 300, max = 1200, value = 600, step = 50),
                   
                   # Add UI for optimization metrics in the Raw Data Analysis tab
                   uiOutput("optimizationMetricsUI")
                 ),
                 
                 # finally your main analysis button
                 actionButton("run", "Run Analysis", class = "btn-primary")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("t-SNE", withSpinner(plotlyOutput("tsnePlot", height = "600px"))),
                   tabPanel("UMAP", withSpinner(plotlyOutput("umapPlot", height = "600px"))),
                   tabPanel("Data Info", verbatimTextOutput("fcsInfo")),
                   # New tabs for cluster analysis
                   tabPanel("Cluster Analysis", 
                            conditionalPanel(
                              condition = "input.showClusteringOptions == true",
                              tabsetPanel(
                                tabPanel("Cluster Visualization", plotlyOutput("clusterPlot")),
                                tabPanel("Cluster Profiles", withSpinner(plotOutput("clusterHeatmap"))),
                                tabPanel("Cluster Statistics", DT::dataTableOutput("clusterStats")),
                                tabPanel("Identified Populations", DT::dataTableOutput("populationTable"))
                              )
                            )
                   )
                 )
               )
             )
    ),
    
    # --- Sample Comparison Tab  ---
    tabPanel("Sample Comparison",
             sidebarLayout(
               sidebarPanel(
                 fileInput("fcsFile1", "Upload Control Sample", accept = c(".fcs", ".csv", ".tsv")),
                 fileInput("fcsFile2", "Upload Treated Sample", accept = c(".fcs", ".csv", ".tsv")),
                 selectInput("comparisonMethod", "Dimensionality Reduction Method", 
                             choices = c("t-SNE", "UMAP"), selected = "t-SNE"),
                 selectInput("comparisonMarkers", "Select Markers", choices = NULL, multiple = TRUE),
                 conditionalPanel(
                   condition = "input.comparisonMethod == 't-SNE'",
                   sliderInput("comparisonPerplexity", "t-SNE perplexity", min = 5, max = 50, value = 30)
                 ),
                 conditionalPanel(
                   condition = "input.comparisonMethod == 'UMAP'",
                   sliderInput("comparisonNeighbors", "UMAP n_neighbors", min = 2, max = 100, value = 15)
                 ),
                 checkboxInput("comparisonTransform", "Apply arcsinh transformation", value = TRUE),
                 numericInput("comparisonCofactor", "Transformation cofactor", value = 5, min = 1, max = 10),
                 numericInput("comparisonEvents", "Events per sample", value = 5000, min = 100, step = 1000),
                 
                 # Add clustering controls for Sample Comparison
                 hr(),
                 h4("Clustering Options"),
                 checkboxInput("showComparisonClustering", "Show Clustering Tools", FALSE),
                 
                 conditionalPanel(
                   condition = "input.showComparisonClustering == true",
                   
                   # clustering methods
                   selectInput("comparisonClusterMethod", "Clustering Method",
                               choices = c("K-means", "DBSCAN", "FlowSOM", "Phenograph")),
                   
                   conditionalPanel(
                     condition = "input.comparisonClusterMethod == 'K-means'",
                     numericInput("comparisonNumClusters", "Number of Clusters", value = 5, min = 2, max = 20)
                   ),
                   conditionalPanel(
                     condition = "input.comparisonClusterMethod == 'DBSCAN'",
                     numericInput("comparison_dbscan_eps", "DBSCAN epsilon", value = 0.5, min = 0.01, step = 0.1),
                     numericInput("comparison_dbscan_minPts", "DBSCAN minPts", value = 5, min = 1, step = 1)
                   ),
                   conditionalPanel(
                     condition = "input.comparisonClusterMethod == 'FlowSOM'",
                     numericInput("comparison_som_xdim", "SOM Grid X dimension", value = 5, min = 2, max = 20),
                     numericInput("comparison_som_ydim", "SOM Grid Y dimension", value = 5, min = 2, max = 20),
                     numericInput("comparison_som_clusters", "Number of metaclusters", value = 10, min = 2, max = 30)
                   ),
                   conditionalPanel(
                     condition = "input.comparisonClusterMethod == 'Phenograph'",
                     numericInput("comparison_pheno_k", "Phenograph k nearest neighbors", value = 30, min = 5, max = 100)
                   ),
                   actionButton("runComparisonClustering", "Run Clustering", class = "btn-info"),
                   
                   hr(),
                   h4("Cell Population Identification"),
                   actionButton("identifyComparisonPopulations", "Identify Cell Populations", class = "btn-success"),
                   checkboxInput("showComparisonPopulationLabels", "Show Population Labels on Plot", TRUE),
                   
                   # Add advanced configuration options for cell identification
                   checkboxInput("showAdvancedComparisonIdentOptions", "Show Advanced Identification Options", FALSE),
                   conditionalPanel(
                     condition = "input.showAdvancedComparisonIdentOptions == true",
                     sliderInput("comparisonHighExpressionThreshold", "High Expression Threshold", 
                                 min = 0.1, max = 1.5, value = 0.5, step = 0.1),
                     sliderInput("comparisonLowExpressionThreshold", "Low Expression Threshold", 
                                 min = -1.5, max = -0.1, value = -0.5, step = 0.1),
                     sliderInput("comparisonMinConfidenceThreshold", "Minimum Confidence Threshold (%)", 
                                 min = 10, max = 90, value = 30, step = 5)
                   )
                 ),
                 
                 actionButton("runComparison", "Run Comparison", class = "btn-primary")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Side-by-Side Comparison",
                            fluidRow(
                              column(6, h4("Control Sample", align = "center")),
                              column(6, h4("Treated Sample", align = "center"))
                            ),
                            fluidRow(
                              column(6, withSpinner(plotlyOutput("comparisonPlot1", height = "500px"))),
                              column(6, withSpinner(plotlyOutput("comparisonPlot2", height = "500px")))
                            )
                   ),
                   tabPanel("Cluster Analysis", 
                            conditionalPanel(
                              condition = "input.showComparisonClustering == true",
                              tabsetPanel(
                                tabPanel("Cluster Visualization", withSpinner(plotlyOutput("comparisonClusterPlot", height = "600px"))),
                                tabPanel("Cluster Profiles", withSpinner(plotOutput("comparisonClusterHeatmap", height = "600px"))),
                                tabPanel("Cluster Statistics", withSpinner(DT::dataTableOutput("comparisonClusterStats"))),
                                tabPanel("Identified Populations", withSpinner(DT::dataTableOutput("comparisonPopulationTable")))
                              )
                            )
                   )
                 )
               )
             )
    ),
    
    # --- Cleaned Data Analysis Tabs ---
    tabPanel("Cleaned Data Analysis",
             sidebarLayout(
               sidebarPanel(
                 fileInput("cleanedFile", "Upload CSV/TSV/Excel File", accept = c(".csv", ".tsv", ".xlsx")),
                 uiOutput("analysis_type_ui"),
                 uiOutput("marker_ui"),
                 uiOutput("treatment_ui"),
                 selectInput("dimred_method", "Dimensionality Reduction Method", choices = c("t-SNE", "UMAP")),
                 numericInput("perplexity_cleaned", "t-SNE: Perplexity", value = 5, min = 2, max = 50),
                 numericInput("neighbors", "UMAP: n_neighbors", value = 5, min = 2, max = 100),
                 numericInput("min_dist", "UMAP: min_dist", value = 0.1, min = 0, max = 1, step = 0.05),
                 numericInput("n_clusters", "Number of Clusters (k-means)", value = 3, min = 1),
                 sliderInput("plot_width", "Plot Width (px)", min = 300, max = 1200, value = 600, step = 50),
                 sliderInput("plot_height", "Plot Height (px)", min = 300, max = 1200, value = 600, step = 50),
                 actionButton("run_cleaned", "Run Analysis", class = "btn-primary")
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
  )
)

server <- function(input, output, session) {
  
  # --- RAW DATA ANALYSIS (from first app) ---
  
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
    req(input$fcsFile)
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
    req(input$fcsFile)
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
  
  # Add optimization metrics UI output
  output$optimizationMetricsUI <- renderUI({
    req(processedData())
    
    # Only show after analysis has been run
    if (is.null(processedData())) {
      return(NULL)
    }
    
    # Calculate optimization metrics for dimensionality reduction methods
    results <- processedData()
    metrics <- list()
    
    # Calculate metrics only for methods that were run
    if (!is.null(results$tsne)) {
      # Calculate t-SNE metrics if it was run
      tsne_data <- results$tsne
      
      # Calculate KL divergence (approximation)
      # This is a simple approximation - actual KL would require access to t-SNE internals
      tsne_kl <- tryCatch({
        # A rough proxy for quality - normalize and sum squared distances
        original_dist <- dist(scale(results$scaled_data))
        embedding_dist <- dist(as.matrix(tsne_data))
        
        # Normalize distances
        original_dist <- original_dist / max(original_dist)
        embedding_dist <- embedding_dist / max(embedding_dist)
        
        # Calculate a simple divergence measure
        mean((as.matrix(original_dist) - as.matrix(embedding_dist))^2)
      }, error = function(e) NA)
      
      # Store t-SNE metrics
      metrics$tsne <- list(
        kl_divergence = round(tsne_kl, 4),
        perplexity = input$perplexity
      )
    }
    
    if (!is.null(results$umap)) {
      # Calculate UMAP metrics if it was run
      umap_data <- results$umap
      
      # Calculate a simple metric for UMAP (trustworthiness approximation)
      umap_trust <- tryCatch({
        # A rough proxy for quality - normalize and sum squared distances
        original_dist <- dist(scale(results$scaled_data))
        embedding_dist <- dist(as.matrix(umap_data))
        
        # Normalize distances
        original_dist <- original_dist / max(original_dist)
        embedding_dist <- embedding_dist / max(embedding_dist)
        
        # Calculate correlation between distance matrices
        cor(as.vector(as.matrix(original_dist)), as.vector(as.matrix(embedding_dist)))
      }, error = function(e) NA)
      
      # Store UMAP metrics
      metrics$umap <- list(
        trustworthiness = round(umap_trust, 4),
        n_neighbors = input$n_neighbors
      )
    }
    
    # Create UI elements to display metrics
    metrics_ui <- tagList(
      hr(),
      h4("Optimization Metrics"),
      p("These metrics help evaluate the quality of dimensionality reduction results.")
    )
    
    # Add t-SNE metrics if available
    if (!is.null(metrics$tsne)) {
      metrics_ui <- tagAppendChildren(
        metrics_ui,
        div(
          h5("t-SNE Metrics:"),
          p(paste("Perplexity:", metrics$tsne$perplexity)),
          p(paste("Approximate KL Divergence:", metrics$tsne$kl_divergence, 
                  "(lower is better)"))
        )
      )
    }
    
    # Add UMAP metrics if available
    if (!is.null(metrics$umap)) {
      metrics_ui <- tagAppendChildren(
        metrics_ui,
        div(
          h5("UMAP Metrics:"),
          p(paste("n_neighbors:", metrics$umap$n_neighbors)),
          p(paste("Distance Correlation:", metrics$umap$trustworthiness, 
                  "(higher is better)"))
        )
      )
    }
    
    # Add recommendation if both methods are available
    if (!is.null(metrics$tsne) && !is.null(metrics$umap)) {
      # Determine which method seems better (simple heuristic)
      tsne_score <- 1 - min(metrics$tsne$kl_divergence, 1)  # Convert to 0-1 scale where higher is better
      umap_score <- max(min(metrics$umap$trustworthiness, 1), 0)  # Ensure in 0-1 range
      
      recommended <- if (tsne_score > umap_score) "t-SNE" else "UMAP"
      
      metrics_ui <- tagAppendChildren(
        metrics_ui,
        div(
          h5("Recommendation:"),
          p(paste("Based on calculated metrics, ", recommended, " may provide better results for this dataset."))
        )
      )
    }
    
    return(metrics_ui)
  })
  
  observe({
    req(input$selectedMarkers)
    markers <- input$selectedMarkers
    updateSelectInput(session, "colorBy", choices = c("None", "Cluster", markers))
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
      
      # Reset clustering and gates when new data is loaded
      clustering_results(NULL)
    })
  })
  
  # --- CLUSTERING FUNCTIONALITY ---
  
  clustering_results <- reactiveVal(NULL)
  
  observeEvent(input$runClustering, {
    req(processedData(), input$selectedMarkers)
    
    withProgress(message = 'Clustering cells...', value = 0, {
      plot_data <- processedData()$plot_data
      # Extract just the marker columns for clustering
      marker_data <- plot_data[, input$selectedMarkers, drop = FALSE]
      
      # Run the selected clustering algorithm
      if (input$clusterMethod == "K-means") {
        set.seed(123)  # For reproducibility
        incProgress(0.3, detail = "Running K-means clustering...")
        km <- kmeans(marker_data, centers = input$numClusters, nstart = 10)
        cluster_ids <- km$cluster
        
        # Store cluster centers for intensity profiles
        centers <- km$centers
        colnames(centers) <- input$selectedMarkers
        
        # Create results object
        results <- list(
          cluster_ids = cluster_ids,
          centers = centers,
          method = "K-means"
        )
        
        clustering_results(results)
      }
      else if (input$clusterMethod == "DBSCAN") {
        incProgress(0.3, detail = "Running DBSCAN clustering...")
        # Scale data for DBSCAN
        scaled_markers <- scale(marker_data)
        
        # Run DBSCAN
        dbscan_result <- dbscan::dbscan(scaled_markers, eps = input$dbscan_eps, minPts = input$dbscan_minPts)
        cluster_ids <- dbscan_result$cluster
        
        # Calculate cluster centers (mean of each cluster)
        unique_clusters <- sort(unique(cluster_ids))
        centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(marker_data))
        
        for (i in seq_along(unique_clusters)) {
          cluster_idx <- which(cluster_ids == unique_clusters[i])
          if (length(cluster_idx) > 0) {
            centers[i,] <- colMeans(marker_data[cluster_idx, , drop = FALSE])
          }
        }
        
        colnames(centers) <- input$selectedMarkers
        rownames(centers) <- paste("Cluster", unique_clusters)
        
        # Create results object
        results <- list(
          cluster_ids = cluster_ids,
          centers = centers,
          method = "DBSCAN"
        )
        
        clustering_results(results)
      }
      else if (input$clusterMethod == "FlowSOM") {
        incProgress(0.3, detail = "Running FlowSOM clustering...")
        
        tryCatch({
          # Create FlowSOM object
          # First, create a flowFrame from the matrix with proper marker names
          marker_matrix <- as.matrix(marker_data)
          colnames(marker_matrix) <- input$selectedMarkers
          fcs_data <- flowCore::flowFrame(marker_matrix)
          
          # Set proper column names in the flowFrame parameters
          params <- flowCore::parameters(fcs_data)
          params$name <- input$selectedMarkers
          params$desc <- input$selectedMarkers
          flowCore::parameters(fcs_data) <- params
          
          # Use ReadInput to properly prepare the data for FlowSOM
          fsom_input <- FlowSOM::ReadInput(fcs_data, transform = FALSE, scale = FALSE)
          
          # Now build the SOM
          fsom <- FlowSOM::BuildSOM(fsom_input, 
                                    xdim = input$som_xdim, 
                                    ydim = input$som_ydim)
          
          # Get metaclusters
          metacl <- FlowSOM::MetaClustering(fsom$map$codes, 
                                            method = "metaClustering_consensus",
                                            max = input$som_clusters)
          
          # Get cluster IDs for each cell
          # First map to SOM clusters
          cell_som_clusters <- fsom$map$mapping[,1]
          # Then map to metaclusters
          cell_metaclusters <- metacl[cell_som_clusters]
          
          # Calculate metacluster centers (mean of each cluster)
          unique_clusters <- sort(unique(cell_metaclusters))
          centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(marker_data))
          
          for (i in seq_along(unique_clusters)) {
            cluster_idx <- which(cell_metaclusters == unique_clusters[i])
            if (length(cluster_idx) > 0) {
              centers[i,] <- colMeans(marker_data[cluster_idx, , drop = FALSE])
            }
          }
          
          colnames(centers) <- input$selectedMarkers
          rownames(centers) <- paste("Cluster", unique_clusters)
          
          # Create results object
          results <- list(
            cluster_ids = cell_metaclusters,
            centers = centers,
            som_object = fsom,
            som_clusters = cell_som_clusters,
            metaclusters = metacl,
            method = "FlowSOM"
          )
          
          clustering_results(results)
        }, error = function(e) {
          # Show error message to user
          showNotification(
            paste("FlowSOM error:", e$message, 
                  "Try adjusting parameters or selecting different markers."),
            type = "error",
            duration = 10
          )
        })
      }
      else if (input$clusterMethod == "Phenograph") {
        incProgress(0.3, detail = "Running Phenograph clustering...")
        
        tryCatch({
          # Run Phenograph
          pheno_result <- Rphenograph(as.matrix(marker_data), k = input$pheno_k)
          
          # Get cluster IDs
          cluster_ids <- as.numeric(membership(pheno_result[[2]]))
          
          # Calculate cluster centers (mean of each cluster)
          unique_clusters <- sort(unique(cluster_ids))
          centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(marker_data))
          
          for (i in seq_along(unique_clusters)) {
            cluster_idx <- which(cluster_ids == unique_clusters[i])
            if (length(cluster_idx) > 0) {
              centers[i,] <- colMeans(marker_data[cluster_idx, , drop = FALSE])
            }
          }
          
          colnames(centers) <- input$selectedMarkers
          rownames(centers) <- paste("Cluster", unique_clusters)
          
          # Create results object
          results <- list(
            cluster_ids = cluster_ids,
            centers = centers,
            pheno_object = pheno_result,
            method = "Phenograph"
          )
          
          clustering_results(results)
        }, error = function(e) {
          # Show error message to user
          showNotification(
            paste("Phenograph error:", e$message, 
                  "Try adjusting k parameter or selecting different markers."),
            type = "error",
            duration = 10
          )
        })
      }
      # Add other clustering methods here
    })
  })
  
  # Update plot outputs to show clusters
  observe({
    req(clustering_results())
    updateSelectInput(session, "colorBy", choices = c("None", "Cluster", input$selectedMarkers), selected = "Cluster")
  })
  
  output$tsnePlot <- renderPlotly({
    req(processedData(), "t-SNE" %in% input$methods)
    plot_data <- processedData()$plot_data
    req("tsne1" %in% colnames(plot_data))
    
    # Create a copy of plot_data to avoid modifying the reactive value
    plot_data_copy <- plot_data
    
    # If clustering results exist, add them to the plot data
    if (!is.null(clustering_results()) && input$colorBy == "Cluster") {
      plot_data_copy$Cluster <- as.factor(clustering_results()$cluster_ids)
      
      p <- ggplot(plot_data_copy, aes(x = tsne1, y = tsne2, color = Cluster, 
                                      text = paste("Cluster:", Cluster))) +
        geom_point(alpha = 0.7) +
        scale_color_viridis_d() +
        labs(color = "Cluster")
    } else if (!is.null(input$colorBy) && input$colorBy != "None" && input$colorBy != "Cluster") {
      p <- ggplot(plot_data_copy, aes(x = tsne1, y = tsne2)) +
        geom_point(aes(color = .data[[input$colorBy]], 
                       text = paste("Value:", .data[[input$colorBy]])), alpha = 0.7) +
        scale_color_viridis_c() +
        labs(color = input$colorBy)
    } else {
      p <- ggplot(plot_data_copy, aes(x = tsne1, y = tsne2)) +
        geom_point(aes(text = paste("Index:", rownames(plot_data_copy))), alpha = 0.7)
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
    
    # Create a copy of plot_data to avoid modifying the reactive value
    plot_data_copy <- plot_data
    
    # If clustering results exist, add them to the plot data
    if (!is.null(clustering_results()) && input$colorBy == "Cluster") {
      plot_data_copy$Cluster <- as.factor(clustering_results()$cluster_ids)
      
      p <- ggplot(plot_data_copy, aes(x = umap1, y = umap2, color = Cluster, 
                                      text = paste("Cluster:", Cluster))) +
        geom_point(alpha = 0.7) +
        scale_color_viridis_d() +
        labs(color = "Cluster")
    } else if (!is.null(input$colorBy) && input$colorBy != "None" && input$colorBy != "Cluster") {
      p <- ggplot(plot_data_copy, aes(x = umap1, y = umap2)) +
        geom_point(aes(color = .data[[input$colorBy]], 
                       text = paste("Value:", .data[[input$colorBy]])), alpha = 0.7) +
        scale_color_viridis_c() +
        labs(color = input$colorBy)
    } else {
      p <- ggplot(plot_data_copy, aes(x = umap1, y = umap2)) +
        geom_point(aes(text = paste("Index:", rownames(plot_data_copy))), alpha = 0.7)
    }
    
    
    p <- p + labs(title = "UMAP Projection", x = "UMAP 1", y = "UMAP 2") +
      theme_minimal(base_size = 16) +
      theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))
    
    ggplotly(p, tooltip = "text") %>% layout(height = input$plotHeight, width = input$plotWidth)
  })
  
  # Cluster visualization in dedicated tab
  output$clusterPlot <- renderPlotly({
    req(processedData(), clustering_results())
    
    # Check what dimension reduction method is available
    plot_data <- processedData()$plot_data
    
    # Choose appropriate dimensions
    if ("tsne1" %in% colnames(plot_data)) {
      dim1 <- "tsne1"
      dim2 <- "tsne2"
      dim_labels <- c("t-SNE 1", "t-SNE 2")
    } else if ("umap1" %in% colnames(plot_data)) {
      dim1 <- "umap1"
      dim2 <- "umap2"
      dim_labels <- c("UMAP 1", "UMAP 2")
    } else {
      # Fallback to first two markers if no dimension reduction available
      dim1 <- input$selectedMarkers[1]
      dim2 <- input$selectedMarkers[2]
      dim_labels <- input$selectedMarkers[1:2]
    }
    
    # Add cluster information
    plot_data$Cluster <- as.factor(clustering_results()$cluster_ids)
    
    # Create plot
    p <- plot_ly(plot_data, 
                 x = ~.data[[dim1]], 
                 y = ~.data[[dim2]],
                 color = ~Cluster, 
                 type = "scatter", 
                 mode = "markers",
                 marker = list(size = 8),
                 text = ~paste("Cluster:", Cluster, 
                               "<br>", dim_labels[1], ":", .data[[dim1]], 
                               "<br>", dim_labels[2], ":", .data[[dim2]])) %>%
      layout(title = paste("Clusters from", clustering_results()$method),
             xaxis = list(title = dim_labels[1]),
             yaxis = list(title = dim_labels[2]),
             height = input$plotHeight,
             width = input$plotWidth)
    
    p
  })
  
  # Cluster heatmap
  output$clusterHeatmap <- renderPlot(
    {
      library(reshape2)
      centers <- clustering_results()$centers
      df <- as.data.frame(centers) %>%
        tibble::rownames_to_column("Cluster") %>%
        melt(id.vars = "Cluster", variable.name = "Marker", value.name = "Expression")
      
      ggplot(df, aes(x = Marker, y = Cluster, fill = Expression)) +
        geom_tile() +
        scale_fill_gradient2(low = "navy", mid = "white", high = "firebrick3") +
        labs(
          title = paste("Cluster Intensity Profiles from", clustering_results()$method),
          x = "Markers", y = "Clusters"
        ) +
        theme_minimal(base_size = 16) +
        theme(
          axis.text.x   = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y   = element_text(size = 16),
          plot.title    = element_text(size = 18, face = "bold"),
          axis.title    = element_text(size = 16)
        )
    },
    width  = function() input$hm_width,
    height = function() input$hm_height
  )
  
  # Cluster statistics
  output$clusterStats <- DT::renderDataTable({
    req(clustering_results(), processedData())
    
    plot_data <- processedData()$plot_data
    plot_data$Cluster <- as.factor(clustering_results()$cluster_ids)
    
    # Calculate basic statistics for each cluster
    cluster_stats <- plot_data %>%
      group_by(Cluster) %>%
      summarize(
        Count = n(),
        Percentage = n() / nrow(plot_data) * 100,
        across(all_of(input$selectedMarkers), 
               list(Mean = ~mean(., na.rm = TRUE), 
                    Median = ~median(., na.rm = TRUE),
                    SD = ~sd(., na.rm = TRUE)))
      )
    
    DT::datatable(cluster_stats, 
                  options = list(scrollX = TRUE, pageLength = 5),
                  caption = paste("Cluster statistics from", clustering_results()$method))
  })
  
  # --- CELL POPULATION IDENTIFICATION ---
  
  identified_populations <- reactiveVal(NULL)
  
  # Add at the start of your identify_cell_populations function
  # Manual mapping for the specific markers in your dataset
  manual_mapping <- list(
    # "FL5-A - CD3 FITC FITC-A" = "CD3",
    # "FL13-A - CD4 APCCy7 APC-A750-A" = "CD4",
    # "FL11-A - MHC II AF647 APC-A" = "MHCII",
    # "FL12-A - Foxp3 A700 APC-A700-A" = "FOXP3",
    # "FL1-A - CXCR5 BV421 PB450-A" = "CXCR5",
    # "FL2-A - CX3 BV510 KO525-A" = "CX3",
    # "FL4-A - IgG2 BV711 V5-711-A" = "IGG2",
    # "FL7-A - BCI6 PE PE-A" = "BCI6",
    # "FL8-A - IgG AF594 ECD-A" = "IGG",
    # "FL10-A - Ki67 PECy7 PC7-A" = "KI67"
    "FL5-A" = "CD3",
    "FL13-A" = "CD4",
    "FL11-A" = "MHCII",
    "FL12-A" = "FOXP3",
    "FL1-A" = "CXCR5",
    "FL2-A" = "CX3",
    "FL4-A" = "IGG2",
    "FL7-A" = "BCI6",
    "FL8-A" = "IGG",
    "FL10-A" = "KI67"
  )
  
  # Then use this mapping directly in your template matching
  # Define common cell population reference profiles based on marker expression patterns
  cell_population_templates <- list(
    "CD4+ T cells" = list(
      high = c("CD3", "CD4"),
      low = c("CD8", "CD19")
    ),
    "CD8+ T cells" = list(
      high = c("CD3", "CD8"),
      low = c("CD4", "CD19")
    ),
    "B cells" = list(
      high = c("CD19", "CD20"),
      low = c("CD3", "CD56")
    ),
    "NK cells" = list(
      high = c("CD56"),
      low = c("CD3", "CD19")
    ),
    "Regulatory T cells" = list(
      high = c("CD3", "CD4", "FOXP3"),
      low = c("CD8")
    ),
    "Helper T cells" = list(
      high = c("CD3", "CD4"),
      low = c("CD8", "FOXP3")
    ),
    "Monocytes" = list(
      high = c("CD14"),
      low = c("CD3", "CD19")
    ),
    "Dendritic cells" = list(
      high = c("CD11c", "HLA-DR"),
      low = c("CD3", "CD19", "CD14")
    )
  )
  
  # Function to identify cell populations based on marker expression patterns
  identify_cell_populations <- function(cluster_centers, marker_names, 
                                        high_threshold = 0.5, 
                                        low_threshold = -0.5,
                                        min_confidence = 0.3) {
    # Create a proper initial data frame first
    results <- data.frame(
      Cluster = 1:nrow(cluster_centers),
      Population = rep("Unknown", nrow(cluster_centers)),
      Confidence = rep(0, nrow(cluster_centers)),
      MatchDetails = rep("", nrow(cluster_centers)),
      stringsAsFactors = FALSE
    )
    
    # Define more comprehensive cell population templates
    cell_population_templates <- list(
      "CD4+ T cells" = list(
        high = c("CD3", "CD4"),
        medium = c(),
        low = c("CD8", "CD19", "CD56")
      ),
      "CD8+ T cells" = list(
        high = c("CD3", "CD8"),
        medium = c(),
        low = c("CD4", "CD19", "CD56")
      ),
      "B cells" = list(
        high = c("CD19", "CD20"),
        medium = c("MHCII"),
        low = c("CD3", "CD56")
      ),
      "NK cells" = list(
        high = c("CD56"),
        medium = c("CD16"),
        low = c("CD3", "CD19")
      ),
      "Regulatory T cells" = list(
        high = c("CD3", "CD4", "FOXP3"),
        medium = c("CD25"),
        low = c("CD8", "CD19")
      ),
      "Helper T cells" = list(
        high = c("CD3", "CD4"),
        medium = c("CD40L"),
        low = c("CD8", "FOXP3")
      ),
      "Monocytes" = list(
        high = c("CD14"),
        medium = c("CD11B", "CD33"),
        low = c("CD3", "CD19", "CD56")
      ),
      "Dendritic cells" = list(
        high = c("CD11C", "MHCII", "HLA-DR"),
        medium = c("CD83", "CD86"),
        low = c("CD3", "CD19", "CD14", "CD56")
      ),
      "Follicular Helper T cells" = list(
        high = c("CD3", "CD4", "CXCR5", "BCI6"),
        medium = c("PD1", "ICOS"),
        low = c("CD8", "FOXP3")
      ),
      "Th1 cells" = list(
        high = c("CD3", "CD4", "TBET"),
        medium = c("IFNG"),
        low = c("CD8", "GATA3", "FOXP3")
      ),
      "Th2 cells" = list(
        high = c("CD3", "CD4", "GATA3"),
        medium = c("IL4"),
        low = c("CD8", "TBET", "FOXP3")
      ),
      "Th17 cells" = list(
        high = c("CD3", "CD4", "RORC"),
        medium = c("IL17A"),
        low = c("CD8", "FOXP3")
      )
    )
    
    # More robust normalization and mapping of marker names
    # Function to clean and standardize marker names
    clean_marker_name <- function(name) {
      # Convert to uppercase
      name <- toupper(name)
      
      # Remove common prefixes/suffixes and special characters
      name <- gsub("FL[0-9]+-[A-Z]+ *- *", "", name)
      name <- gsub(" +[A-Z0-9]+-[A-Z0-9]+-[A-Z0-9]+$", "", name) # Remove suffixes like APC-A750-A
      name <- gsub(" +[A-Z0-9]+-[A-Z0-9]+$", "", name)  # Remove suffixes like PE-A
      name <- gsub(" +[A-Z]+$", "", name)  # Remove single suffix like A
      name <- gsub("[-_\\s]+", "", name)   # Remove spaces, hyphens, underscores
      
      # Common marker name variations
      name <- gsub("^DR$", "HLADR", name)
      name <- gsub("^MHCII$", "HLADR", name)
      name <- gsub("^IFN[GY]$", "INTERFERON", name)
      name <- gsub("^IL4$", "INTERLEUKIN4", name)
      
      return(name)
    }
    
    # Apply standardization to marker names
    normalized_markers <- sapply(marker_names, clean_marker_name)
    
    # Add manual mapping for specific markers if needed
    manual_map <- list(
      "FL5-A" = "CD3",
      "FL13-A" = "CD4",
      "FL11-A" = "MHCII",
      "FL12-A" = "FOXP3",
      "FL1-A" = "CXCR5",
      "FL2-A" = "CX3",
      "FL4-A" = "IGG2",
      "FL7-A" = "BCI6",
      "FL8-A" = "IGG",
      "FL10-A" = "KI67"
    )
    
    for (i in 1:length(marker_names)) {
      if (marker_names[i] %in% names(manual_map)) {
        normalized_markers[i] <- manual_map[[marker_names[i]]]
      }
    }
    
    # Calculate z-scores for all markers across clusters
    z_scores <- apply(cluster_centers, 2, function(x) {
      if (sd(x, na.rm=TRUE) == 0) return(rep(0, length(x)))
      return((x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE))
    })
    
    # For each cluster, compare expression profile to templates
    for (i in 1:nrow(cluster_centers)) {
      best_match <- "Unknown"
      best_score <- 0
      best_details <- ""
      
      # Compare with each reference profile
      for (pop_name in names(cell_population_templates)) {
        template <- cell_population_templates[[pop_name]]
        score <- 0
        total_markers <- 0
        matching_details <- c()
        
        # Weight factors for different marker categories
        high_weight <- 1.0
        medium_weight <- 0.7
        low_weight <- 0.5
        
        # Track matched markers for logging
        matched_high <- c()
        matched_medium <- c()
        matched_low <- c()
        
        # Check for high-expression markers
        for (marker in template$high) {
          clean_marker <- clean_marker_name(marker)
          matching_cols <- grep(paste0("^", clean_marker, "$"), normalized_markers, ignore.case = TRUE)
          
          if (length(matching_cols) > 0) {
            total_markers <- total_markers + high_weight
            col_idx <- matching_cols[1]  # Take first match if multiple
            marker_z <- z_scores[i, col_idx]
            
            if (marker_z > high_threshold) {
              score <- score + high_weight
              matched_high <- c(matched_high, marker_names[col_idx])
              matching_details <- c(matching_details, paste0(marker_names[col_idx], "(+)"))
            }
          }
        }
        
        # Check for medium-expression markers
        for (marker in template$medium) {
          clean_marker <- clean_marker_name(marker)
          matching_cols <- grep(paste0("^", clean_marker, "$"), normalized_markers, ignore.case = TRUE)
          
          if (length(matching_cols) > 0) {
            total_markers <- total_markers + medium_weight
            col_idx <- matching_cols[1]
            marker_z <- z_scores[i, col_idx]
            
            if (marker_z > -0.2 && marker_z < 0.7) {  # Medium expression range
              score <- score + medium_weight
              matched_medium <- c(matched_medium, marker_names[col_idx])
              matching_details <- c(matching_details, paste0(marker_names[col_idx], "(~)"))
            }
          }
        }
        
        # Check for low-expression markers
        for (marker in template$low) {
          clean_marker <- clean_marker_name(marker)
          matching_cols <- grep(paste0("^", clean_marker, "$"), normalized_markers, ignore.case = TRUE)
          
          if (length(matching_cols) > 0) {
            total_markers <- total_markers + low_weight
            col_idx <- matching_cols[1]
            marker_z <- z_scores[i, col_idx]
            
            if (marker_z < low_threshold) {
              score <- score + low_weight
              matched_low <- c(matched_low, marker_names[col_idx])
              matching_details <- c(matching_details, paste0(marker_names[col_idx], "(-)"))
            }
          }
        }
        
        # Calculate weighted confidence score
        if (total_markers > 0) {
          confidence <- score / total_markers
          
          # Log match details for debugging
          log_text <- paste0("Cluster ", i, " vs ", pop_name, 
                             ": matched ", length(matched_high), "/", length(template$high), " high markers, ",
                             length(matched_medium), "/", length(template$medium), " medium markers, ",
                             length(matched_low), "/", length(template$low), " low markers. ",
                             "Confidence: ", round(confidence * 100, 1), "%")
          
          # If this population is a better match than previous ones
          if (confidence > best_score && confidence > min_confidence) {
            best_score <- confidence
            best_match <- pop_name
            best_details <- paste(matching_details, collapse = ", ")
          }
        }
      }
      
      # Assign the best matching population
      results$Population[i] <- best_match
      results$Confidence[i] <- best_score * 100  # Convert to percentage
      results$MatchDetails[i] <- best_details
    }
    
    return(results)
  }
  
  # Handle the population identification button click
  observeEvent(input$identifyPopulations, {
    req(clustering_results())
    
    withProgress(message = 'Identifying cell populations...', value = 0, {
      # Get cluster centers
      centers <- clustering_results()$centers
      marker_names <- colnames(centers)
      
      # Run identification algorithm with user-configured thresholds
      populations <- identify_cell_populations(
        centers, 
        marker_names,
        high_threshold = input$highExpressionThreshold,
        low_threshold = input$lowExpressionThreshold,
        min_confidence = input$minConfidenceThreshold/100  # Convert from percentage
      )
      
      # Store results
      identified_populations(populations)
      
      # Show notification
      showNotification("Cell populations identified based on marker expression patterns", type = "message")
    })
  })
  
  # Population results table
  output$populationTable <- DT::renderDataTable({
    req(identified_populations())
    
    population_info <- identified_populations()
    
    # Add cluster sizes if clustering results exist
    if (!is.null(clustering_results())) {
      cluster_counts <- table(clustering_results()$cluster_ids)
      total_cells <- sum(cluster_counts)
      
      result_table <- data.frame(
        Cluster = population_info$Cluster,
        Population = population_info$Population,
        Count = as.numeric(cluster_counts[population_info$Cluster]),
        Percentage = round(as.numeric(cluster_counts[population_info$Cluster]) / total_cells * 100, 2),
        Confidence = round(population_info$Confidence, 1),
        MatchDetails = population_info$MatchDetails
      )
      
      # Sort by population name then by cluster number
      result_table <- result_table[order(result_table$Population, result_table$Cluster), ]
      
      DT::datatable(
        result_table,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        caption = "Identified Cell Populations"
      )
    }
  })
  
  # Update cluster visualization to include population labels
  output$clusterPlot <- renderPlotly({
    req(processedData(), clustering_results())
    
    # Check what dimension reduction method is available
    plot_data <- processedData()$plot_data
    
    # Choose appropriate dimensions
    if ("tsne1" %in% colnames(plot_data)) {
      dim1 <- "tsne1"
      dim2 <- "tsne2"
      dim_labels <- c("t-SNE 1", "t-SNE 2")
    } else if ("umap1" %in% colnames(plot_data)) {
      dim1 <- "umap1"
      dim2 <- "umap2"
      dim_labels <- c("UMAP 1", "UMAP 2")
    } else {
      # Fallback to first two markers if no dimension reduction available
      dim1 <- input$selectedMarkers[1]
      dim2 <- input$selectedMarkers[2]
      dim_labels <- input$selectedMarkers[1:2]
    }
    
    # Add cluster information
    plot_data$Cluster <- as.factor(clustering_results()$cluster_ids)
    
    # Add population labels if available
    if (!is.null(identified_populations()) && input$showPopulationLabels) {
      pop_data <- identified_populations()
      populations <- pop_data$Population[plot_data$Cluster]
      plot_data$Population <- populations
      
      hover_text <- paste(
        "Cluster:", plot_data$Cluster,
        "<br>Population:", plot_data$Population,
        "<br>", dim_labels[1], ":", plot_data[[dim1]],
        "<br>", dim_labels[2], ":", plot_data[[dim2]]
      )
    } else {
      hover_text <- paste(
        "Cluster:", plot_data$Cluster,
        "<br>", dim_labels[1], ":", plot_data[[dim1]],
        "<br>", dim_labels[2], ":", plot_data[[dim2]]
      )
    }
    
    # Create plot
    p <- plot_ly(plot_data, 
                 x = ~.data[[dim1]], 
                 y = ~.data[[dim2]],
                 color = ~Cluster, 
                 type = "scatter", 
                 mode = "markers",
                 marker = list(size = 8, opacity = 0.95),
                 text = hover_text) %>%
      layout(title = paste("Clusters from", clustering_results()$method),
             xaxis = list(title = dim_labels[1]),
             yaxis = list(title = dim_labels[2]),
             height = input$plotHeight,
             width = input$plotWidth)
    
    # Add cluster labels with population names if available
    if (!is.null(identified_populations()) && input$showPopulationLabels) {
      # Calculate cluster centers for label positioning
      cluster_centers <- plot_data %>%
        group_by(Cluster, Population) %>%
        summarize(
          x = mean(.data[[dim1]], na.rm = TRUE),
          y = mean(.data[[dim2]], na.rm = TRUE),
          .groups = 'drop'
        )
      
      # Add annotations to plot
      for (i in 1:nrow(cluster_centers)) {
        p <- p %>% add_annotations(
          x = cluster_centers$x[i],
          y = cluster_centers$y[i],
          text = paste0("Cluster ", cluster_centers$Cluster[i], ": ", cluster_centers$Population[i]),
          showarrow = TRUE,
          arrowhead = 0.5,
          arrowsize = 0.5,
          arrowwidth = 1,
          ax = 20,
          ay = -20,
          bgcolor = "rgba(255, 255, 255, 0.8)",
          bordercolor = "rgba(0, 0, 0, 0.5)",
          borderwidth = 1,
          font = list(size = 12)
        )
      }
    }
    
    return(p)
  })
  
  # --- CLEANED DATA ANALYSIS (from second app) ---
  
  cleaned_data <- reactive({
    req(input$cleanedFile)
    ext <- tools::file_ext(input$cleanedFile$name)
    switch(ext,
           csv = read_csv(input$cleanedFile$datapath),
           tsv = read_tsv(input$cleanedFile$datapath),
           xlsx = read.xlsx(input$cleanedFile$datapath),
           validate("Unsupported file format"))
  })
  
  # --- SAMPLE COMPARISON FUNCTIONALITY ---
  
  # Create reactive expressions for the two files
  fcsFile1 <- reactive({
    req(input$fcsFile1)
    ext <- tools::file_ext(input$fcsFile1$name)
    switch(ext,
           "fcs" = read.FCS(input$fcsFile1$datapath, transformation = FALSE),
           "csv" = fread(input$fcsFile1$datapath),
           "tsv" = fread(input$fcsFile1$datapath, sep = "\t"),
           stop("Unsupported file format")
    )
  })
  
  fcsFile2 <- reactive({
    req(input$fcsFile2)
    ext <- tools::file_ext(input$fcsFile2$name)
    switch(ext,
           "fcs" = read.FCS(input$fcsFile2$datapath, transformation = FALSE),
           "csv" = fread(input$fcsFile2$datapath),
           "tsv" = fread(input$fcsFile2$datapath, sep = "\t"),
           stop("Unsupported file format")
    )
  })
  
  # Update marker choices when both files are loaded
  observe({
    req(input$fcsFile1, input$fcsFile2)
    
    # Get marker options from first file
    data1 <- fcsFile1()
    data2 <- fcsFile2()
    
    if (inherits(data1, "flowFrame") && inherits(data2, "flowFrame")) {
      # Get common parameters between both files
      params1 <- parameters(data1)
      params2 <- parameters(data2)
      
      common_markers <- intersect(params1$name, params2$name)
      choices <- setNames(common_markers, paste0(common_markers, " - ", params1$desc[match(common_markers, params1$name)]))
    } else {
      # For non-flowFrame data
      common_markers <- intersect(colnames(data1), colnames(data2))
      choices <- common_markers
    }
    
    updateSelectInput(session, "comparisonMarkers", choices = choices, 
                      selected = choices[1:min(5, length(choices))])
  })
  
  # Process both files for comparison
  comparison_results <- reactiveVal(NULL)
  
  observeEvent(input$runComparison, {
    req(input$fcsFile1, input$fcsFile2, input$comparisonMarkers)
    
    withProgress(message = 'Processing comparison...', value = 0, {
      # Process file 1
      data1 <- fcsFile1()
      if (inherits(data1, "flowFrame")) {
        exprs_data1 <- exprs(data1)[, input$comparisonMarkers, drop = FALSE]
      } else {
        exprs_data1 <- data1[, input$comparisonMarkers, drop = FALSE]
      }
      
      # Process file 2
      data2 <- fcsFile2()
      if (inherits(data2, "flowFrame")) {
        exprs_data2 <- exprs(data2)[, input$comparisonMarkers, drop = FALSE]
      } else {
        exprs_data2 <- data2[, input$comparisonMarkers, drop = FALSE]
      }
      
      # Sample events - ensure equal number of cells from each sample
      set.seed(123)
      max_events <- min(input$comparisonEvents, min(nrow(exprs_data1), nrow(exprs_data2)))
      
      # Log the number of cells being sampled
      cat("Sampling", max_events, "cells from each sample\n")
      
      sampled_indices1 <- sample(nrow(exprs_data1), max_events)
      sampled_indices2 <- sample(nrow(exprs_data2), max_events)
      
      sampled_data1 <- exprs_data1[sampled_indices1, ]
      sampled_data2 <- exprs_data2[sampled_indices2, ]
      
      # Transform if requested
      incProgress(0.2, detail = "Transforming data...")
      if (input$comparisonTransform) {
        sampled_data1 <- apply(sampled_data1, 2, asinhTransform, cofactor = input$comparisonCofactor)
        sampled_data2 <- apply(sampled_data2, 2, asinhTransform, cofactor = input$comparisonCofactor)
      }
      
      # Create separate scaled data for each sample
      scaled_data1 <- scale(sampled_data1)
      scaled_data2 <- scale(sampled_data2)
      
      # First perform dimensionality reduction separately for each sample
      incProgress(0.3, detail = paste("Running", input$comparisonMethod, "on individual samples..."))
      
      # Run DR on Control sample
      if (input$comparisonMethod == "t-SNE") {
        dr_result1 <- Rtsne(scaled_data1, dims = 2, perplexity = min(30, max(5, max_events/10)), verbose = FALSE)
        reduced_data1 <- data.frame(dim1 = dr_result1$Y[,1], dim2 = dr_result1$Y[,2])
      } else {
        dr_result1 <- umap(scaled_data1, n_neighbors = min(15, max(3, max_events/10)))
        reduced_data1 <- data.frame(dim1 = dr_result1[,1], dim2 = dr_result1[,2])
      }
      
      # Run DR on Treated sample
      if (input$comparisonMethod == "t-SNE") {
        dr_result2 <- Rtsne(scaled_data2, dims = 2, perplexity = min(30, max(5, max_events/10)), verbose = FALSE)
        reduced_data2 <- data.frame(dim1 = dr_result2$Y[,1], dim2 = dr_result2$Y[,2])
      } else {
        dr_result2 <- umap(scaled_data2, n_neighbors = min(15, max(3, max_events/10)))
        reduced_data2 <- data.frame(dim1 = dr_result2[,1], dim2 = dr_result2[,2])
      }
      
      # Then optionally run on combined data for comparison
      incProgress(0.5, detail = paste("Running", input$comparisonMethod, "on combined data..."))
      
      # Combine data for joint dimensionality reduction
      combined_data <- rbind(sampled_data1, sampled_data2)
      combined_scaled <- scale(combined_data)
      
      if (input$comparisonMethod == "t-SNE") {
        dr_result_combined <- Rtsne(combined_scaled, dims = 2, perplexity = input$comparisonPerplexity, verbose = FALSE)
        reduced_data_combined <- data.frame(dim1 = dr_result_combined$Y[,1], dim2 = dr_result_combined$Y[,2])
      } else {
        dr_result_combined <- umap(combined_scaled, n_neighbors = input$comparisonNeighbors)
        reduced_data_combined <- data.frame(dim1 = dr_result_combined[,1], dim2 = dr_result_combined[,2])
      }
      
      # Split the combined results
      result1_combined <- reduced_data_combined[1:nrow(sampled_data1), ]
      result2_combined <- reduced_data_combined[(nrow(sampled_data1)+1):nrow(combined_data), ]
      
      # Add original data to the individual reductions
      result1 <- cbind(reduced_data1, as.data.frame(sampled_data1))
      result2 <- cbind(reduced_data2, as.data.frame(sampled_data2))
      
      # Add original data to the combined reduction
      result1_combined <- cbind(result1_combined, as.data.frame(sampled_data1))
      result2_combined <- cbind(result2_combined, as.data.frame(sampled_data2))
      
      # Mark sample origin
      result1$sample <- "Control"
      result2$sample <- "Treated"
      result1_combined$sample <- "Control"
      result2_combined$sample <- "Treated"
      
      # Add a flag to indicate which reduction method was used
      result1$reduction_type <- "individual"
      result2$reduction_type <- "individual"
      result1_combined$reduction_type <- "combined"
      result2_combined$reduction_type <- "combined"
      
      # Store both separate and combined results
      combined_result_separate <- rbind(result1, result2)
      combined_result_combined <- rbind(result1_combined, result2_combined)
      
      # Store results - now including both reduction approaches
      comparison_results(list(
        control = result1,
        treated = result2,
        control_combined = result1_combined, 
        treated_combined = result2_combined,
        combined = combined_result_combined,
        combined_separate = combined_result_separate,
        method = input$comparisonMethod,
        n_control = nrow(result1),
        n_treated = nrow(result2)
      ))
      
      # Reset clustering when new data is loaded
      comparison_clustering_results(NULL)
    })
  })
  
  # Plot individual comparisons
  output$comparisonPlot1 <- renderPlotly({
    req(comparison_results())
    results <- comparison_results()
    
    p <- ggplot(results$control, aes(x = dim1, y = dim2)) +
      geom_point(alpha = 0.7, color = "blue") +
      labs(title = "Control Sample",
           x = paste(results$method, "1"), 
           y = paste(results$method, "2")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$comparisonPlot2 <- renderPlotly({
    req(comparison_results())
    results <- comparison_results()
    
    p <- ggplot(results$treated, aes(x = dim1, y = dim2)) +
      geom_point(alpha = 0.7, color = "red") +
      labs(title = "Treated Sample",
           x = paste(results$method, "1"), 
           y = paste(results$method, "2")) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Create outputs for comparison clustering visualizations
  
  # Update combined visualization to include cluster information
  output$combinedComparisonPlot <- renderPlotly({
    req(comparison_results())
    results <- comparison_results()
    
    plot_data <- results$combined
    
    # Add cluster information if available
    if (!is.null(comparison_clustering_results())) {
      plot_data$Cluster <- as.factor(comparison_clustering_results()$cluster_ids)
      
      p <- ggplot(plot_data, aes(x = dim1, y = dim2, color = sample, shape = Cluster)) +
        geom_point(alpha = 0.7) +
        scale_color_manual(values = c("Control" = "blue", "Treated" = "red")) +
        labs(title = paste("Combined", results$method, "Projection with Clusters"),
             x = paste(results$method, "1"), 
             y = paste(results$method, "2")) +
        theme_minimal()
    } else {
      p <- ggplot(plot_data, aes(x = dim1, y = dim2, color = sample)) +
        geom_point(alpha = 0.7) +
        scale_color_manual(values = c("Control" = "blue", "Treated" = "red")) +
        labs(title = paste("Combined", results$method, "Projection"),
             x = paste(results$method, "1"), 
             y = paste(results$method, "2")) +
        theme_minimal()
    }
    
    ggplotly(p)
  })
  
  # Dedicated cluster plot
  output$comparisonClusterPlot <- renderPlotly({
    req(comparison_clustering_results(), comparison_results())
    
    results <- comparison_results()
    
    # Get the total number of cells from both samples
    total_cells <- results$n_control + results$n_treated
    
    # Extract individual sample data
    control_data <- results$control
    treated_data <- results$treated
    
    # Get cluster IDs
    cluster_ids <- comparison_clustering_results()$cluster_ids
    
    # Check if cluster IDs match the total number of cells
    if (length(cluster_ids) != total_cells) {
      showNotification(paste("Cluster ID mismatch:", length(cluster_ids), 
                             "IDs for", total_cells, "cells. Adjusting..."), 
                       type = "warning", duration = 5)
      
      # Fix the mismatch - either truncate or extend the cluster IDs
      if (length(cluster_ids) > total_cells) {
        cluster_ids <- cluster_ids[1:total_cells]
      } else {
        # Fill with most common cluster ID if not enough
        most_common <- as.numeric(names(sort(table(cluster_ids), decreasing = TRUE)[1]))
        cluster_ids <- c(cluster_ids, rep(most_common, total_cells - length(cluster_ids)))
      }
    }
    
    # Split cluster IDs between control and treated samples
    control_cluster_ids <- cluster_ids[1:nrow(control_data)]
    treated_cluster_ids <- cluster_ids[(nrow(control_data)+1):length(cluster_ids)]
    
    # Assign cluster IDs to each sample
    control_data$Cluster <- as.factor(control_cluster_ids)
    treated_data$Cluster <- as.factor(treated_cluster_ids)
    
    # Add population labels if available
    if (!is.null(comparison_identified_populations()) && input$showComparisonPopulationLabels) {
      pop_data <- comparison_identified_populations()
      
      # Helper function to safely assign population labels
      assign_populations <- function(data) {
        if (nrow(data) == 0) {
          return(data)
        }
        
        cluster_nums <- as.numeric(as.character(data$Cluster))
        valid_idx <- cluster_nums <= nrow(pop_data) & cluster_nums > 0
        
        if (!all(valid_idx)) {
          showNotification("Some cluster IDs couldn't be matched to populations", type = "warning")
          cluster_nums[!valid_idx] <- 1
        }
        
        data$Population <- pop_data$Population[cluster_nums]
        return(data)
      }
      
      control_data <- assign_populations(control_data)
      treated_data <- assign_populations(treated_data)
      
      # Create hover text for each sample
      control_hover <- paste(
        "Cluster:", control_data$Cluster,
        "<br>Population:", control_data$Population,
        "<br>", results$method, "1:", round(control_data$dim1, 2),
        "<br>", results$method, "2:", round(control_data$dim2, 2)
      )
      
      treated_hover <- paste(
        "Cluster:", treated_data$Cluster,
        "<br>Population:", treated_data$Population,
        "<br>", results$method, "1:", round(treated_data$dim1, 2),
        "<br>", results$method, "2:", round(treated_data$dim2, 2)
      )
    } else {
      # Simple hover text without population info
      control_hover <- paste(
        "Cluster:", control_data$Cluster,
        "<br>", results$method, "1:", round(control_data$dim1, 2),
        "<br>", results$method, "2:", round(control_data$dim2, 2)
      )
      
      treated_hover <- paste(
        "Cluster:", treated_data$Cluster,
        "<br>", results$method, "1:", round(treated_data$dim1, 2),
        "<br>", results$method, "2:", round(treated_data$dim2, 2)
      )
    }
    
    # Now create a subplot with both samples side by side
    # Control plot
    p1 <- plot_ly(control_data, x = ~dim1, y = ~dim2, color = ~Cluster, 
                  text = control_hover, type = "scatter", mode = "markers",
                  marker = list(size = 8, opacity = 0.7),
                  showlegend = FALSE) %>%
      layout(
        title = "Control Sample",
        xaxis = list(title = paste(results$method, "1")),
        yaxis = list(title = paste(results$method, "2")),
        margin = list(t = 50)
      )
    
    # Treated plot  
    p2 <- plot_ly(treated_data, x = ~dim1, y = ~dim2, color = ~Cluster, 
                  text = treated_hover, type = "scatter", mode = "markers",
                  marker = list(size = 8, opacity = 0.7)) %>%
      layout(
        title = "Treated Sample",
        xaxis = list(title = paste(results$method, "1")),
        yaxis = list(title = paste(results$method, "2")),
        margin = list(t = 50)
      )
    
    # Combine into a single subplot with shared legend
    combinedPlot <- subplot(p1, p2, nrows = 1, shareY = TRUE, titleX = TRUE) %>%
      layout(
        title = list(
          text = paste("Clusters from", comparison_clustering_results()$method),
          font = list(size = 20)
        ),
        legend = list(title = list(text = "Cluster")),
        annotations = list(
          list(
            x = 0.25, y = 1,
            text = paste("Control (", nrow(control_data), " cells)", sep=""),
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            font = list(size = 16)
          ),
          list(
            x = 0.75, y = 1,
            text = paste("Treated (", nrow(treated_data), " cells)", sep=""),
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            font = list(size = 16)
          )
        )
      )
    
    return(combinedPlot)
  })
  
  # Cluster heatmap
  output$comparisonClusterHeatmap <- renderPlot({
    req(comparison_clustering_results())
    
    library(reshape2)
    centers <- comparison_clustering_results()$centers
    df <- as.data.frame(centers) %>%
      tibble::rownames_to_column("Cluster") %>%
      melt(id.vars = "Cluster", variable.name = "Marker", value.name = "Expression")
    
    ggplot(df, aes(x = Marker, y = Cluster, fill = Expression)) +
      geom_tile() +
      scale_fill_gradient2(low = "navy", mid = "white", high = "firebrick3") +
      labs(
        title = paste("Cluster Intensity Profiles from", comparison_clustering_results()$method),
        x = "Markers", y = "Clusters"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        axis.text.x   = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y   = element_text(size = 16),
        plot.title    = element_text(size = 18, face = "bold"),
        axis.title    = element_text(size = 16)
      )
  })
  
  # Cluster statistics including sample distribution
  output$comparisonClusterStats <- DT::renderDataTable({
    req(comparison_clustering_results(), comparison_results())
    
    # Get data with clusters
    plot_data <- comparison_results()$combined
    plot_data$Cluster <- as.factor(comparison_clustering_results()$cluster_ids)
    
    # Calculate basic cluster statistics
    cluster_stats <- plot_data %>%
      group_by(Cluster) %>%
      summarize(
        Count = n(),
        Percentage = n() / nrow(plot_data) * 100,
        across(all_of(input$comparisonMarkers), 
               list(Mean = ~mean(., na.rm = TRUE), 
                    Median = ~median(., na.rm = TRUE),
                    SD = ~sd(., na.rm = TRUE)))
      )
    
    # Calculate sample distribution within each cluster
    sample_distribution <- plot_data %>%
      group_by(Cluster, sample) %>%
      summarize(Count = n(), .groups = "drop_last") %>%
      mutate(Percentage = Count / sum(Count) * 100) %>%
      pivot_wider(
        id_cols = Cluster,
        names_from = sample, 
        values_from = c(Count, Percentage),
        names_glue = "{sample}_{.value}"
      )
    
    # Join the two tables
    full_stats <- left_join(cluster_stats, sample_distribution, by = "Cluster")
    
    DT::datatable(full_stats, 
                  options = list(scrollX = TRUE, pageLength = 5),
                  caption = paste("Cluster statistics from", comparison_clustering_results()$method))
  })
  
  markers_cleaned <- reactive({
    req(cleaned_data())
    grep("^[48]", colnames(cleaned_data()), value = TRUE)
  })
  
  output$preview <- renderDT({
    req(cleaned_data())
    datatable(head(cleaned_data(), 10), options = list(scrollX = TRUE))
  })
  
  output$structure <- renderPrint({
    req(cleaned_data())
    list(
      Columns = colnames(cleaned_data()),
      CD4 = grep("^4", colnames(cleaned_data()), value = TRUE),
      CD8 = grep("^8", colnames(cleaned_data()), value = TRUE)
    )
  })
  
  output$analysis_type_ui <- renderUI({
    req(input$cleanedFile)
    selectInput("analysis_type", "Select Analysis Type", 
                choices = c("Marker Comparison", "Treatment Comparison", 
                            "Fold Change", "Summary Statistics"))
  })
  
  output$marker_ui <- renderUI({
    req(markers_cleaned())
    pickerInput("selected_markers", "Select Markers", choices = markers_cleaned(), 
                multiple = TRUE, selected = markers_cleaned()[1], options = list(`actions-box` = TRUE))
  })
  
  output$treatment_ui <- renderUI({
    req(cleaned_data())
    selectInput("selected_treatment", "Select Treatment Column", 
                choices = names(cleaned_data())[sapply(cleaned_data(), is.character)])
  })
  
  output$plot <- renderPlotly({
    req(input$run_cleaned, cleaned_data(), input$selected_markers, input$selected_treatment)
    df <- cleaned_data()
    df_long <- pivot_longer(df, cols = all_of(input$selected_markers),
                            names_to = "Marker", values_to = "Value")
    
    p <- ggplot(df_long, aes_string(x = input$selected_treatment, y = "Value", color = "Marker")) +
      geom_point(position = position_jitter(width = 0.2), alpha = 0.8, size = 3.5) +
      theme_minimal(base_size = 16) +
      theme(axis.title = element_text(size = 18),
            axis.text  = element_text(size = 15),
            legend.title = element_text(size = 16),
            legend.text  = element_text(size = 14))
    
    ggplotly(p)
  })
  
  output$dimred_plot <- renderPlotly({
    req(input$run_cleaned, cleaned_data(), input$selected_markers, input$selected_treatment)
    df <- cleaned_data()
    df_sel <- df[, input$selected_markers, drop = FALSE] %>%
      mutate(across(everything(), as.numeric)) %>%
      na.omit()
    
    if (nrow(df_sel) < 3) {
      showNotification("Not enough data for dimensionality reduction.", type = "error")
      return(NULL)
    }
    
    treatment <- df[as.numeric(rownames(df_sel)), input$selected_treatment]
    sample_ids <- if ("Sample" %in% colnames(df)) df[as.numeric(rownames(df_sel)), "Sample"] else paste("Sample", seq_len(nrow(df_sel)))
    
    set.seed(42)  # Ensure reproducibility
    dimred <- switch(input$dimred_method,
                     "t-SNE" = Rtsne(df_sel, perplexity = input$perplexity_cleaned, verbose = FALSE, check_duplicates = FALSE)$Y,
                     "UMAP" = umap(df_sel, n_neighbors = input$neighbors, min_dist = input$min_dist))
    
    cluster_labels <- as.factor(kmeans(df_sel, centers = input$n_clusters, nstart = 10)$cluster)
    
    dimred_df <- data.frame(Dim1 = dimred[,1], Dim2 = dimred[,2], 
                            Treatment = treatment, 
                            Cluster = cluster_labels,
                            Sample = sample_ids)
    
    plot_ly(dimred_df, x = ~Dim1, y = ~Dim2, color = ~Treatment,
            text = ~paste("Sample:", Sample, "<br>Cluster:", Cluster),
            colors = "Set1",
            type = 'scatter', mode = 'markers',
            marker = list(size = 12, line = list(width = 1, color = '#333')),
            width = input$plot_width, height = input$plot_height) %>%
      layout(title = list(text = paste(input$dimred_method, "Projection"), font = list(size = 20)),
             xaxis = list(title = "Dim 1", titlefont = list(size = 18), tickfont = list(size = 15),
                          scaleanchor = "y", scaleratio = 1),
             yaxis = list(title = "Dim 2", titlefont = list(size = 18), tickfont = list(size = 15)),
             margin = list(l = 50, r = 50, b = 50, t = 60))
    
  })
  
  output$summary_table <- renderDT({
    req(input$run_cleaned, cleaned_data(), input$selected_markers, input$selected_treatment)
    df <- cleaned_data()
    df_long <- pivot_longer(df, cols = all_of(input$selected_markers),
                            names_to = "Marker", values_to = "Value")
    
    summary_df <- df_long %>%
      group_by(across(all_of(input$selected_treatment)), Marker) %>%
      summarise(Mean = mean(Value, na.rm = TRUE),
                SD = sd(Value, na.rm = TRUE), .groups = "drop")
    
    datatable(summary_df)
  })
  
  # --- SAMPLE COMPARISON CLUSTERING ---
  
  comparison_clustering_results <- reactiveVal(NULL)
  comparison_identified_populations <- reactiveVal(NULL) # Store identified populations for comparison
  
  observeEvent(input$runComparisonClustering, {
    req(comparison_results(), input$comparisonMarkers)
    
    # Reset identified populations when new clustering is performed
    comparison_identified_populations(NULL)
    
    withProgress(message = 'Clustering comparison data...', value = 0, {
      # Get marker data for both control and treated samples
      results <- comparison_results()
      
      # Extract the marker columns, not the dimensionality reduction columns
      control_data <- results$control[, input$comparisonMarkers, drop = FALSE]
      treated_data <- results$treated[, input$comparisonMarkers, drop = FALSE]
      
      # Combine data for clustering - using marker expression values not the DR coordinates
      combined_markers <- rbind(control_data, treated_data)
      
      # Count cells
      n_control <- nrow(control_data)
      n_treated <- nrow(treated_data)
      total_cells <- n_control + n_treated
      
      # Log info
      cat(sprintf("Clustering %d total cells (%d Control, %d Treated)\n", 
                  total_cells, n_control, n_treated))
      
      # Scale the marker data for clustering
      scaled_markers <- scale(combined_markers)
      
      # Run the selected clustering algorithm
      if (input$comparisonClusterMethod == "K-means") {
        set.seed(123)  # For reproducibility
        incProgress(0.3, detail = "Running K-means clustering...")
        
        # Run K-means
        km <- kmeans(scaled_markers, centers = input$comparisonNumClusters, nstart = 25)
        cluster_ids <- km$cluster
        
        # Store cluster centers
        centers <- km$centers
        colnames(centers) <- input$comparisonMarkers
        
        # Create results object
        results <- list(
          cluster_ids = cluster_ids,
          centers = centers,
          method = "K-means"
        )
        
        comparison_clustering_results(results)
      }
      else if (input$comparisonClusterMethod == "DBSCAN") {
        incProgress(0.3, detail = "Running DBSCAN clustering...")
        
        # Run DBSCAN
        dbscan_result <- dbscan::dbscan(scaled_markers, 
                                        eps = input$comparison_dbscan_eps, 
                                        minPts = input$comparison_dbscan_minPts)
        cluster_ids <- dbscan_result$cluster
        
        # Handle outliers (cluster 0 in DBSCAN) - assign to a new cluster
        if (any(cluster_ids == 0)) {
          max_cluster <- max(cluster_ids)
          cluster_ids[cluster_ids == 0] <- max_cluster + 1
          showNotification(paste("Assigned", sum(cluster_ids == (max_cluster + 1)), 
                                 "DBSCAN outlier points to cluster", max_cluster + 1),
                           type = "message")
        }
        
        # Calculate cluster centers
        unique_clusters <- sort(unique(cluster_ids))
        centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(combined_markers))
        
        for (i in seq_along(unique_clusters)) {
          cluster_idx <- which(cluster_ids == unique_clusters[i])
          if (length(cluster_idx) > 0) {
            centers[i,] <- colMeans(combined_markers[cluster_idx, , drop = FALSE])
          }
        }
        
        colnames(centers) <- input$comparisonMarkers
        
        # Create results object
        results <- list(
          cluster_ids = cluster_ids,
          centers = centers,
          method = "DBSCAN"
        )
        
        comparison_clustering_results(results)
      }
      else if (input$comparisonClusterMethod == "FlowSOM") {
        incProgress(0.3, detail = "Running FlowSOM clustering...")
        
        tryCatch({
          # Create a flowFrame for FlowSOM
          marker_matrix <- as.matrix(combined_markers)
          colnames(marker_matrix) <- input$comparisonMarkers
          fcs_data <- flowCore::flowFrame(marker_matrix)
          
          # Set proper parameters
          params <- flowCore::parameters(fcs_data)
          params$name <- input$comparisonMarkers
          params$desc <- input$comparisonMarkers
          flowCore::parameters(fcs_data) <- params
          
          # Prepare data for FlowSOM
          fsom_input <- FlowSOM::ReadInput(fcs_data, transform = FALSE, scale = TRUE)
          
          # Build the SOM
          fsom <- FlowSOM::BuildSOM(fsom_input, 
                                    xdim = input$comparison_som_xdim, 
                                    ydim = input$comparison_som_ydim)
          
          # Get metaclusters
          metacl <- FlowSOM::MetaClustering(fsom$map$codes, 
                                            method = "metaClustering_consensus",
                                            max = input$comparison_som_clusters)
          
          # Get cluster IDs for each cell
          cell_som_clusters <- fsom$map$mapping[,1]
          cell_metaclusters <- metacl[cell_som_clusters]
          
          # Calculate metacluster centers
          unique_clusters <- sort(unique(cell_metaclusters))
          centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(combined_markers))
          
          for (i in seq_along(unique_clusters)) {
            cluster_idx <- which(cell_metaclusters == unique_clusters[i])
            if (length(cluster_idx) > 0) {
              centers[i,] <- colMeans(combined_markers[cluster_idx, , drop = FALSE])
            }
          }
          
          colnames(centers) <- input$comparisonMarkers
          
          # Create results object
          results <- list(
            cluster_ids = cell_metaclusters,
            centers = centers,
            som_object = fsom,
            som_clusters = cell_som_clusters,
            metaclusters = metacl,
            method = "FlowSOM"
          )
          
          comparison_clustering_results(results)
        }, error = function(e) {
          showNotification(
            paste("FlowSOM error:", e$message, 
                  "Try adjusting parameters or selecting different markers."),
            type = "error",
            duration = 10
          )
        })
      }
      else if (input$comparisonClusterMethod == "Phenograph") {
        incProgress(0.3, detail = "Running Phenograph clustering...")
        
        tryCatch({
          # Run Phenograph - adjust k parameter based on dataset size
          k_param <- min(input$comparison_pheno_k, max(5, floor(total_cells/5)))
          
          if (k_param != input$comparison_pheno_k) {
            showNotification(paste("Adjusted Phenograph k parameter to", k_param, 
                                   "based on dataset size"),
                             type = "message")
          }
          
          pheno_result <- Rphenograph(as.matrix(scaled_markers), k = k_param)
          
          # Get cluster IDs
          cluster_ids <- as.numeric(membership(pheno_result[[2]]))
          
          # Calculate cluster centers
          unique_clusters <- sort(unique(cluster_ids))
          centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(combined_markers))
          
          for (i in seq_along(unique_clusters)) {
            cluster_idx <- which(cluster_ids == unique_clusters[i])
            if (length(cluster_idx) > 0) {
              centers[i,] <- colMeans(combined_markers[cluster_idx, , drop = FALSE])
            }
          }
          
          colnames(centers) <- input$comparisonMarkers
          
          # Create results object
          results <- list(
            cluster_ids = cluster_ids,
            centers = centers,
            pheno_object = pheno_result,
            method = "Phenograph"
          )
          
          comparison_clustering_results(results)
          
          # If successful, show info about cluster distribution
          control_clusters <- cluster_ids[1:n_control]
          treated_clusters <- cluster_ids[(n_control+1):total_cells]
          
          control_unique <- length(unique(control_clusters))
          treated_unique <- length(unique(treated_clusters))
          
          showNotification(paste("Clustering complete. Control sample has cells in", 
                                 control_unique, "clusters. Treated sample has cells in", 
                                 treated_unique, "clusters."),
                           type = "message", duration = 5)
          
        }, error = function(e) {
          showNotification(
            paste("Phenograph error:", e$message, 
                  "Try adjusting k parameter or selecting different markers."),
            type = "error",
            duration = 10
          )
        })
      }
    })
  })
  
  # Handle the comparison population identification button click
  observeEvent(input$identifyComparisonPopulations, {
    req(comparison_clustering_results())
    
    withProgress(message = 'Identifying cell populations in comparison data...', value = 0, {
      # Get cluster centers
      centers <- comparison_clustering_results()$centers
      marker_names <- colnames(centers)
      
      # Run identification algorithm with user-configured thresholds
      populations <- identify_cell_populations(
        centers, 
        marker_names,
        high_threshold = input$comparisonHighExpressionThreshold,
        low_threshold = input$comparisonLowExpressionThreshold,
        min_confidence = input$comparisonMinConfidenceThreshold/100  # Convert from percentage
      )
      
      # Store results
      comparison_identified_populations(populations)
      
      # Show notification
      showNotification("Cell populations identified for comparison clusters", type = "message")
    })
  })
  
  # Comparison population results table
  output$comparisonPopulationTable <- DT::renderDataTable({
    req(comparison_identified_populations(), comparison_clustering_results(), comparison_results())
    
    population_info <- comparison_identified_populations()
    
    # Add cluster sizes and distribution by sample
    cluster_counts <- table(comparison_clustering_results()$cluster_ids)
    total_cells <- sum(cluster_counts)
    
    # Get sample distribution for each cluster
    plot_data <- comparison_results()$combined
    plot_data$Cluster <- comparison_clustering_results()$cluster_ids
    
    # Calculate per-sample distribution
    sample_distribution <- plot_data %>%
      group_by(Cluster, sample) %>%
      summarize(Count = n(), .groups = "drop_last") %>%
      mutate(Percentage = Count / sum(Count) * 100) %>%
      pivot_wider(
        id_cols = Cluster,
        names_from = sample, 
        values_from = c(Count, Percentage),
        names_glue = "{sample}_{.value}"
      )
    
    # Create main results table  
    result_table <- data.frame(
      Cluster = population_info$Cluster,
      Population = population_info$Population,
      Count = as.numeric(cluster_counts[population_info$Cluster]),
      Percentage = round(as.numeric(cluster_counts[population_info$Cluster]) / total_cells * 100, 2),
      Confidence = round(population_info$Confidence, 1),
      MatchDetails = population_info$MatchDetails
    )
    
    # Merge with sample distribution info
    result_table <- left_join(result_table, sample_distribution, by = "Cluster")
    
    # Sort by population name then by cluster number
    result_table <- result_table[order(result_table$Population, result_table$Cluster), ]
    
    DT::datatable(
      result_table,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      caption = "Identified Cell Populations for Sample Comparison"
    )
  })
}


shinyApp(ui, server)
