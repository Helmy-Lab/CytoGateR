# Clustering Module for Flow Cytometry Analysis Tool

#' UI for the Clustering Module
#' @param id Module ID
#' @return UI elements for clustering controls
clusteringModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Clustering options
    h4("Clustering Options"),
    checkboxInput(ns("showClusteringOptions"), "Enable Clustering", value = TRUE),
    
    conditionalPanel(
      condition = paste0("input['", ns("showClusteringOptions"), "'] === true"),
      selectInput(ns("clusterMethod"), "Clustering Method",
                  choices = c("K-means", "DBSCAN", "FlowSOM", "Phenograph"),
                  selected = "FlowSOM"),
      
      # K-means specific parameters
      conditionalPanel(
        condition = paste0("input['", ns("clusterMethod"), "'] === 'K-means'"),
        numericInput(ns("numClusters"), "Number of Clusters", value = 8, min = 2, max = 20)
      ),
      
      # DBSCAN specific parameters
      conditionalPanel(
        condition = paste0("input['", ns("clusterMethod"), "'] === 'DBSCAN'"),
        numericInput(ns("dbscanEps"), "DBSCAN epsilon", value = 0.5, min = 0.01, step = 0.05),
        numericInput(ns("dbscanMinPts"), "DBSCAN minPts", value = 5, min = 1, step = 1)
      ),
      
      # FlowSOM specific parameters
      conditionalPanel(
        condition = paste0("input['", ns("clusterMethod"), "'] === 'FlowSOM'"),
        numericInput(ns("som_xdim"), "SOM Grid X dimension", value = 6, min = 2, max = 20),
        numericInput(ns("som_ydim"), "SOM Grid Y dimension", value = 6, min = 2, max = 20),
        numericInput(ns("som_clusters"), "Number of metaclusters", value = 12, min = 2, max = 30),
        numericInput(ns("som_rlen"), "Training iterations", value = 10, min = 5, max = 100, step = 5)
      ),
      
      # Phenograph specific parameters
      conditionalPanel(
        condition = paste0("input['", ns("clusterMethod"), "'] === 'Phenograph'"),
        numericInput(ns("phenoK"), "Phenograph k nearest neighbors", value = 30, min = 5, max = 100)
      ),
      
      # Population identification section
      h5("Population Identification"),
      checkboxInput(ns("identifyPopulations"), "Identify Cell Populations", value = TRUE),
      checkboxInput(ns("showPopulationLabels"), "Show Population Labels", value = TRUE),
      
      # Advanced identification options
      checkboxInput(ns("showAdvancedIdentOptions"), "Show Advanced Population ID Settings", value = FALSE),
      conditionalPanel(
        condition = paste0("input['", ns("showAdvancedIdentOptions"), "'] === true"),
        sliderInput(ns("highExpressionThreshold"), "High Expression Threshold", 
                    min = 0.1, max = 1.5, value = 0.5, step = 0.1),
        sliderInput(ns("lowExpressionThreshold"), "Low Expression Threshold", 
                    min = -1.5, max = -0.1, value = -0.5, step = 0.1),
        sliderInput(ns("minConfidenceThreshold"), "Minimum Confidence Threshold (%)", 
                    min = 10, max = 90, value = 30, step = 5)
      ),
      
      # Run clustering button
      br(),
      actionButton(ns("runClustering"), "Run Clustering", class = "btn-success"),
      
      # Add cluster visualization controls section
      conditionalPanel(
        condition = paste0("input['", ns("showClusteringOptions"), "'] === true"),
        hr(),
        h5("Cluster Visualization Controls"),
        
        # Show cluster labels checkbox (moved from main panel)
        div(
          style = "margin-bottom: 15px;",
          checkboxInput(ns("showClusterLabels"), "Show Cluster Labels", value = FALSE)
        ),
        
        # Cluster management buttons (moved from main panel)
        div(
          style = "display: flex; flex-direction: column; gap: 10px;",
          actionButton(ns("showMergeModal"), "Merge Similar Clusters", 
                      class = "btn-info", icon = icon("object-group"), width = "100%"),
          actionButton(ns("resetMerging"), "Reset to Original Clusters", 
                      class = "btn-warning", icon = icon("undo"), width = "100%")
        )
      )
    )
  )
}

#' Server function for the Clustering Module
#' 
#' @param id Module ID
#' @param input_data Reactive expression containing data to cluster (scaled_data and markers)
#' @param app_state Reactive values containing global app state
#' @return List with clustering results and populations
clusteringModuleServer <- function(id, input_data, app_state) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values to store results
    clustering_results <- reactiveVal(NULL)
    populations <- reactiveVal(NULL)
    
    # Run clustering when button is clicked
    observeEvent(input$runClustering, {
      req(input_data(), input$showClusteringOptions)
      
      # Ensure we have data to cluster
      data <- input_data()
      req(data$scaled_data, data$markers)
      
      # Extract marker data for clustering
      marker_data <- data$scaled_data
      marker_names <- data$markers
      
      # Create progress notification
      withProgress(message = 'Clustering cells...', value = 0, {
        
        # Prepare clustering parameters based on selected method
        method <- input$clusterMethod
        params <- list()
        
        if (method == "K-means") {
          params$num_clusters <- input$numClusters
        }
        else if (method == "DBSCAN") {
          params$eps <- input$dbscanEps
          params$minPts <- input$dbscanMinPts
        }
        else if (method == "FlowSOM") {
          params$xdim <- input$som_xdim
          params$ydim <- input$som_ydim
          params$n_metaclusters <- input$som_clusters
          params$rlen <- input$som_rlen
        }
        else if (method == "Phenograph") {
          params$k <- input$phenoK
        }
        
        # Run clustering
        incProgress(0.3, detail = paste("Running", method, "clustering..."))
        cluster_result <- runClustering(marker_data, method, params)
        
        # Store clustering results
        if (!is.null(cluster_result)) {
          clustering_results(cluster_result)
          
          # Run population identification if enabled
          if (input$identifyPopulations) {
            incProgress(0.7, detail = "Identifying cell populations...")
            
            # Get identification parameters
            high_threshold <- input$highExpressionThreshold
            low_threshold <- input$lowExpressionThreshold
            min_confidence <- input$minConfidenceThreshold / 100  # Convert from percentage
            
            # Run identification
            pop_result <- identify_cell_populations(
              cluster_result$centers,
              marker_names,
              high_threshold = high_threshold,
              low_threshold = low_threshold,
              min_confidence = min_confidence
            )
            
            # Store population results
            populations(pop_result)
            
            # Show success notification
            showNotification(
              paste("Cell clustering complete with", method, "and populations identified"),
              type = "message",
              duration = 4
            )
          } else {
            # Clear population results if not identifying
            populations(NULL)
            
            # Show success notification
            showNotification(
              paste("Cell clustering complete with", method),
              type = "message",
              duration = 4
            )
          }
        } else {
          # Show error notification if clustering failed
          showNotification(
            paste("Clustering failed. Try different parameters or another method."),
            type = "error",
            duration = 5
          )
        }
      })
    })
    
    # Return reactive expressions with results
    return(list(
      clustering_results = clustering_results,
      populations = populations,
      showClusteringOptions = reactive(input$showClusteringOptions),
      showPopulationLabels = reactive(input$showPopulationLabels),
      showClusterLabels = reactive(input$showClusterLabels)
    ))
  })
}