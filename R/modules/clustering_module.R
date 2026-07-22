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
    
    # Replace conditionalPanel with server-side rendering
    uiOutput(ns("clusteringSettingsUI"))
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
    
    # ============================================================================
    # SERVER-SIDE CONDITIONAL UI RENDERING
    # ============================================================================
    
    # Main clustering settings UI
    output$clusteringSettingsUI <- renderUI({
      if (isTRUE(input$showClusteringOptions)) {
        tagList(
          selectInput(session$ns("clusterMethod"), "Clustering Method",
                      choices = c("K-means", "DBSCAN", "FlowSOM", "Phenograph"),
                      selected = "FlowSOM"),
          
          # Method-specific parameters (server-side rendering)
          uiOutput(session$ns("methodSpecificParametersUI")),
          
          # Population identification section
          h5("Population Identification"),
          checkboxInput(session$ns("identifyPopulations"), "Identify Cell Populations", value = TRUE),
          checkboxInput(session$ns("showPopulationLabels"), "Show Population Labels", value = TRUE),
          
          # Advanced identification options
          checkboxInput(session$ns("showAdvancedIdentOptions"), "Show Advanced Population ID Settings", value = FALSE),
          
          # Advanced options (server-side rendering)
          uiOutput(session$ns("advancedIdentOptionsUI")),
          
          # Run clustering button
          br(),
          actionButton(session$ns("runClustering"), "Run Clustering", class = "btn-success"),
          
          # Cluster visualization controls
          hr(),
          h5("Cluster Visualization Controls"),
          
          # Show cluster labels checkbox
          div(
            style = "margin-bottom: 15px;",
            checkboxInput(session$ns("showClusterLabels"), "Show Cluster Labels", value = FALSE)
          ),
          
          # Cluster management buttons
          div(
            style = "display: flex; flex-direction: column; gap: 10px;",
            actionButton(session$ns("showMergeModal"), "Merge Similar Clusters", 
                        class = "btn-info", icon = icon("object-group"), width = "100%"),
            actionButton(session$ns("resetMerging"), "Reset to Original Clusters", 
                        class = "btn-warning", icon = icon("undo"), width = "100%")
          )
        )
      }
    })
    
    # Method-specific parameters UI
    output$methodSpecificParametersUI <- renderUI({
      req(input$clusterMethod)
      
      method <- input$clusterMethod
      
      if (method == "K-means") {
        div(class = "method-controls", style = "background-color: #e3f2fd; padding: 10px; border-radius: 5px; margin: 10px 0;",
          h6(icon("cog"), "K-means Parameters"),
          numericInput(session$ns("numClusters"), "Number of Clusters", value = 8, min = 2, max = 20),
          helpText("Specify the number of clusters to create. K-means requires this to be set in advance.")
        )
      } else if (method == "DBSCAN") {
        div(class = "method-controls", style = "background-color: #f3e5f5; padding: 10px; border-radius: 5px; margin: 10px 0;",
          h6(icon("cog"), "DBSCAN Parameters"),
          numericInput(session$ns("dbscanEps"), "DBSCAN epsilon", value = 0.5, min = 0.01, step = 0.05),
          helpText("Maximum distance between two samples for one to be considered as in the neighborhood of the other."),
          numericInput(session$ns("dbscanMinPts"), "DBSCAN minPts", value = 5, min = 1, step = 1),
          helpText("Minimum number of samples in a neighborhood for a point to be considered as a core point.")
        )
      } else if (method == "FlowSOM") {
        div(class = "method-controls", style = "background-color: #e8f5e8; padding: 10px; border-radius: 5px; margin: 10px 0;",
          h6(icon("cog"), "FlowSOM Parameters"),
          fluidRow(
            column(6,
              numericInput(session$ns("som_xdim"), "SOM Grid X dimension", value = 6, min = 2, max = 20)
            ),
            column(6,
              numericInput(session$ns("som_ydim"), "SOM Grid Y dimension", value = 6, min = 2, max = 20)
            )
          ),
          numericInput(session$ns("som_clusters"), "Number of metaclusters", value = 12, min = 2, max = 30),
          helpText("Number of final clusters to create from the SOM grid."),
          numericInput(session$ns("som_rlen"), "Training iterations", value = 10, min = 5, max = 100, step = 5),
          helpText("Number of training iterations for the SOM.")
        )
      } else if (method == "Phenograph") {
        div(class = "method-controls", style = "background-color: #fff3e0; padding: 10px; border-radius: 5px; margin: 10px 0;",
          h6(icon("cog"), "Phenograph Parameters"),
          numericInput(session$ns("phenoK"), "Phenograph k nearest neighbors", value = 30, min = 5, max = 100),
          helpText("Number of nearest neighbors to consider when building the graph for clustering.")
        )
      }
    })
    
    # Advanced identification options UI
    output$advancedIdentOptionsUI <- renderUI({
      if (isTRUE(input$showAdvancedIdentOptions)) {
        div(class = "parameter-group", style = "background-color: #f8f9fa; padding: 15px; border-radius: 8px; margin: 10px 0; border: 1px solid #dee2e6;",
          h6(icon("sliders-h"), "Advanced Population Identification Settings"),
          sliderInput(session$ns("highExpressionThreshold"), "High Expression Threshold", 
                      min = 0.1, max = 1.5, value = 0.5, step = 0.1),
          helpText("Z-score threshold above which a marker is considered highly expressed."),
          
          sliderInput(session$ns("lowExpressionThreshold"), "Low Expression Threshold", 
                      min = -1.5, max = -0.1, value = -0.5, step = 0.1),
          helpText("Z-score threshold below which a marker is considered lowly expressed."),
          
          sliderInput(session$ns("minConfidenceThreshold"), "Minimum Confidence Threshold (%)", 
                      min = 10, max = 90, value = 30, step = 5),
          helpText("Minimum confidence percentage required for population identification.")
        )
      }
    })
    
    # ============================================================================
    # END OF SERVER-SIDE CONDITIONAL UI RENDERING
    # ============================================================================
    
    # ============================================================================
    # RUN CLUSTERING -- ASYNC (Framework 2.1)
    #
    # DBSCAN/FlowSOM/Phenograph on more than a few thousand events routinely
    # run for tens of seconds to minutes. Running that synchronously inside
    # withProgress() blocked the main R session and was one of the causes of
    # server disconnections. Heavy work now runs inside future_promise() on a
    # worker process (plan(multisession) set in global.R), keeping the main
    # session responsive.
    #
    # IMPORTANT: all input$* values are captured as local variables BEFORE
    # entering future_promise() -- Shiny inputs are not readable inside the
    # async worker.
    # ============================================================================
    observeEvent(input$runClustering, {
      req(input_data(), input$showClusteringOptions)
      
      # Ensure we have data to cluster
      data <- input_data()
      req(data$scaled_data, data$markers)
      
      # Pre-computation guard: warn before launching expensive algorithms on
      # large datasets. Does not block -- user can still proceed.
      n_events <- nrow(data$scaled_data)
      if (n_events > 50000) {
        showNotification(
          paste0("Large dataset: ", format(n_events, big.mark = ","),
                 " events detected. Clustering may take several minutes."),
          type = "warning", duration = 8
        )
      }
      
      # Capture everything the worker needs before leaving the reactive context
      marker_data  <- data$scaled_data
      marker_names <- data$markers
      method       <- input$clusterMethod
      
      # Prepare clustering parameters based on selected method
      params <- list()
      if (method == "K-means") {
        params$num_clusters <- max(2L, min(100L, as.integer(input$numClusters)))
      } else if (method == "DBSCAN") {
        params$eps    <- max(0.001, min(100, as.numeric(input$dbscanEps)))
        params$minPts <- max(1L, min(1000L, as.integer(input$dbscanMinPts)))
      } else if (method == "FlowSOM") {
        params$xdim           <- max(2L, min(20L, as.integer(input$som_xdim)))
        params$ydim           <- max(2L, min(20L, as.integer(input$som_ydim)))
        params$n_metaclusters <- max(2L, min(50L, as.integer(input$som_clusters)))
        params$rlen           <- max(1L, min(1000L, as.integer(input$som_rlen)))
      } else if (method == "Phenograph") {
        params$k <- max(5L, min(500L, as.integer(input$phenoK)))
      }
      
      identify_pops  <- isTRUE(input$identifyPopulations)
      high_threshold <- input$highExpressionThreshold
      low_threshold  <- input$lowExpressionThreshold
      min_confidence <- input$minConfidenceThreshold / 100  # Convert from percentage
      
      showNotification(paste("Running", method, "clustering..."), id = "cluster_msg", duration = NULL)
      
      # Runs in a separate R process. All objects referenced inside must be
      # self-contained -- no reactive reads, no writes to reactiveVal().
      future_promise({
        
        tryCatch({
          # Explicit library() calls: multisession workers do not reliably
          # inherit the main session's attached packages, and runClustering()
          # calls Rphenograph()/membership() unqualified (no pkg:: prefix).
          library(dbscan)
          library(FlowSOM)
          library(Rphenograph)
          library(igraph)
          library(flowCore)
          
          cluster_result <- runClustering(marker_data, method, params)
          
          if (is.null(cluster_result)) {
            stop(paste0(method, " clustering failed. Try different parameters or another method."))
          }
          
          pop_result <- NULL
          if (identify_pops) {
            pop_result <- identify_cell_populations(
              cluster_result$centers,
              marker_names,
              high_threshold = high_threshold,
              low_threshold  = low_threshold,
              min_confidence = min_confidence
            )
          }
          
          list(cluster_result = cluster_result, pop_result = pop_result, method = method)
          
        }, error = function(e) {
          stop(paste("Clustering error:", conditionMessage(e)))
        })
        
        # Promise success handler -- back on the main Shiny thread. Only here
        # is it safe to write to reactiveVal() / update the UI.
      }) %...>% (function(out) {
        
        clustering_results(out$cluster_result)
        populations(out$pop_result)
        removeNotification("cluster_msg")
        
        if (!is.null(out$pop_result)) {
          showNotification(
            paste("Cell clustering complete with", out$method, "and populations identified"),
            type = "message", duration = 4
          )
        } else {
          showNotification(
            paste("Cell clustering complete with", out$method),
            type = "message", duration = 4
          )
        }
        
        # Promise error handler -- catches any error thrown inside
        # future_promise() so the user gets visible feedback instead of a
        # spinner that never resolves.
      }) %...!% (function(err) {
        removeNotification("cluster_msg")
        showNotification(
          paste("Clustering failed:", conditionMessage(err)),
          type = "error", duration = 6
        )
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
