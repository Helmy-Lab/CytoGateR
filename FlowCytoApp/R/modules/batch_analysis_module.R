# Batch Analysis Module for Flow Cytometry Analysis Tool

#' UI for the Batch Analysis Module
#' @param id Module ID
#' @return UI elements for batch analysis
batchAnalysisModuleUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      h4("Sample Management"),
      # Sample File Upload and Group Assignment
      fileInput(ns("batchFile"), "Upload FCS/CSV/TSV File", accept = c(".fcs", ".csv", ".tsv"), multiple = TRUE),
      uiOutput(ns("batchSampleList")),  # Dynamic UI for sample list and group assignment
      actionButton(ns("addSample"), "Add Selected Files", class = "btn-info"),
      hr(),
      
      # Sample Grouping Options
      selectInput(ns("groupingVariable"), "Group Samples By:",
                  choices = c("Manual Assignment", "Filename Pattern"),
                  selected = "Manual Assignment"),
      conditionalPanel(
        condition = paste0("input['", ns("groupingVariable"), "'] === 'Filename Pattern'"),
        textInput(ns("patternControl"), "Control Pattern", value = "control|ctrl"),
        textInput(ns("patternTreated"), "Treated Pattern", value = "treated|sample")
      ),
      hr(),
      
      # Common Analysis Parameters
      h4("Analysis Parameters"),
      uiOutput(ns("batchMarkerSelectUI")),
      
      # Preprocessing options (same for all samples)
      checkboxInput(ns("batchTransform"), "Apply arcsinh transformation", value = TRUE),
      numericInput(ns("batchCofactor"), "Transformation cofactor", value = 5, min = 1, max = 10),
      numericInput(ns("batchEvents"), "Events per sample", value = 5000, min = 100, step = 1000),
      
      # Show advanced preprocessing
      checkboxInput(ns("showBatchPreprocessing"), "Show Advanced Preprocessing", value = FALSE),
      conditionalPanel(
        condition = paste0("input['", ns("showBatchPreprocessing"), "'] === true"),
        # QC options
        h5("Quality Control"),
        checkboxInput(ns("batchPerformQC"), "Perform Quality Control", value = TRUE),
        conditionalPanel(
          condition = paste0("input['", ns("showBatchPreprocessing"), "'] === true && input['", ns("batchPerformQC"), "'] === true"),
          numericInput(ns("batchMaxAnomalies"), "Max Anomalies (%)", value = 10, min = 0, max = 50)
        ),
        
        # Gating options
        h5("Gating"),
        checkboxInput(ns("batchPerformGating"), "Perform Debris/Dead Cell Gating", value = TRUE),
        conditionalPanel(
          condition = paste0("input['", ns("showBatchPreprocessing"), "'] === true && input['", ns("batchPerformGating"), "'] === true"),
          textInput(ns("batchDebrisGate"), "FSC/SSC Parameters (comma-separated)", 
                    value = "FSC-A,SSC-A"),
          selectInput(ns("batchLiveDeadGate"), "Live/Dead Parameter", 
                      choices = c("None", "Live Dead BV570 Violet-610-A"),
                      selected = "None")
        )
      ),
      
      # Dimensionality Reduction
      h5("Dimensionality Reduction"),
      selectInput(ns("batchDimRedMethod"), "Method", 
                  choices = c("t-SNE", "UMAP", "PCA", "MDS"), selected = "t-SNE"),
      conditionalPanel(
        condition = paste0("input['", ns("batchDimRedMethod"), "'] === 't-SNE'"),
        sliderInput(ns("batchPerplexity"), "t-SNE perplexity", min = 5, max = 50, value = 30),
        checkboxInput(ns("batch_use_barnes_hut"), "Use Barnes-Hut Approximation (faster)", value = TRUE),
        conditionalPanel(
          condition = paste0("input['", ns("batch_use_barnes_hut"), "']"),
          sliderInput(ns("batchTsneTheta"), "Barnes-Hut theta", 
                      min = 0.0, max = 1.0, value = 0.5, step = 0.1)
        ),
        conditionalPanel(
          condition = paste0("!input['", ns("batch_use_barnes_hut"), "']"),
          tags$div(class = "alert alert-warning",
                   "Warning: Exact t-SNE is very slow for datasets > 1000 cells.")
        ),
        numericInput(ns("batch_tsne_max_iter"), "Maximum Iterations", value = 1000, min = 100, max = 10000, step = 100)
      ),
      conditionalPanel(
        condition = paste0("input['", ns("batchDimRedMethod"), "'] === 'UMAP'"),
        sliderInput(ns("batchNeighbors"), "UMAP n_neighbors", min = 2, max = 100, value = 15)
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("batchDimRedMethod"), "'] === 'PCA'"),
        numericInput(ns("batchPcaComponents"), "PCA: Number of Components", value = 2, min = 2, max = 10)
      ),
      
      conditionalPanel(
        condition = paste0("input['", ns("batchDimRedMethod"), "'] === 'MDS'"),
        tags$p("MDS does not require additional parameters. It uses Euclidean distances by default."),
        tags$small("Note: MDS can be slow for large datasets.")
      ),
      
      # Add clustering controls
      hr(),
      h4("Clustering Options"),
      checkboxInput(ns("showBatchClustering"), "Enable Clustering", value = TRUE),
      
      conditionalPanel(
        condition = paste0("input['", ns("showBatchClustering"), "'] === true"),
        # clustering methods
        selectInput(ns("batchClusterMethod"), "Clustering Method",
                    choices = c("K-means", "FlowSOM", "DBSCAN", "Phenograph"),
                    selected = "FlowSOM"),
        
        conditionalPanel(
          condition = paste0("input['", ns("showBatchClustering"), "'] === true && input['", ns("batchClusterMethod"), "'] === 'K-means'"),
          numericInput(ns("batchNumClusters"), "Number of Clusters", value = 8, min = 2, max = 30)
        ),
        conditionalPanel(
          condition = paste0("input['", ns("showBatchClustering"), "'] === true && input['", ns("batchClusterMethod"), "'] === 'FlowSOM'"),
          numericInput(ns("batchSomXdim"), "SOM Grid X dimension", value = 6, min = 2, max = 20),
          numericInput(ns("batchSomYdim"), "SOM Grid Y dimension", value = 6, min = 2, max = 20),
          numericInput(ns("batchSomRlen"), "Training iterations", value = 10, min = 5, max = 50, step = 5),
          numericInput(ns("batchSomClusters"), "Number of clusters", value = 12, min = 2, max = 30)
        ),
        conditionalPanel(
          condition = paste0("input['", ns("showBatchClustering"), "'] === true && input['", ns("batchClusterMethod"), "'] === 'DBSCAN'"),
          numericInput(ns("batchDbscanEps"), "Epsilon (neighborhood size)", value = 0.5, min = 0.1, max = 5, step = 0.1),
          numericInput(ns("batchDbscanMinPts"), "MinPts (min samples in neighborhood)", value = 5, min = 3, max = 50)
        ),
        conditionalPanel(
          condition = paste0("input['", ns("showBatchClustering"), "'] === true && input['", ns("batchClusterMethod"), "'] === 'Phenograph'"),
          numericInput(ns("batchPhenoK"), "k (nearest neighbors)", value = 30, min = 5, max = 100)
        ),
        
        # Population identification
        checkboxInput(ns("batchIdentifyPops"), "Identify Cell Populations", value = TRUE),
        checkboxInput(ns("batchShowPopLabels"), "Show Population Labels", value = TRUE),
        
        # Population identification thresholds
        conditionalPanel(
          condition = paste0("input['", ns("batchIdentifyPops"), "'] === true"),
          sliderInput(ns("batchHighExpressionThreshold"), "High Expression Threshold",
                      min = 0, max = 2, value = 0.5, step = 0.1),
          sliderInput(ns("batchLowExpressionThreshold"), "Low Expression Threshold",
                      min = -2, max = 0, value = -0.5, step = 0.1),
          sliderInput(ns("batchMinConfidenceThreshold"), "Minimum Confidence Threshold (%)",
                      min = 0, max = 100, value = 30, step = 5)
        ),
        
        # Add cluster visualization controls section
        hr(),
        h5("Cluster Visualization Controls"),
        
        # Show cluster labels checkbox (moved from main panel)
        div(
          style = "margin-bottom: 15px;",
          checkboxInput(ns("showClusterLabels"), "Show Population Labels", value = TRUE)
        ),
        
        # Cluster management buttons (moved from main panel)
        div(
          style = "display: flex; flex-direction: column; gap: 10px;",
          actionButton(ns("showMergeModal"), "Merge Similar Clusters", 
                    class = "btn-info", icon = icon("object-group"), width = "100%"),
          actionButton(ns("resetMerging"), "Reset to Original Clusters", 
                    class = "btn-warning", icon = icon("undo"), width = "100%")
        )
      ),
      
      # Run button for analysis
      hr(),
      actionButton(ns("runBatchAnalysis"), "Run Batch Analysis", class = "btn-primary", 
                   style = "width: 100%; font-weight: bold;")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Sample Management",
                 h4("Sample Groups"),
                 DT::dataTableOutput(ns("batchSampleTable")),
                 hr(),
                 fluidRow(
                   column(12, h4("Sample Management Actions")),
                   column(4, actionButton(ns("clearSamples"), "Clear All Samples", class = "btn-warning", style = "width: 100%")),
                   column(4, downloadButton(ns("downloadSampleConfig"), "Save Sample Config", style = "width: 100%")),
                   column(4, div(style = "width: 100%", fileInput(ns("uploadSampleConfig"), "Load Config", accept = c(".csv"))))
                 )
        ),
        
        tabPanel("Sample Visualization",
                 fluidRow(
                   column(3, selectInput(ns("viewSample"), "Select Sample:", choices = NULL)),
                   column(9, h4(textOutput(ns("sampleViewTitle")), align = "center"))
                 ),
                 hr(),
                 div(
                   class = "plot-container-wrapper",
                   style = "margin-bottom: 250px; position: relative; overflow: visible; padding-bottom: 50px;", # Much larger margin
                   fluidRow(
                     column(12, 
                            h4("Dimensionality Reduction Plot", align = "center"),
                            shinycssloaders::withSpinner(plotlyOutput(ns("sampleDimensionalityPlot"), height = "600px")))
                   )
                 ),
                 conditionalPanel(
                   condition = paste0("input['", ns("showBatchClustering"), "'] === true"),
                   hr(),
                   # Drastically increase margin for clustering visualization
                   div(
                     class = "plot-container-wrapper",
                     style = "margin-bottom: 300px; position: relative; overflow: visible; padding-bottom: 50px;", # Very large margin
                     fluidRow(
                       column(12, h4("Clustering Visualization", align = "center")),
                       column(6, shinycssloaders::withSpinner(plotlyOutput(ns("sampleClusterPlot"), height = "550px"))),
                       column(6, shinycssloaders::withSpinner(plotOutput(ns("sampleHeatmap"), height = "550px")))
                     )
                   ),
                   hr(),
                   # Increase spacing for cluster statistics section
                   div(
                     class = "plot-container-wrapper",
                     style = "margin-bottom: 200px; position: relative; overflow: visible; padding-bottom: 50px;", # Large margin
                     fluidRow(
                       column(12, h4("Cluster Statistics", align = "center")),
                       column(12, shinycssloaders::withSpinner(DT::dataTableOutput(ns("sampleClusterStats"))))
                     )
                   ),
                   hr(),
                   # Add much more spacing for marker expression section
                   div(
                     class = "plot-container-wrapper",
                     style = "margin-top: 30px; margin-bottom: 250px; position: relative; overflow: visible; padding-bottom: 80px;", # Very large margin
                     fluidRow(
                       column(12, h4("Marker Expression by Cluster", align = "center")),
                       column(4, selectInput(ns("sampleMarkerSelect"), "Select Marker:", choices = NULL)),
                       column(8, shinycssloaders::withSpinner(plotlyOutput(ns("markerExpressionByCluster"), height = "550px")))
                     )
                   )
                 )
        ),
        
        tabPanel("Control vs Treated", 
                 fluidRow(
                   column(12, h4("Sample Comparison", align = "center")),
                   column(6, selectInput(ns("compareViewControl"), "Control Sample:", choices = NULL)),
                   column(6, selectInput(ns("compareViewTreated"), "Treated Sample:", choices = NULL))
                 ),
                 hr(),
                 # Drastically increase spacing for Control vs Treated dimensionality reduction plots
                 div(
                   class = "plot-container-wrapper",
                   style = "margin-top: 20px; margin-bottom: 300px; position: relative; overflow: visible; padding-bottom: 50px;", # Much larger margin
                   fluidRow(
                     column(12, h4("Dimensionality Reduction Comparison", align = "center")),
                     column(6, shinycssloaders::withSpinner(plotlyOutput(ns("controlSamplePlot"), height = "600px"))),
                     column(6, shinycssloaders::withSpinner(plotlyOutput(ns("treatedSamplePlot"), height = "600px")))
                   )
                 )
        ),
        
        tabPanel("Cluster Comparison",
                 conditionalPanel(
                   condition = paste0("input['", ns("showBatchClustering"), "'] === true"),
                   fluidRow(
                     column(6, selectInput(ns("clusterCompareControl"), "Control Sample:", choices = NULL)),
                     column(6, selectInput(ns("clusterCompareTreated"), "Treated Sample:", choices = NULL))
                   ),
                   hr(),
                   # Added container with spacing for the cluster mapping heatmap
                   div(
                     class = "plot-container-wrapper",
                     style = "margin-bottom: 250px; position: relative; overflow: visible; padding-bottom: 80px;", # Much larger margin
                     fluidRow(
                       column(12, 
                              h4("Cluster Mapping", align = "center"),
                              p("Visualize how clusters from Control and Treated samples relate to each other based on marker expression similarity"),
                              shinycssloaders::withSpinner(plotOutput(ns("clusterMappingHeatmap"), height = "650px")))
                     )
                   ),

                   hr(),
                   # Add much larger margins for signature markers heatmap
                   div(
                     class = "plot-container-wrapper",
                     style = "margin-top: 30px; margin-bottom: 300px; position: relative; overflow: visible; padding-bottom: 80px;", # Much larger margin
                     fluidRow(
                       column(12, h4("Signature Markers by Cluster", align = "center")),
                       column(12, shinycssloaders::withSpinner(plotOutput(ns("signatureMarkerHeatmap"), height = "650px")))
                     )
                   )
                 ),
                 conditionalPanel(
                   condition = paste0("!input['", ns("showBatchClustering"), "']"),
                   h3("Clustering must be enabled to use this feature", align = "center", 
                      style = "margin-top: 100px; color: #888;")
                 )
        )
      )
    )
  )
}

#' Server function for the Batch Analysis Module
#' 
#' @param id Module ID
#' @param app_state Reactive values with global app state
#' @return List with batch analysis results
batchAnalysisModuleServer <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    # Create reactive values to store batch samples and their results
    batchSamples <- reactiveVal(list())
    batchResults <- reactiveVal(list())
    
    # Add reactive values to track cluster merging
    mergeHistory <- reactiveVal(list(
      active = FALSE,
      current_clusters = NULL,
      original_clusters = NULL,
      current_mapping = NULL,
      operations = list()
    ))
    
    # Function to add samples to the batch list
    observeEvent(input$addSample, {
      req(input$batchFile)
      
      # Get current sample list
      current_samples <- batchSamples()
      
      # Process new files
      for (i in 1:nrow(input$batchFile)) {
        file_data <- input$batchFile[i, ]
        sample_id <- paste0("sample_", length(current_samples) + 1)
        
        # Determine group based on filename pattern or default to "Unknown"
        group <- "Unknown"
        if (input$groupingVariable == "Filename Pattern") {
          if (grepl(input$patternControl, file_data$name, ignore.case = TRUE)) {
            group <- "Control"
          } else if (grepl(input$patternTreated, file_data$name, ignore.case = TRUE)) {
            group <- "Treated"
          }
        }
        
        # Add the sample to the list
        current_samples[[sample_id]] <- list(
          id = sample_id,
          name = file_data$name,
          path = file_data$datapath,
          group = group,
          added = Sys.time()
        )
      }
      
      # Update the sample list
      batchSamples(current_samples)
    })
    
    # Clear all samples
    observeEvent(input$clearSamples, {
      batchSamples(list())
      batchResults(list())
    })
    
    # UI for sample list management
    output$batchSampleList <- renderUI({
      samples <- batchSamples()
      if (length(samples) == 0) {
        return(div(
          style = "margin-top: 15px; text-align: center;",
          "No samples added yet. Use the file selector above to add samples."
        ))
      }
      
      # Create a panel for each sample
      sample_panels <- lapply(names(samples), function(id) {
        sample <- samples[[id]]
        div(
          style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 10px;",
          div(
            style = "display: flex; justify-content: space-between;",
            strong(sample$name),
            selectInput(
              inputId = session$ns(paste0("group_", id)),
              label = NULL,
              choices = c("Control", "Treated", "Unknown"),
              selected = sample$group,
              width = "120px"
            )
          ),
          div(
            style = "display: flex; justify-content: space-between; margin-top: 5px;",
            span(paste("Added:", format(sample$added, "%Y-%m-%d %H:%M"))),
            actionButton(
              inputId = session$ns(paste0("remove_", id)),
              label = "Remove",
              class = "btn-danger btn-sm"
            )
          )
        )
      })
      
      # Return the UI
      div(
        style = "max-height: 300px; overflow-y: auto; padding: 10px; border: 1px solid #eee; margin-top: 10px;",
        do.call(tagList, sample_panels)
      )
    })
    
    # Update sample groups when changed in UI
    observe({
      samples <- batchSamples()
      if (length(samples) == 0) return()
      
      # Check for group changes
      updated <- FALSE
      for (id in names(samples)) {
        group_input_id <- paste0("group_", id)
        if (!is.null(input[[group_input_id]])) {
          if (samples[[id]]$group != input[[group_input_id]]) {
            samples[[id]]$group <- input[[group_input_id]]
            updated <- TRUE
          }
        }
      }
      
      # Update samples if needed
      if (updated) {
        batchSamples(samples)
      }
    })
    
    # Handle sample removal buttons
    observe({
      samples <- batchSamples()
      if (length(samples) == 0) return()
      
      # Check for remove button clicks
      for (id in names(samples)) {
        # Local id to avoid capturing issues
        local_id <- id 
        remove_id <- paste0("remove_", local_id)
        
        # Create remove button observer
        # Note: This creates many observers but is necessary for dynamic buttons
        observeEvent(input[[remove_id]], {
          current_samples <- batchSamples()
          current_samples[[local_id]] <- NULL
          batchSamples(current_samples)
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      }
    })
    
    # Display the sample table
    output$batchSampleTable <- DT::renderDataTable({
      samples <- batchSamples()
      if (length(samples) == 0) {
        return(data.frame(
          "Sample Name" = character(0),
          "Group" = character(0),
          "Status" = character(0)
        ))
      }
      
      # Create a data frame for the table
      df <- data.frame(
        "Sample ID" = sapply(samples, function(s) s$id),
        "Sample Name" = sapply(samples, function(s) s$name),
        "Group" = sapply(samples, function(s) s$group),
        "Status" = sapply(samples, function(s) {
          if (!is.null(batchResults()[[s$id]])) {
            "Analyzed"
          } else {
            "Pending"
          }
        }),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(
        df,
        options = list(
          pageLength = 10,
          searching = TRUE,
          dom = 'tip',
          scrollX = TRUE
        ),
        rownames = FALSE
      )
    })
    
    # Create marker selection UI for batch analysis
    output$batchMarkerSelectUI <- renderUI({
      samples <- batchSamples()
      if (length(samples) == 0) {
        return(div("Add samples first to select markers"))
      }
      
      # Try to get markers from the first sample
      first_sample <- samples[[1]]
      
      # Load the sample data
      tryCatch({
        file_data <- loadFlowData(first_sample$path, first_sample$name)$data
        
        if (inherits(file_data, "flowFrame")) {
          exprs_data <- exprs(file_data)
          params <- parameters(file_data)
          choices <- setNames(colnames(exprs_data), paste0(colnames(exprs_data), " - ", params$desc))
        } else {
          exprs_data <- file_data
          choices <- colnames(exprs_data)
        }
        
        selectInput(
          session$ns("batchSelectedMarkers"), 
          "Select Markers/Channels", 
          choices = choices, 
          selected = choices[1:min(8, length(choices))], 
          multiple = TRUE
        )
      }, error = function(e) {
        div("Error loading sample data:", e$message)
      })
    })
    
    # Handle save/load sample configuration 
    output$downloadSampleConfig <- downloadHandler(
      filename = function() {
        paste0("flowcyto_samples_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        samples <- batchSamples()
        if (length(samples) == 0) return(NULL)
        
        # Create a data frame for export
        df <- data.frame(
          id = sapply(samples, function(s) s$id),
          name = sapply(samples, function(s) s$name),
          path = sapply(samples, function(s) s$path),
          group = sapply(samples, function(s) s$group),
          stringsAsFactors = FALSE
        )
        
        write.csv(df, file, row.names = FALSE)
      }
    )
    
    # Handle loading sample configuration
    observeEvent(input$uploadSampleConfig, {
      req(input$uploadSampleConfig)
      
      tryCatch({
        config <- read.csv(input$uploadSampleConfig$datapath, stringsAsFactors = FALSE)
        
        # Validate the config
        if (!all(c("id", "name", "path", "group") %in% colnames(config))) {
          showNotification("Invalid configuration file format", type = "error")
          return()
        }
        
        # Create sample list from config
        samples <- list()
        for (i in 1:nrow(config)) {
          samples[[config$id[i]]] <- list(
            id = config$id[i],
            name = config$name[i],
            path = config$path[i],
            group = config$group[i],
            added = Sys.time()
          )
        }
        
        batchSamples(samples)
        showNotification("Sample configuration loaded", type = "message")
      }, error = function(e) {
        showNotification(paste("Error loading configuration:", e$message), type = "error")
      })
    })
    
    # Run the batch analysis
    observeEvent(input$runBatchAnalysis, {
      samples <- batchSamples()
      
      if (length(samples) == 0) {
        showNotification("No samples to analyze", type = "warning")
        return()
      }
      
      req(input$batchSelectedMarkers)
      
      # Prepare common preprocessing parameters
      preprocessing_params <- list(
        markers = input$batchSelectedMarkers,
        transform = input$batchTransform,
        cofactor = input$batchCofactor,
        n_events = input$batchEvents,
        perform_qc = isTRUE(input$batchPerformQC),
        perform_gating = isTRUE(input$batchPerformGating),
        scale_data = TRUE,
        seed = 123
      )
      
      # Add QC settings if enabled
      if (isTRUE(input$batchPerformQC)) {
        preprocessing_params$qc_settings <- list(
          max_anomalies = input$batchMaxAnomalies / 100  # Convert from percentage to proportion
        )
      }
      
      # Add gating parameters if enabled
      if (isTRUE(input$batchPerformGating)) {
        # Parse debris gate parameters from comma-separated string
        debris_gate_params <- unlist(strsplit(input$batchDebrisGate, ",\\s*"))
        
        preprocessing_params$gates <- list(
          debris_gate = if (length(debris_gate_params) >= 2) debris_gate_params[1:2] else NULL,
          live_dead_gate = if (input$batchLiveDeadGate != "None") input$batchLiveDeadGate else NULL
        )
      }
      
      # Initialize results list
      results <- list()
      
      # Define a processing function for one sample
      processSample <- function(sample) {
        withProgress(
          message = paste("Processing", sample$name),
          value = 0, 
          {
            # Load the file data
            file_data <- loadFlowData(sample$path, sample$name)$data
            
            # Process the data using the common parameters
            incProgress(0.2, detail = "Preprocessing data...")
            preprocess_results <- preprocessFlowData(file_data, preprocessing_params)
            
            # Run dimensionality reduction
            incProgress(0.4, detail = paste("Running", input$batchDimRedMethod, "..."))
            
            # Perform dimensionality reduction
            if (input$batchDimRedMethod == "t-SNE") {
              perplexity_value <- min(input$batchPerplexity, max(5, nrow(preprocess_results$scaled_data) / 10))
              
              # Ensure the data is a matrix
              data_matrix <- as.matrix(preprocess_results$scaled_data)
              
              # Prepare t-SNE parameters
              tsne_params <- list(
                dims = 2,
                perplexity = perplexity_value,
                max_iter = input$batch_tsne_max_iter,
                verbose = FALSE
              )
              
              if (input$batch_use_barnes_hut) {
                # Use Barnes-Hut t-SNE
                tsne_params$theta <- input$batchTsneTheta
                incProgress(0.1, detail = "Running Barnes-Hut t-SNE...")
              } else {
                # Use exact t-SNE (very slow for large datasets)
                tsne_params$theta <- 0.0  # This triggers exact t-SNE
                incProgress(0.1, detail = "Running exact t-SNE (may be slow)...")
              }
              
              # Run t-SNE with the configured parameters
              dr_result <- do.call(Rtsne, c(list(X = data_matrix), tsne_params))
              
              reduced_data <- data.frame(dim1 = dr_result$Y[,1], dim2 = dr_result$Y[,2])
            } else if (input$batchDimRedMethod == "UMAP") {
              # Run UMAP
              umap_result <- umap(preprocess_results$scaled_data, n_neighbors = input$batchNeighbors)
              reduced_data <- data.frame(dim1 = umap_result[,1], dim2 = umap_result[,2])
            } else if (input$batchDimRedMethod == "PCA") {
              # Run PCA
              num_components <- input$batchPcaComponents
              pca_result <- prcomp(preprocess_results$scaled_data, center = TRUE, scale. = TRUE)
              reduced_data <- data.frame(dim1 = pca_result$x[,1], dim2 = pca_result$x[,2])
              if (num_components > 2) {
                for (i in 3:num_components) {
                  reduced_data[[paste0("PC", i)]] <- pca_result$x[, i]
                }
              }
            } else if (input$batchDimRedMethod == "MDS") {
              # Run MDS
              incProgress(0.1, detail = "Running MDS...")
              data_matrix <- as.matrix(preprocess_results$scaled_data)
              
              # Compute distance matrix and MDS
              dist_matrix <- dist(data_matrix)
              mds_result <- cmdscale(dist_matrix, k = 2)
              
              # Store MDS results in dim1 and dim2
              reduced_data <- data.frame(dim1 = mds_result[,1], dim2 = mds_result[,2])
            }
            
            # Create plot data
            plot_data <- as.data.frame(preprocess_results$sampled_data)
            colnames(plot_data) <- input$batchSelectedMarkers
            
            # Add dimensionality reduction coordinates
            plot_data$dim1 <- reduced_data$dim1
            plot_data$dim2 <- reduced_data$dim2
            
            # Run clustering if enabled
            cluster_results <- NULL
            if (input$showBatchClustering) {
              incProgress(0.6, detail = paste("Clustering with", input$batchClusterMethod, "..."))
              
              # Extract marker data for clustering
              marker_data <- plot_data[, input$batchSelectedMarkers, drop = FALSE]
              
              # Prepare clustering parameters based on method
              method <- input$batchClusterMethod
              params <- list()
              
              if (method == "K-means") {
                params$num_clusters <- input$batchNumClusters
              }
              else if (method == "DBSCAN") {
                params$eps <- input$batchDbscanEps
                params$minPts <- input$batchDbscanMinPts
              }
              else if (method == "FlowSOM") {
                params$xdim <- input$batchSomXdim
                params$ydim <- input$batchSomYdim
                params$n_metaclusters <- input$batchSomClusters
                params$rlen <- input$batchSomRlen
              }
              else if (method == "Phenograph") {
                params$k <- input$batchPhenoK
              }
              
              # Run the clustering algorithm
              cluster_results <- runClustering(marker_data, method, params)
              
              # Add cluster IDs to plot data if clustering was successful
              if (!is.null(cluster_results)) {
                plot_data$Cluster <- as.factor(cluster_results$cluster_ids)
              }
            }
            
            # Run population identification if enabled and clustering was successful
            populations <- NULL
            if (input$batchIdentifyPops && !is.null(cluster_results)) {
              incProgress(0.8, detail = "Identifying cell populations...")
              
              # Get identification parameters
              high_threshold <- input$batchHighExpressionThreshold
              low_threshold <- input$batchLowExpressionThreshold
              min_confidence <- input$batchMinConfidenceThreshold / 100  # Convert from percentage
              
              # Run identification
              populations <- identify_cell_populations(
                cluster_results$centers,
                input$batchSelectedMarkers,
                high_threshold = high_threshold,
                low_threshold = low_threshold,
                min_confidence = min_confidence
              )
              
              # Add population to plot data if available
              if (!is.null(populations) && !is.null(cluster_results)) {
                # Map cluster IDs to population names
                population_map <- setNames(
                  populations$Population,
                  populations$Cluster
                )
                
                # Add population column to plot data
                plot_data$Population <- population_map[as.character(plot_data$Cluster)]
              }
            }
            
            # Store final results
            sample_result <- list(
              id = sample$id,
              name = sample$name,
              group = sample$group,
              preprocess_results = preprocess_results,
              plot_data = plot_data,
              cluster_results = cluster_results,
              populations = populations,
              dim_red_method = input$batchDimRedMethod,
              num_cells = nrow(plot_data),
              processed_time = Sys.time()
            )
            
            return(sample_result)
          }
        )
      }
      
      # Process each sample
      withProgress(
        message = "Running batch analysis",
        value = 0, 
        {
          for (i in seq_along(samples)) {
            incProgress(amount = 1/length(samples), 
                        detail = paste("Processing sample", i, "of", length(samples)))
            
            sample <- samples[[i]]
            result <- processSample(sample)
            results[[sample$id]] <- result
          }
        }
      )
      
      # Update results
      batchResults(results)
      
      # Update sample selection dropdown for visualization
      updateSelectInput(session, "viewSample", 
                        choices = setNames(
                          names(results),
                          sapply(results, function(r) r$name)
                        ))
      
      # Update comparison dropdowns
      control_ids <- c()
      treated_ids <- c()
      control_names <- c()
      treated_names <- c()
      
      # Loop through results to find control and treated samples
      for (id in names(results)) {
        sample <- results[[id]]
        if (sample$group == "Control") {
          control_ids <- c(control_ids, id)
          control_names <- c(control_names, sample$name)
        } else if (sample$group == "Treated") {
          treated_ids <- c(treated_ids, id)
          treated_names <- c(treated_names, sample$name)
        }
      }
      
      if (length(control_ids) > 0) {
        updateSelectInput(session, "compareViewControl", 
                          choices = setNames(control_ids, control_names))
      }
      
      if (length(treated_ids) > 0) {
        updateSelectInput(session, "compareViewTreated",
                          choices = setNames(treated_ids, treated_names))
      }
      
      # Update marker comparison dropdown
      if (length(results) > 0) {
        # Get marker selection
        updateSelectInput(session, "sampleMarkerSelect", 
                          choices = input$batchSelectedMarkers,
                          selected = input$batchSelectedMarkers[1])
      }
      
      # Update cluster comparison dropdowns
      if (length(control_ids) > 0) {
        updateSelectInput(session, "clusterCompareControl", 
                          choices = setNames(control_ids, control_names))
      }
      
      if (length(treated_ids) > 0) {
        updateSelectInput(session, "clusterCompareTreated",
                          choices = setNames(treated_ids, treated_names))
      }
      
      # Show batch analysis completion notification
      showNotification(paste("Batch analysis complete for", length(samples), "samples"), 
                       type = "message", 
                       duration = 5)
    })
    
    # Single sample view title
    output$sampleViewTitle <- renderText({
      req(input$viewSample)
      results <- batchResults()
      if (is.null(results) || is.null(results[[input$viewSample]])) {
        return("No sample selected")
      }
      
      sample <- results[[input$viewSample]]
      paste0(sample$name, " (", sample$group, ")")
    })
    
    # Render individual sample dimensionality reduction plot
    output$sampleDimensionalityPlot <- renderPlotly({
      # Explicitly use plot settings to force reactivity
      font_size <- app_state$plot_settings$font_size
      point_size <- app_state$plot_settings$point_size
      color_palette <- app_state$plot_settings$color_palette
      plot_width <- app_state$plot_settings$width
      plot_height <- app_state$plot_settings$height
      
      req(input$viewSample)
      results <- batchResults()
      if (is.null(results) || is.null(results[[input$viewSample]])) {
        return(NULL)
      }
      
      sample <- results[[input$viewSample]]
      plot_data <- sample$plot_data
      
      # If clustering was performed, color by cluster
      if (!is.null(sample$cluster_results) && "Cluster" %in% colnames(plot_data)) {
        # Create hover text based on available information
        if ("Population" %in% colnames(plot_data)) {
          hover_text <- paste(
            "Cluster:", plot_data$Cluster,
            "<br>Population:", plot_data$Population,
            "<br>", sample$dim_red_method, "1:", round(plot_data$dim1, 2),
            "<br>", sample$dim_red_method, "2:", round(plot_data$dim2, 2)
          )
        } else {
          hover_text <- paste(
            "Cluster:", plot_data$Cluster,
            "<br>", sample$dim_red_method, "1:", round(plot_data$dim1, 2),
            "<br>", sample$dim_red_method, "2:", round(plot_data$dim2, 2)
          )
        }
        
        # Determine what to use for color mapping - Population or Cluster
        color_by <- "Cluster"
        if ("Population" %in% colnames(plot_data)) {
          color_by <- "Population"
        }
        
        # Create a ggplot object first
        p <- ggplot(plot_data, aes(x = dim1, y = dim2, color = .data[[color_by]], text = hover_text)) +
          geom_point(alpha = 0.7, size = point_size/2) +
          get_color_palette(color_palette) +
          labs(
            title = paste(sample$dim_red_method, "Plot Colored by", if(color_by == "Population") "Cell Population" else "Cluster"),
            x = paste(sample$dim_red_method, "1"),
            y = paste(sample$dim_red_method, "2"),
            color = if(color_by == "Population") "Cell Population" else "Cluster"
          ) +
          get_standard_theme(font_size) +
          coord_fixed(ratio = 1) # Add fixed coordinate ratio to prevent squeezing
        
        # Convert to plotly with completely explicit font settings
        p_plotly <- ggplotly(p, tooltip = "text", width = plot_width, height = plot_height)
        
        # Apply completely explicit font settings to ensure they're properly applied
        p_plotly <- p_plotly %>% layout(
          font = list(
            family = "Arial",
            size = font_size,
            color = "black"
          ),
          title = list(
            text = paste(sample$dim_red_method, "Plot Colored by", if(color_by == "Population") "Cell Population" else "Cluster"),
            font = list(
              family = "Arial",
              size = font_size * 1.2,
              color = "black"
            )
          ),
          xaxis = list(
            title = list(
              text = paste(sample$dim_red_method, "1"),
              font = list(
                family = "Arial",
                size = font_size * 1.1,
                color = "black"
              )
            ),
            tickfont = list(
              family = "Arial",
              size = font_size
            ),
            scaleanchor = "y",
            scaleratio = 1
          ),
          yaxis = list(
            title = list(
              text = paste(sample$dim_red_method, "2"),
              font = list(
                family = "Arial",
                size = font_size * 1.1,
                color = "black"
              )
            ),
            tickfont = list(
              family = "Arial",
              size = font_size
            )
          ),
          legend = list(
            title = list(
              text = if(color_by == "Population") "Cell Population" else "Cluster",
              font = list(
                family = "Arial",
                size = font_size,
                color = "black"
              )
            ),
            font = list(
              family = "Arial",
              size = font_size * 0.9,
              color = "black"
            ),
            bgcolor = "rgba(255, 255, 255, 0.9)",
            bordercolor = "rgba(0, 0, 0, 0.2)",
            borderwidth = 1,
            x = 0.5,
            xanchor = "center",
            y = -0.15,
            yanchor = "top",
            orientation = "h"
          ),
          hoverlabel = list(
            bgcolor = "white",
            font = list(
              family = "Arial",
              size = font_size * 0.9
            )
          ),
          margin = list(b = 100, l = 80, t = 100, r = 80)
        )
        
        return(p_plotly)
      } else {
        # If no clustering, create a simple plot
        hover_text <- paste(
          sample$dim_red_method, "1:", round(plot_data$dim1, 2),
          "<br>", sample$dim_red_method, "2:", round(plot_data$dim2, 2)
        )
        
        # If we have markers, add them to hover
        for (marker in input$batchSelectedMarkers) {
          if (marker %in% colnames(plot_data)) {
            hover_text <- paste0(hover_text, "<br>", marker, ": ", round(plot_data[[marker]], 2))
          }
        }
        
        # Create a ggplot object first
        p <- ggplot(plot_data, aes(x = dim1, y = dim2, text = hover_text)) +
          geom_point(alpha = 0.7, size = point_size/2, color = "#3366CC") +
          labs(
            title = paste(sample$dim_red_method, "Plot"),
            x = paste(sample$dim_red_method, "1"),
            y = paste(sample$dim_red_method, "2")
          ) +
          get_standard_theme(font_size) +
          coord_fixed(ratio = 1) # Add fixed coordinate ratio to prevent squeezing
        
        # Convert to plotly with completely explicit font settings
        p_plotly <- ggplotly(p, tooltip = "text", width = plot_width, height = plot_height)
        
        # Apply completely explicit font settings to ensure they're properly applied
        p_plotly <- p_plotly %>% layout(
          font = list(
            family = "Arial",
            size = font_size,
            color = "black"
          ),
          title = list(
            text = paste(sample$dim_red_method, "Plot"),
            font = list(
              family = "Arial",
              size = font_size * 1.2,
              color = "black"
            )
          ),
          xaxis = list(
            title = list(
              text = paste(sample$dim_red_method, "1"),
              font = list(
                family = "Arial",
                size = font_size * 1.1,
                color = "black"
              )
            ),
            tickfont = list(
              family = "Arial",
              size = font_size
            ),
            scaleanchor = "y",
            scaleratio = 1
          ),
          yaxis = list(
            title = list(
              text = paste(sample$dim_red_method, "2"),
              font = list(
                family = "Arial",
                size = font_size * 1.1,
                color = "black"
              )
            ),
            tickfont = list(
              family = "Arial",
              size = font_size
            )
          ),
          hoverlabel = list(
            bgcolor = "white",
            font = list(
              family = "Arial",
              size = font_size * 0.9
            )
          ),
          margin = list(b = 100, l = 80, t = 100, r = 80)
        )
        
        return(p_plotly)
      }
    })
    
    # Make cluster plot reactive to palette changes
    observe({
      # This observer will re-run whenever plot settings change
      app_state$plot_settings
      
      # Force all the cluster-related plots to invalidate and re-render
      session$sendCustomMessage(type = "refreshClusterPlot", message = list())
    })
    
    # NEW: Make all dimensionality reduction plots reactive to plot settings changes
    observe({
      # This observer will re-run whenever plot settings change
      app_state$plot_settings
      
      # Force the dimensionality reduction plots to invalidate
      session$sendCustomMessage(type = "refreshPlots", message = list())
      
      # Explicitly invalidate all plotly outputs to force redraw with new font settings
      session$sendCustomMessage(type = "plotly-replot", message = list(id = session$ns("sampleDimensionalityPlot")))
      session$sendCustomMessage(type = "plotly-replot", message = list(id = session$ns("controlSamplePlot")))
      session$sendCustomMessage(type = "plotly-replot", message = list(id = session$ns("treatedSamplePlot")))
      session$sendCustomMessage(type = "plotly-replot", message = list(id = session$ns("markerExpressionByCluster")))
    })
    
    output$sampleClusterPlot <- renderPlotly({
      # Force reactivity to plot settings - convert to local variables
      font_size <- app_state$plot_settings$font_size
      point_size <- app_state$plot_settings$point_size
      color_palette <- app_state$plot_settings$color_palette
      width <- app_state$plot_settings$width
      height <- app_state$plot_settings$height
      
      req(input$viewSample)
      results <- batchResults()
      if (is.null(results) || is.null(results[[input$viewSample]])) {
        return(NULL)
      }
      
      sample <- results[[input$viewSample]]
      if (is.null(sample$cluster_results) || !("Cluster" %in% colnames(sample$plot_data))) {
        return(NULL)
      }
      
      plot_data <- sample$plot_data
      
      # Check if we have merged clusters
      current_merge_history <- mergeHistory()
      if (current_merge_history$active) {
        # Create a temporary data frame with merged clusters
        plot_data <- plot_data
        plot_data$Cluster <- as.factor(current_merge_history$current_clusters)
        
        # Add population information if available
        if (!is.null(current_merge_history$current_mapping)) {
          # Create mapping between cluster ID and population name
          population_map <- setNames(
            current_merge_history$current_mapping$Population,
            current_merge_history$current_mapping$Cluster
          )
          
          # Add population column
          plot_data$Population <- population_map[as.character(plot_data$Cluster)]
        }
      }
      
      # Determine what to use for color mapping - Population or Cluster
      color_by <- "Cluster"
      if ("Population" %in% colnames(plot_data)) {
        color_by <- "Population"
      }
      
      # Create a base ggplot with correct color palette
      p <- ggplot(plot_data, aes(x = dim1, y = dim2, color = .data[[color_by]])) +
        geom_point(alpha = 0.7, size = point_size/2) +
        get_color_palette(color_palette) +
        labs(
          title = paste("Clusters from", sample$cluster_results$method),
          x = paste(sample$dim_red_method, "1"),
          y = paste(sample$dim_red_method, "2"),
          color = if(color_by == "Population") "Cell Population" else "Cluster"
        ) +
        get_standard_theme(font_size)
      
      # Convert to plotly with completely explicit font settings
      p_plotly <- ggplotly(p, tooltip = c("color", "x", "y"), width = width, height = height)
      
      # Apply completely explicit font settings to ensure they're properly applied
      p_plotly <- p_plotly %>% layout(
        font = list(
          family = "Arial",
          size = font_size,
          color = "black"
        ),
        title = list(
          text = paste("Clusters from", sample$cluster_results$method),
          font = list(
            family = "Arial",
            size = font_size * 1.2,
            color = "black"
          )
        ),
        xaxis = list(
          title = list(
            text = paste(sample$dim_red_method, "1"),
            font = list(
              family = "Arial",
              size = font_size * 1.1,
              color = "black"
            )
          ),
          tickfont = list(
            family = "Arial",
            size = font_size
          ),
          scaleanchor = "y",
          scaleratio = 1
        ),
        yaxis = list(
          title = list(
            text = paste(sample$dim_red_method, "2"),
            font = list(
              family = "Arial",
              size = font_size * 1.1,
              color = "black"
            )
          ),
          tickfont = list(
            family = "Arial",
            size = font_size
          )
        ),
        legend = list(
          title = list(
            text = if(color_by == "Population") "Cell Population" else "Cluster",
            font = list(
              family = "Arial",
              size = font_size,
              color = "black"
            )
          ),
          font = list(
            family = "Arial",
            size = font_size * 0.9,
            color = "black"
          ),
          bgcolor = "rgba(255, 255, 255, 0.9)",
          bordercolor = "rgba(0, 0, 0, 0.2)",
          borderwidth = 1,
          x = 0.5,
          xanchor = "center",
          y = -0.15,
          yanchor = "top",
          orientation = "h"
        ),
        hoverlabel = list(
          bgcolor = "white",
          font = list(
            family = "Arial",
            size = font_size * 0.9
          )
        ),
        margin = list(b = 120, l = 80, t = 100, r = 50)
      )
      
      # Add cluster labels if showing population names and user has enabled labels
      if (color_by == "Population" && isTRUE(input$showClusterLabels)) {
        # Calculate cluster centers for label positioning
        cluster_centers <- plot_data %>%
          group_by(Cluster, Population) %>%
          summarize(
            x = mean(dim1, na.rm = TRUE),
            y = mean(dim2, na.rm = TRUE),
            .groups = 'drop'
          )
        
        # Add annotations to plot
        for (i in 1:nrow(cluster_centers)) {
          p_plotly <- p_plotly %>% add_annotations(
            x = cluster_centers$x[i],
            y = cluster_centers$y[i],
            text = cluster_centers$Population[i],
            showarrow = TRUE,
            arrowhead = 0.5,
            arrowsize = 0.5,
            arrowwidth = 1,
            ax = 20,
            ay = -20,
            bgcolor = "rgba(255, 255, 255, 0.8)",
            bordercolor = "rgba(0, 0, 0, 0.5)",
            borderwidth = 1,
            font = list(size = font_size)
          )
        }
      }
      
      return(p_plotly)
    })
    
    # Render sample heatmap
    output$sampleClusterHeatmap <- renderPlot({
      req(input$viewSample)
      results <- batchResults()
      if (is.null(results) || is.null(results[[input$viewSample]])) {
        return(NULL)
      }
      
      sample <- results[[input$viewSample]]
      if (is.null(sample$cluster_results) || !("Cluster" %in% colnames(sample$plot_data))) {
        return(NULL)
      }
      
      # Get center data
      centers <- sample$cluster_results$centers
      method <- sample$cluster_results$method
      
      # Get population data - use merged populations if available
      current_merge_history <- mergeHistory()
      if (current_merge_history$active) {
        population_data <- current_merge_history$current_mapping
      } else {
        population_data <- sample$populations
      }
      
      # Create heatmap
      createClusterHeatmap(
        centers = centers,
        method = method,
        title = paste("Marker Expression by Cluster -", sample$name),
        font_size = app_state$plot_settings$font_size,
        population_data = population_data
      )
    }, width = function() app_state$plot_settings$width,
    height = function() app_state$plot_settings$height)
    
    # Render sample cluster statistics
    output$sampleClusterStats <- DT::renderDataTable({
      req(input$viewSample)
      results <- batchResults()
      if (is.null(results) || is.null(results[[input$viewSample]])) {
        return(NULL)
      }
      
      sample <- results[[input$viewSample]]
      if (is.null(sample$cluster_results) || !("Cluster" %in% colnames(sample$plot_data))) {
        return(NULL)
      }
      
      # Calculate cluster statistics
      plot_data <- sample$plot_data
      
      # Check whether to use merged or original clusters
      current_merge_history <- mergeHistory()
      cluster_ids <- sample$cluster_results$cluster_ids
      population_data <- sample$populations
      
      if (current_merge_history$active) {
        cluster_ids <- current_merge_history$current_clusters
        population_data <- current_merge_history$current_mapping
      }
      
      # Format stats
      stats_df <- formatClusterStats(
        cluster_ids = cluster_ids,
        total_cells = nrow(plot_data),
        population_data = population_data
      )
      
      # Add merge history if available
      if (current_merge_history$active && length(current_merge_history$operations) > 0) {
        # Create a data frame to show merge history
        merge_history_df <- do.call(rbind, lapply(current_merge_history$operations, function(op) {
          data.frame(
            MergeOperation = paste0("Merge ", which(current_merge_history$operations == op)),
            NewPopulation = op$new_name,
            TargetCluster = op$target_cluster,
            OriginalClusters = paste(op$merged_clusters, collapse=", "),
            Timestamp = format(op$timestamp, "%Y-%m-%d %H:%M:%S")
          )
        }))
        
        # Return combined data frame with history at the top
        combined_df <- rbind(
          cbind(stats_df[1, 1:2], MergeHistory = "Cluster Merge History", stringsAsFactors = FALSE),
          cbind(merge_history_df, Cells = NA, Percentage = NA, Confidence = NA)[, c(3,2,1,4,5,6,7,8)],
          stats_df
        )
        
        DT::datatable(
          combined_df,
          options = list(
            pageLength = 15,
            scrollX = TRUE
          ),
          rownames = FALSE,
          caption = paste("Cluster Statistics for", sample$name, "with Merge History")
        ) %>% formatRound(columns = c("Percentage", "Confidence"), digits = 2)
      } else {
        # Just show regular stats
        DT::datatable(
          stats_df,
          options = list(
            pageLength = 15,
            scrollX = TRUE
          ),
          rownames = FALSE,
          caption = paste("Cluster Statistics for", sample$name)
        ) %>% formatRound(columns = c("Percentage", "Confidence"), digits = 2)
      }
    })
    
    # Single sample marker expression by cluster
    output$markerExpressionByCluster <- renderPlotly({
      # Explicitly track app_state$plot_settings to make this plot reactive to font changes
      font_size <- app_state$plot_settings$font_size
      point_size <- app_state$plot_settings$point_size
      color_palette <- app_state$plot_settings$color_palette
      width <- app_state$plot_settings$width
      height <- app_state$plot_settings$height
      
      req(input$viewSample, input$sampleMarkerSelect)
      results <- batchResults()
      
      if (is.null(results) || is.null(results[[input$viewSample]]) || 
          is.null(results[[input$viewSample]]$cluster_results)) {
        return(NULL)
      }
      
      sample <- results[[input$viewSample]]
      marker <- input$sampleMarkerSelect
      
      # Check if marker exists in sample data
      if (!(marker %in% colnames(sample$plot_data))) {
        return(NULL)
      }
      
      # Create marker expression plot and convert to plotly
      p <- createMarkerExpressionPlot(
        plot_data = sample$plot_data,
        marker = marker,
        color_palette = color_palette,
        font_size = font_size,
        include_violin = TRUE
      )
      
      # Convert to plotly with explicit font settings
      p_plotly <- ggplotly(p, width = width, height = height)
      
      # Apply explicit font settings
      p_plotly <- p_plotly %>% layout(
        font = list(
          family = "Arial",
          size = font_size,
          color = "black"
        ),
        title = list(
          text = paste("Distribution of", marker, "by Cluster"),
          font = list(
            family = "Arial",
            size = font_size * 1.2,
            color = "black"
          )
        ),
        xaxis = list(
          title = list(
            text = "Cluster",
            font = list(
              family = "Arial",
              size = font_size * 1.1,
              color = "black"
            )
          ),
          tickfont = list(
            family = "Arial",
            size = font_size
          )
        ),
        yaxis = list(
          title = list(
            text = marker,
            font = list(
              family = "Arial",
              size = font_size * 1.1,
              color = "black"
            )
          ),
          tickfont = list(
            family = "Arial",
            size = font_size
          )
        ),
        hoverlabel = list(
          bgcolor = "white",
          font = list(
            family = "Arial",
            size = font_size * 0.9
          )
        )
      )
      
      return(p_plotly)
    })
    
    # Render comparison plots between control and treated samples
    output$controlSamplePlot <- renderPlotly({
      # Explicitly track app_state$plot_settings to make this plot reactive to font changes
      font_size <- app_state$plot_settings$font_size
      point_size <- app_state$plot_settings$point_size
      color_palette <- app_state$plot_settings$color_palette
      width <- app_state$plot_settings$width
      height <- app_state$plot_settings$height
      
      req(input$compareViewControl)
      results <- batchResults()
      if (is.null(results) || is.null(results[[input$compareViewControl]])) {
        return(NULL)
      }
      
      sample <- results[[input$compareViewControl]]
      plot_data <- sample$plot_data
      
      # If clustering was performed, color by cluster
      if (!is.null(sample$cluster_results) && "Cluster" %in% colnames(plot_data)) {
        # Determine what to use for color mapping - Population or Cluster
        color_by <- "Cluster"
        if ("Population" %in% colnames(plot_data)) {
          color_by <- "Population"
        }
        
        # Create base ggplot with correct colors
        p <- ggplot(plot_data, aes(x = dim1, y = dim2, color = .data[[color_by]])) +
          geom_point(alpha = 0.7, size = point_size/2) +
          get_color_palette(color_palette) +
          labs(
            title = paste("Control:", sample$name),
            x = paste(sample$dim_red_method, "1"),
            y = paste(sample$dim_red_method, "2"),
            color = if(color_by == "Population") "Cell Population" else "Cluster"
          ) +
          get_standard_theme(font_size) +
          coord_fixed(ratio = 1) # Add fixed ratio for proper scaling
        
        # Convert to plotly with completely explicit font settings
        p_plotly <- ggplotly(p, tooltip = c("color", "x", "y"), width = width, height = height)
        
        # Apply completely explicit font settings to ensure they're properly applied
        p_plotly <- p_plotly %>% layout(
          font = list(
            family = "Arial",
            size = font_size,
            color = "black"
          ),
          title = list(
            text = paste("Control:", sample$name),
            font = list(
              family = "Arial",
              size = font_size * 1.2,
              color = "black"
            )
          ),
          xaxis = list(
            title = list(
              text = paste(sample$dim_red_method, "1"),
              font = list(
                family = "Arial",
                size = font_size * 1.1,
                color = "black"
              )
            ),
            tickfont = list(
              family = "Arial",
              size = font_size
            ),
            scaleanchor = "y",
            scaleratio = 1
          ),
          yaxis = list(
            title = list(
              text = paste(sample$dim_red_method, "2"),
              font = list(
                family = "Arial",
                size = font_size * 1.1,
                color = "black"
              )
            ),
            tickfont = list(
              family = "Arial",
              size = font_size
            )
          ),
          legend = list(
            title = list(
              text = if(color_by == "Population") "Cell Population" else "Cluster",
              font = list(
                family = "Arial",
                size = font_size,
                color = "black"
              )
            ),
            font = list(
              family = "Arial",
              size = font_size * 0.9,
              color = "black"
            ),
            bgcolor = "rgba(255, 255, 255, 0.9)",
            bordercolor = "rgba(0, 0, 0, 0.2)",
            borderwidth = 1,
            x = 0.5,
            xanchor = "center",
            y = -0.15,
            yanchor = "top",
            orientation = "h"
          ),
          hoverlabel = list(
            bgcolor = "white",
            font = list(
              family = "Arial",
              size = font_size * 0.9
            )
          ),
          margin = list(b = 80, l = 80, t = 100, r = 50)
        )
        
        return(p_plotly)
      } else {
        # If no clustering, create a simple ggplot and convert to plotly
        p <- ggplot(plot_data, aes(x = dim1, y = dim2)) +
          geom_point(alpha = 0.7, size = point_size/2, color = "steelblue") +
          labs(
            title = paste("Control:", sample$name, "(No Clustering)"),
            x = paste(sample$dim_red_method, "1"),
            y = paste(sample$dim_red_method, "2")
          ) +
          get_standard_theme(font_size) +
          coord_fixed(ratio = 1) # Add fixed ratio for proper scaling
        
        # Convert to plotly with completely explicit font settings
        p_plotly <- ggplotly(p, tooltip = c("x", "y"), width = width, height = height)
        
        # Apply completely explicit font settings to ensure they're properly applied
        p_plotly <- p_plotly %>% layout(
          font = list(
            family = "Arial",
            size = font_size,
            color = "black"
          ),
          title = list(
            text = paste("Control:", sample$name, "(No Clustering)"),
            font = list(
              family = "Arial",
              size = font_size * 1.2,
              color = "black"
            )
          ),
          xaxis = list(
            title = list(
              text = paste(sample$dim_red_method, "1"),
              font = list(
                family = "Arial",
                size = font_size * 1.1,
                color = "black"
              )
            ),
            tickfont = list(
              family = "Arial",
              size = font_size
            ),
            scaleanchor = "y",
            scaleratio = 1
          ),
          yaxis = list(
            title = list(
              text = paste(sample$dim_red_method, "2"),
              font = list(
                family = "Arial",
                size = font_size * 1.1,
                color = "black"
              )
            ),
            tickfont = list(
              family = "Arial",
              size = font_size
            )
          ),
          hoverlabel = list(
            bgcolor = "white",
            font = list(
              family = "Arial",
              size = font_size * 0.9
            )
          ),
          margin = list(b = 80, l = 80, t = 100, r = 50)
        )
        
        return(p_plotly)
      }
    })
    
    output$treatedSamplePlot <- renderPlotly({
      # Explicitly track app_state$plot_settings to make this plot reactive to font changes
      font_size <- app_state$plot_settings$font_size
      point_size <- app_state$plot_settings$point_size
      color_palette <- app_state$plot_settings$color_palette
      width <- app_state$plot_settings$width
      height <- app_state$plot_settings$height
      
      req(input$compareViewTreated)
      results <- batchResults()
      if (is.null(results) || is.null(results[[input$compareViewTreated]])) {
        return(NULL)
      }
      
      sample <- results[[input$compareViewTreated]]
      plot_data <- sample$plot_data
      
      # If clustering was performed, color by cluster
      if (!is.null(sample$cluster_results) && "Cluster" %in% colnames(plot_data)) {
        # Determine what to use for color mapping - Population or Cluster
        color_by <- "Cluster"
        if ("Population" %in% colnames(plot_data)) {
          color_by <- "Population"
        }
        
        # Create base ggplot with correct colors
        p <- ggplot(plot_data, aes(x = dim1, y = dim2, color = .data[[color_by]])) +
          geom_point(alpha = 0.7, size = point_size/2) +
          get_color_palette(color_palette) +
          labs(
            title = paste("Treated:", sample$name),
            x = paste(sample$dim_red_method, "1"),
            y = paste(sample$dim_red_method, "2"),
            color = if(color_by == "Population") "Cell Population" else "Cluster"
          ) +
          get_standard_theme(font_size) +
          coord_fixed(ratio = 1) # Add fixed ratio for proper scaling
        
        # Convert to plotly with completely explicit font settings
        p_plotly <- ggplotly(p, tooltip = c("color", "x", "y"), width = width, height = height)
        
        # Apply completely explicit font settings to ensure they're properly applied
        p_plotly <- p_plotly %>% layout(
          font = list(
            family = "Arial",
            size = font_size,
            color = "black"
          ),
          title = list(
            text = paste("Treated:", sample$name),
            font = list(
              family = "Arial",
              size = font_size * 1.2,
              color = "black"
            )
          ),
          xaxis = list(
            title = list(
              text = paste(sample$dim_red_method, "1"),
              font = list(
                family = "Arial",
                size = font_size * 1.1,
                color = "black"
              )
            ),
            tickfont = list(
              family = "Arial",
              size = font_size
            ),
            scaleanchor = "y",
            scaleratio = 1
          ),
          yaxis = list(
            title = list(
              text = paste(sample$dim_red_method, "2"),
              font = list(
                family = "Arial",
                size = font_size * 1.1,
                color = "black"
              )
            ),
            tickfont = list(
              family = "Arial",
              size = font_size
            )
          ),
          legend = list(
            title = list(
              text = if(color_by == "Population") "Cell Population" else "Cluster",
              font = list(
                family = "Arial",
                size = font_size,
                color = "black"
              )
            ),
            font = list(
              family = "Arial",
              size = font_size * 0.9,
              color = "black"
            ),
            bgcolor = "rgba(255, 255, 255, 0.9)",
            bordercolor = "rgba(0, 0, 0, 0.2)",
            borderwidth = 1,
            x = 0.5,
            xanchor = "center",
            y = -0.15,
            yanchor = "top",
            orientation = "h"
          ),
          hoverlabel = list(
            bgcolor = "white",
            font = list(
              family = "Arial",
              size = font_size * 0.9
            )
          ),
          margin = list(b = 80, l = 80, t = 100, r = 50)
        )
        
        return(p_plotly)
      } else {
        # If no clustering, create a simple ggplot and convert to plotly
        p <- ggplot(plot_data, aes(x = dim1, y = dim2)) +
          geom_point(alpha = 0.7, size = point_size/2, color = "firebrick") +
          labs(
            title = paste("Treated:", sample$name, "(No Clustering)"),
            x = paste(sample$dim_red_method, "1"),
            y = paste(sample$dim_red_method, "2")
          ) +
          get_standard_theme(font_size) +
          coord_fixed(ratio = 1) # Add fixed ratio for proper scaling
        
        # Convert to plotly with completely explicit font settings
        p_plotly <- ggplotly(p, tooltip = c("x", "y"), width = width, height = height)
        
        # Apply completely explicit font settings to ensure they're properly applied
        p_plotly <- p_plotly %>% layout(
          font = list(
            family = "Arial",
            size = font_size,
            color = "black"
          ),
          title = list(
            text = paste("Treated:", sample$name, "(No Clustering)"),
            font = list(
              family = "Arial",
              size = font_size * 1.2,
              color = "black"
            )
          ),
          xaxis = list(
            title = list(
              text = paste(sample$dim_red_method, "1"),
              font = list(
                family = "Arial",
                size = font_size * 1.1,
                color = "black"
              )
            ),
            tickfont = list(
              family = "Arial",
              size = font_size
            ),
            scaleanchor = "y",
            scaleratio = 1
          ),
          yaxis = list(
            title = list(
              text = paste(sample$dim_red_method, "2"),
              font = list(
                family = "Arial",
                size = font_size * 1.1,
                color = "black"
              )
            ),
            tickfont = list(
              family = "Arial",
              size = font_size
            )
          ),
          hoverlabel = list(
            bgcolor = "white",
            font = list(
              family = "Arial",
              size = font_size * 0.9
            )
          ),
          margin = list(b = 80, l = 80, t = 100, r = 50)
        )
        
        return(p_plotly)
      }
    })
    
    # Cluster mapping heatmap for comparison
    output$clusterMappingHeatmap <- renderPlot({
      req(input$clusterCompareControl, input$clusterCompareTreated)
      results <- batchResults()
      
      if (is.null(results) || is.null(results[[input$clusterCompareControl]]) || 
          is.null(results[[input$clusterCompareTreated]])) {
        return(NULL)
      }
      
      # Get the samples
      control_sample <- results[[input$clusterCompareControl]]
      treated_sample <- results[[input$clusterCompareTreated]]
      
      # Check if clustering was performed for both samples
      if (is.null(control_sample$cluster_results) || is.null(treated_sample$cluster_results)) {
        return(NULL)
      }
      
      # Get population data - use merged populations if available
      current_merge_history <- mergeHistory()
      control_pops <- control_sample$populations
      treated_pops <- treated_sample$populations
      
      if (current_merge_history$active) {
        control_pops <- current_merge_history$current_mapping
        treated_pops <- current_merge_history$current_mapping
      }
      
      # Create comparison heatmap
      createClusterComparisonHeatmap(
        control_centers = control_sample$cluster_results$centers,
        treated_centers = treated_sample$cluster_results$centers,
        control_pops = control_pops,
        treated_pops = treated_pops,
        font_size = app_state$plot_settings$font_size
      )
    }, width = function() app_state$plot_settings$width,
    height = function() app_state$plot_settings$height)
    
    # Signature marker heatmap for comparison
    output$signatureMarkerHeatmap <- renderPlot({
      req(input$clusterCompareControl, input$clusterCompareTreated)
      results <- batchResults()
      
      if (is.null(results) || is.null(results[[input$clusterCompareControl]]) || 
          is.null(results[[input$clusterCompareTreated]])) {
        return(NULL)
      }
      
      # Get the samples
      control_sample <- results[[input$clusterCompareControl]]
      treated_sample <- results[[input$clusterCompareTreated]]
      
      # Check if clustering was performed for both samples
      if (is.null(control_sample$cluster_results) || is.null(treated_sample$cluster_results)) {
        return(NULL)
      }
      
      # Create signature marker heatmap
      createSignatureMarkerHeatmap(
        control_centers = control_sample$cluster_results$centers,
        treated_centers = treated_sample$cluster_results$centers,
        markers = input$batchSelectedMarkers,
        control_pops = control_sample$populations,
        treated_pops = treated_sample$populations,
        font_size = app_state$plot_settings$font_size,
        control_name = control_sample$name,
        treated_name = treated_sample$name
      )
    }, width = function() app_state$plot_settings$width,
    height = function() app_state$plot_settings$height)
    
    # Cluster merge modal
    observeEvent(input$showMergeModal, {
      req(input$viewSample)
      results <- batchResults()
      if (is.null(results) || is.null(results[[input$viewSample]])) {
        return(NULL)
      }
      
      sample <- results[[input$viewSample]]
      
      # Check if we have clusters
      if (is.null(sample$cluster_results)) {
        showNotification("No clustering results available for this sample", type = "error")
        return(NULL)
      }
      
      # Initialize merge history for this sample if not already done
      current_merge_history <- mergeHistory()
      if (!current_merge_history$active) {
        # Store original cluster IDs
        original_clusters <- sample$cluster_results$cluster_ids
        
        # Set up initial merge history
        mergeHistory(list(
          active = TRUE,
          current_clusters = original_clusters,
          original_clusters = original_clusters,
          current_mapping = if (!is.null(sample$populations)) sample$populations else data.frame(Cluster = unique(original_clusters), Population = paste0("Cluster ", unique(original_clusters))),
          operations = list()
        ))
      }
      
      # Get current clusters and populations
      current_merge_history <- mergeHistory()
      current_clusters <- unique(current_merge_history$current_clusters)
      
      # Create cluster choices
      if (!is.null(sample$populations)) {
        # Use population names if available
        populations <- current_merge_history$current_mapping
        cluster_choices <- setNames(
          as.character(populations$Cluster),
          paste0(populations$Cluster, ": ", populations$Population)
        )
      } else {
        # Just use cluster IDs
        cluster_choices <- setNames(
          as.character(current_clusters),
          paste0("Cluster ", current_clusters)
        )
      }
      
      # Show modal
      showModal(modalDialog(
        title = "Merge Similar Clusters",
        
        fluidRow(
          column(12,
            checkboxGroupInput(session$ns("clustersToMerge"),
                              "Select clusters to merge:",
                              choices = cluster_choices,
                              selected = NULL),
            textInput(session$ns("mergedClusterName"), "Name for merged cluster:",
                     value = "Merged Cell Population")
          )
        ),
        
        tags$div(class = "alert alert-info",
                "This merge will be added to any previous merges. Use 'Reset to Original Clusters' to start over."),
        
        footer = tagList(
          actionButton(session$ns("performMerge"), "Merge Clusters", 
                       class = "btn-primary"),
          modalButton("Cancel")
        ),
        
        size = "m",
        easyClose = TRUE
      ))
    })
    
    # Handle cluster merging
    observeEvent(input$performMerge, {
      req(input$clustersToMerge, length(input$clustersToMerge) >= 2)
      
      # Get current state (either original or already-merged clusters)
      current_merge_history <- mergeHistory()
      
      current_clusters <- current_merge_history$current_clusters
      new_clusters <- current_clusters
      
      # Get current mapping
      new_mapping <- current_merge_history$current_mapping
      
      # Get lowest cluster ID from selection (to use as the merged ID)
      merged_id <- min(as.numeric(input$clustersToMerge))
      
      # Change all selected clusters to the merged ID
      for (cluster_id in input$clustersToMerge) {
        new_clusters[current_clusters == cluster_id] <- merged_id
      }
      
      # Update name for the merged cluster
      new_mapping$Population[new_mapping$Cluster == merged_id] <- input$mergedClusterName
      
      # Remove rows for clusters that were merged (except the target)
      new_mapping <- new_mapping[!(new_mapping$Cluster %in% setdiff(input$clustersToMerge, merged_id)), ]
      
      # Add to merge history
      operations <- current_merge_history$operations
      operations[[length(operations) + 1]] <- list(
        timestamp = Sys.time(),
        merged_clusters = input$clustersToMerge,
        target_cluster = merged_id,
        new_name = input$mergedClusterName
      )
      
      # Update merge history
      mergeHistory(list(
        active = TRUE,
        current_clusters = new_clusters,
        original_clusters = current_merge_history$original_clusters,
        current_mapping = new_mapping,
        operations = operations
      ))
      
      # Close modal and show success notification
      removeModal()
      
      # Calculate remaining clusters and show notification
      remaining_clusters <- length(unique(new_clusters))
      total_merges <- length(operations)
      
      showNotification(
        paste0("Merged into '", input$mergedClusterName, "'. You now have ",
               remaining_clusters, " clusters (", total_merges, " merge operations)"),
        type = "message"
      )
      
      # Force re-render of visualizations
      session$sendCustomMessage(type = 'refreshPlots', message = list())
    })
    
    # Reset merging
    observeEvent(input$resetMerging, {
      req(input$viewSample)
      
      # Reset merge history
      mergeHistory(list(
        active = FALSE,
        current_clusters = NULL,
        original_clusters = NULL,
        current_mapping = NULL,
        operations = list()
      ))
      
      # Show notification
      showNotification("Reset to original clusters", type = "message")
      
      # Force re-render of visualizations
      session$sendCustomMessage(type = 'refreshPlots', message = list())
    })
    
    # Return reactive values that might be needed by other modules
    return(list(
      batch_samples = batchSamples,
      batch_results = batchResults
    ))
  })
}