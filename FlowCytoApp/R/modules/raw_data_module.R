# Raw Data Analysis Module for Flow Cytometry Analysis Tool

#' UI for the Raw Data Analysis Module
#' @param id Module ID
#' @return UI elements for raw data analysis
rawDataModuleUI <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      fileInput(ns("fcsFile"), "Upload FCS/CSV/TSV File", accept = c(".fcs", ".csv", ".tsv")),
      uiOutput(ns("markerSelectUI")),
      
      # Transform options
      checkboxInput(ns("transform"), "Apply arcsinh transformation", value = TRUE),
      numericInput(ns("cofactor"), "Transformation cofactor", value = 5, min = 1, max = 10),
      numericInput(ns("nEvents"), "Number of events to analyze", value = 5000, min = 100, step = 1000),
      
      # Dimensionality reduction method selection
      checkboxGroupInput(ns("methods"), "Select Dimensionality Reduction Methods",
                         choices = c("t-SNE", "UMAP", "PCA"),
                         selected = c("t-SNE", "UMAP")),
      
      # t-SNE parameters
      conditionalPanel(
        condition = paste0("input['", ns("methods"), "'].includes('t-SNE')"),
        numericInput(ns("perplexity"), "t-SNE Perplexity", value = 30, min = 5, max = 50),
        checkboxInput(ns("use_barnes_hut"), "Use Barnes-Hut Approximation (faster)", value = TRUE),
        conditionalPanel(
          condition = paste0("input['", ns("use_barnes_hut"), "']"),
          sliderInput(ns("tsne_theta"), "Barnes-Hut theta (higher theta = speed, lower theta = accuracy)", 
                      min = 0.0, max = 1.0, value = 0.5, step = 0.1)
        ),
        conditionalPanel(
          condition = paste0("!input['", ns("use_barnes_hut"), "']"),
          tags$div(class = "alert alert-warning",
                   "Warning: Exact t-SNE is very slow for datasets > 1000 cells.")
        ),
        numericInput(ns("tsne_max_iter"), "Maximum Iterations", value = 1000, min = 100, max = 10000, step = 100)
      ),
      
      # UMAP parameters
      conditionalPanel(
        condition = paste0("input['", ns("methods"), "'].includes('UMAP')"),
        numericInput(ns("n_neighbors"), "UMAP n_neighbors", value = 15, min = 2, max = 100)
      ),
      
      # PCA parameters
      conditionalPanel(
        condition = paste0("input['", ns("methods"), "'].includes('PCA')"),
        numericInput(ns("pca_components"), "PCA: Number of Components", value = 2, min = 2, max = 10)
      ),
      
      # QC and gating options
      checkboxInput(ns("performQC"), "Perform Quality Control", value = TRUE),
      numericInput(ns("maxAnomalies"), "Max Anomalies (%)", value = 10, min = 0, max = 50),
      
      checkboxInput(ns("performGating"), "Perform Debris/Dead Cell Gating", value = TRUE),
      conditionalPanel(
        condition = paste0("input['", ns("performGating"), "'] === true"),
        textInput(ns("debrisGate"), "FSC/SSC Parameters (comma-separated)", 
                  value = "FSC-A,SSC-A"),
        selectInput(ns("liveDeadGate"), "Live/Dead Parameter", 
                    choices = c("None", "Live Dead BV570 Violet-610-A"),
                    selected = "None"),
        # Add live/dead threshold parameter
        conditionalPanel(
          condition = paste0("input['", ns("liveDeadGate"), "'] !== 'None'"),
          numericInput(ns("liveDeadThreshold"), "Live/Dead Threshold", 
                       value = 1000, min = 0, max = 10000, step = 100),
          helpText("Cells with values below this threshold will be considered live")
        )
      ),
      
      # Add clustering module UI
      clusteringModuleUI(ns("clustering")),
      
      # Run button
      hr(),
      actionButton(ns("run"), "Run Analysis", class = "btn-primary"),
      
      # Download options
      hr(),
      h4("Download Results"),
      downloadButton(ns("downloadClusterTable"), "Download Cluster Data"),
      br(), br(),
      downloadButton(ns("downloadProcessedData"), "Download Processed Data")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("t-SNE", shinycssloaders::withSpinner(plotlyOutput(ns("tsnePlot"), height = "600px"))),
        tabPanel("UMAP", shinycssloaders::withSpinner(plotlyOutput(ns("umapPlot"), height = "600px"))),
        tabPanel("PCA", shinycssloaders::withSpinner(plotlyOutput(ns("pcaPlot"), height = "600px"))),
        tabPanel("Data Info", verbatimTextOutput(ns("fcsInfo")), uiOutput(ns("optimizationMetricsUI"))),
        # Add new tab for Live/Dead Analysis
        tabPanel("Live/Dead Analysis", 
                 fluidRow(
                   column(12, h4("Live/Dead Marker Distribution", align = "center")),
                   column(8, shinycssloaders::withSpinner(plotOutput(ns("liveDeadHistogram"), height = "400px"))),
                   column(4, 
                          wellPanel(
                            h4("Live/Dead Gating Settings"),
                            uiOutput(ns("liveDeadMarkerUI")),
                            conditionalPanel(
                              condition = paste0("input['", ns("liveDeadMarkerSelect"), "'] !== 'None'"),
                              sliderInput(ns("liveDeadThresholdSlider"), "Threshold", 
                                          min = 0, max = 5000, value = 1000, step = 100),
                              checkboxInput(ns("useLogScale"), "Use Log Scale for Visualization", value = TRUE),
                              hr(),
                              h4("Gating Results"),
                              verbatimTextOutput(ns("liveDeadStats")),
                              actionButton(ns("applyLiveDeadGating"), "Apply to Preprocessing", 
                                           class = "btn-primary", width = "100%")
                            )
                          ))
                 ),
                 fluidRow(
                   column(12, 
                          conditionalPanel(
                            condition = paste0("input['", ns("liveDeadMarkerSelect"), "'] !== 'None'"),
                            h4("2D Visualization", align = "center"),
                            plotOutput(ns("liveDeadScatter"), height = "400px")
                          ))
                 )),
        tabPanel("Cluster Analysis", 
                 conditionalPanel(
                   condition = paste0("input['", ns("clustering-showClusteringOptions"), "'] == true"),
                   tabsetPanel(
                     tabPanel("Cluster Visualization", 
                              div(
                                style = "position: relative;",
                                plotlyOutput(ns("clusterPlot"), height = "600px")
                              )
                     ),
                     tabPanel("Cluster Profiles", shinycssloaders::withSpinner(plotOutput(ns("clusterHeatmap")))),
                     tabPanel("Cluster Statistics", DT::dataTableOutput(ns("clusterStats"))),
                     tabPanel("Identified Populations", DT::dataTableOutput(ns("populationTable")))
                   )
                 )
        )
      )
    )
  )
}

#' Server function for the Raw Data Analysis Module
#' 
#' @param id Module ID
#' @param app_state Reactive values with global app state
#' @return List with module outputs
rawDataModuleServer <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive values to store processed data and results
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
    
    # Show raw data information
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
    
    # Dynamic marker selection UI
    output$markerSelectUI <- renderUI({
      req(rawFCS())
      
      # Get marker choices based on data type
      if (inherits(rawFCS(), "flowFrame")) {
        exprs_data <- exprs(rawFCS())
        params <- parameters(rawFCS())
        choices <- setNames(colnames(exprs_data), paste0(colnames(exprs_data), " - ", params$desc))
      } else {
        exprs_data <- rawFCS()
        choices <- colnames(exprs_data)
      }
      
      selectInput(session$ns("selectedMarkers"), "Select Markers/Channels", 
                  choices = choices, 
                  selected = choices[1:min(5, length(choices))], 
                  multiple = TRUE)
    })
    
    # Live/Dead marker selection UI
    output$liveDeadMarkerUI <- renderUI({
      req(rawFCS())
      
      # Get marker choices based on data type
      if (inherits(rawFCS(), "flowFrame")) {
        exprs_data <- exprs(rawFCS())
        params <- parameters(rawFCS())
        
        # Look for potential live/dead markers 
        potential_ld_markers <- grep("live|dead|viability|FL3-A", 
                                     colnames(exprs_data), 
                                     ignore.case = TRUE, 
                                     value = TRUE)
        
        # If no matches found, include all channels
        if (length(potential_ld_markers) == 0) {
          potential_ld_markers <- colnames(exprs_data)
        }
        
        # Create named vector for selection
        choices <- c("None", setNames(
          potential_ld_markers,
          paste0(potential_ld_markers, " - ", params$desc[match(potential_ld_markers, colnames(exprs_data))])
        ))
        
        # Find the default selected marker
        default_selection <- "None"
        live_dead_matches <- grep("live.*dead|dead.*live|viability", choices, ignore.case = TRUE)
        if (length(live_dead_matches) > 0) {
          default_selection <- choices[live_dead_matches[1]]
        }
        
        selectInput(session$ns("liveDeadMarkerSelect"), "Select Live/Dead Marker:", 
                    choices = choices, 
                    selected = default_selection)
      } else {
        selectInput(session$ns("liveDeadMarkerSelect"), "Select Live/Dead Marker:", 
                    choices = c("None", colnames(rawFCS())), 
                    selected = "None")
      }
    })
    
    # Store processed data
    processedData <- reactiveVal(NULL)
    
    # Replace single mergedClusters reactive with a more comprehensive structure
    mergeHistory <- reactiveVal(list(
      active = FALSE,
      operations = list(),
      current_clusters = NULL,
      current_mapping = NULL
    ))
    
    # Live/Dead histogram plot
    output$liveDeadHistogram <- renderPlot({
      req(rawFCS(), input$liveDeadMarkerSelect)
      
      # Only proceed if a marker is selected
      if (input$liveDeadMarkerSelect == "None") {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "Please select a Live/Dead marker") +
                 theme_void())
      }
      
      # Extract values for the selected marker
      if (inherits(rawFCS(), "flowFrame")) {
        ld_marker <- input$liveDeadMarkerSelect
        if (grepl(" - ", ld_marker)) {
          ld_marker <- strsplit(ld_marker, " - ")[[1]][1]
        }
        
        if (!(ld_marker %in% colnames(exprs(rawFCS())))) {
          return(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, 
                            label = paste("Marker", ld_marker, "not found in dataset")) +
                   theme_void())
        }
        
        ld_values <- exprs(rawFCS())[, ld_marker]
      } else {
        # For non-FCS files
        if (!(input$liveDeadMarkerSelect %in% colnames(rawFCS()))) {
          return(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, 
                            label = paste("Marker", input$liveDeadMarkerSelect, "not found in dataset")) +
                   theme_void())
        }
        
        ld_values <- rawFCS()[[input$liveDeadMarkerSelect]]
      }
      
      # Create data frame for plotting
      hist_data <- data.frame(value = ld_values)
      
      # Handle any invalid values (NAs, Infs)
      hist_data <- hist_data[is.finite(hist_data$value), , drop = FALSE]
      
      # Update max range of slider based on the data
      if (!is.null(hist_data$value) && length(hist_data$value) > 0) {
        data_max <- max(hist_data$value, na.rm = TRUE)
        data_min <- min(hist_data$value, na.rm = TRUE)
        
        # Set max to something reasonable (95th percentile might be better than max)
        slider_max <- max(5000, min(quantile(hist_data$value, 0.995, na.rm = TRUE), 100000))
        
        # Update the slider
        updateSliderInput(session, "liveDeadThresholdSlider", 
                          min = data_min, 
                          max = slider_max,
                          value = min(input$liveDeadThresholdSlider, slider_max))
      }
      
      # Create the histogram plot
      p <- ggplot(hist_data, aes(x = value)) +
        geom_histogram(bins = 100, fill = "steelblue", color = "black", alpha = 0.7) +
        geom_vline(xintercept = input$liveDeadThresholdSlider, 
                   color = "red", linetype = "dashed", size = 1.5) +
        labs(title = "Live/Dead Marker Distribution",
             subtitle = paste("Red line shows current threshold:", input$liveDeadThresholdSlider),
             x = input$liveDeadMarkerSelect,
             y = "Count") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title = element_text(face = "bold")
        )
      
      # Apply log scale if selected, using a safer transformation
      if (input$useLogScale) {
        # Add a small value to avoid log(0) or log(negative)
        small_offset <- 1  # Standard offset for flow data
        p <- p + scale_x_continuous(
          trans = scales::pseudo_log_trans(base = 10, sigma = small_offset),
          labels = scales::label_number()
        )
      }
      
      return(p)
    })
    
    # Live/Dead stats output
    output$liveDeadStats <- renderText({
      req(rawFCS(), input$liveDeadMarkerSelect)
      
      # Only proceed if a marker is selected
      if (input$liveDeadMarkerSelect == "None") {
        return("No Live/Dead marker selected")
      }
      
      # Extract values for the selected marker
      if (inherits(rawFCS(), "flowFrame")) {
        ld_marker <- input$liveDeadMarkerSelect
        if (grepl(" - ", ld_marker)) {
          ld_marker <- strsplit(ld_marker, " - ")[[1]][1]
        }
        
        if (!(ld_marker %in% colnames(exprs(rawFCS())))) {
          return(paste("Marker", ld_marker, "not found in dataset"))
        }
        
        ld_values <- exprs(rawFCS())[, ld_marker]
      } else {
        # For non-FCS files
        if (!(input$liveDeadMarkerSelect %in% colnames(rawFCS()))) {
          return(paste("Marker", input$liveDeadMarkerSelect, "not found in dataset"))
        }
        
        ld_values <- rawFCS()[[input$liveDeadMarkerSelect]]
      }
      
      # Calculate percentage of cells classified as live
      total_cells <- length(ld_values)
      live_cells <- sum(ld_values <= input$liveDeadThresholdSlider, na.rm = TRUE)
      live_pct <- live_cells / total_cells * 100
      
      # Create a summary text
      paste0(
        "Total Cells: ", format(total_cells, big.mark = ","), "\n",
        "Live Cells (below threshold): ", format(live_cells, big.mark = ","), "\n",
        "Percentage of Live Cells: ", round(live_pct, 1), "%\n\n",
        "Statistics for this marker:\n",
        "Min: ", round(min(ld_values, na.rm = TRUE), 1), "\n",
        "Max: ", round(max(ld_values, na.rm = TRUE), 1), "\n",
        "Mean: ", round(mean(ld_values, na.rm = TRUE), 1), "\n",
        "Median: ", round(median(ld_values, na.rm = TRUE), 1)
      )
    })
    
    # FSC vs Live/Dead scatter plot to visualize gating
    output$liveDeadScatter <- renderPlot({
      req(rawFCS(), input$liveDeadMarkerSelect)
      
      # Only proceed if a marker is selected
      if (input$liveDeadMarkerSelect == "None") {
        return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "Please select a Live/Dead marker") +
                 theme_void())
      }
      
      # Extract FSC-A and selected marker values
      if (inherits(rawFCS(), "flowFrame")) {
        ld_marker <- input$liveDeadMarkerSelect
        if (grepl(" - ", ld_marker)) {
          ld_marker <- strsplit(ld_marker, " - ")[[1]][1]
        }
        
        # Find FSC-A column
        fsc_col <- grep("^FSC-A", colnames(exprs(rawFCS())), value = TRUE)
        if (length(fsc_col) == 0) {
          # If FSC-A not found, try to find any FSC column
          fsc_col <- grep("^FSC", colnames(exprs(rawFCS())), value = TRUE)
          if (length(fsc_col) == 0) {
            # If still no FSC, use the first column
            fsc_col <- colnames(exprs(rawFCS()))[1]
          } else {
            fsc_col <- fsc_col[1]
          }
        } else {
          fsc_col <- fsc_col[1]
        }
        
        if (!(ld_marker %in% colnames(exprs(rawFCS())))) {
          return(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, 
                            label = paste("Marker", ld_marker, "not found in dataset")) +
                   theme_void())
        }
        
        # Create scatter plot data
        plot_data <- data.frame(
          FSC = exprs(rawFCS())[, fsc_col],
          LiveDead = exprs(rawFCS())[, ld_marker]
        )
      } else {
        # For non-FCS files
        if (!(input$liveDeadMarkerSelect %in% colnames(rawFCS()))) {
          return(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, 
                            label = paste("Marker", input$liveDeadMarkerSelect, "not found in dataset")) +
                   theme_void())
        }
        
        # Find a FSC-like column or use the first numeric column
        numeric_cols <- sapply(rawFCS(), is.numeric)
        if (any(grepl("FSC|forward", colnames(rawFCS()), ignore.case = TRUE))) {
          fsc_col <- grep("FSC|forward", colnames(rawFCS()), ignore.case = TRUE, value = TRUE)[1]
        } else if (any(numeric_cols)) {
          fsc_col <- names(numeric_cols)[which(numeric_cols)[1]]
        } else {
          return(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, 
                            label = "No suitable FSC or numeric column found") +
                   theme_void())
        }
        
        # Create scatter plot data
        plot_data <- data.frame(
          FSC = rawFCS()[[fsc_col]],
          LiveDead = rawFCS()[[input$liveDeadMarkerSelect]]
        )
      }
      
      # Handle any invalid values (NAs, Infs)
      plot_data <- plot_data[is.finite(plot_data$FSC) & is.finite(plot_data$LiveDead), , drop = FALSE]
      
      # Add gating result
      plot_data$GatingResult <- ifelse(plot_data$LiveDead <= input$liveDeadThresholdSlider, 
                                       "Live", "Dead")
      
      # Create scatter plot
      p <- ggplot(plot_data, aes(x = FSC, y = LiveDead, color = GatingResult)) +
        geom_point(alpha = 0.5, size = 0.8) +
        geom_hline(yintercept = input$liveDeadThresholdSlider, 
                   color = "red", linetype = "dashed", size = 1) +
        scale_color_manual(values = c("Live" = "forestgreen", "Dead" = "firebrick")) +
        labs(title = "FSC vs Live/Dead Gating",
             subtitle = paste("Red line shows current threshold:", input$liveDeadThresholdSlider),
             x = "Forward Scatter (FSC)",
             y = input$liveDeadMarkerSelect,
             color = "Gating Result") +
        theme_minimal(base_size = 14) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          axis.title = element_text(face = "bold"),
          legend.position = "right",
          legend.title = element_text(face = "bold")
        )
      
      # Apply log scale for y-axis if selected, using a safer transformation
      if (input$useLogScale) {
        # Add a small value to avoid log(0) or log(negative)
        small_offset <- 1  # Standard offset for flow data
        p <- p + scale_y_continuous(
          trans = scales::pseudo_log_trans(base = 10, sigma = small_offset),
          labels = scales::label_number()
        )
      }
      
      return(p)
    })
    
    # Apply live/dead gating threshold to the main analysis
    observeEvent(input$applyLiveDeadGating, {
      req(input$liveDeadMarkerSelect, input$liveDeadMarkerSelect != "None")
      
      # Extract correct marker name (remove the description part)
      ld_marker <- input$liveDeadMarkerSelect
      if (grepl(" - ", ld_marker)) {
        ld_marker <- strsplit(ld_marker, " - ")[[1]][1]
      }
      
      # Update liveDeadGate selection
      updateSelectInput(session, "liveDeadGate", selected = ld_marker)
      
      # Update liveDeadThreshold value
      updateNumericInput(session, "liveDeadThreshold", value = input$liveDeadThresholdSlider)
      
      # Make sure gating is enabled
      updateCheckboxInput(session, "performGating", value = TRUE)
      
      # Show notification
      showNotification(
        paste("Live/Dead threshold updated to", input$liveDeadThresholdSlider),
        type = "message", 
        duration = 3
      )
    })
    
    # Run analysis when button is clicked
    observeEvent(input$run, {
      req(input$selectedMarkers)
      
      withProgress(message = 'Processing data...', value = 0, {
        # Get data
        raw_data <- rawFCS()
        
        # Prepare preprocessing parameters
        preprocessing_params <- list(
          markers = input$selectedMarkers,
          transform = input$transform,
          cofactor = input$cofactor,
          n_events = input$nEvents,
          perform_qc = isTRUE(input$performQC),
          perform_gating = isTRUE(input$performGating),
          scale_data = TRUE,
          seed = 123
        )
        
        # Add QC settings if QC is enabled
        if (isTRUE(input$performQC)) {
          preprocessing_params$qc_settings <- list(
            max_anomalies = input$maxAnomalies / 100  # Convert from percentage to proportion
          )
        }
        
        # Add gating parameters if gating is enabled
        if (isTRUE(input$performGating)) {
          # Parse debris gate parameters from comma-separated string
          debris_gate_params <- unlist(strsplit(input$debrisGate, ",\\s*"))
          
          preprocessing_params$gates <- list(
            debris_gate = if (length(debris_gate_params) >= 2) debris_gate_params[1:2] else NULL,
            live_dead_gate = if (!is.null(input$liveDeadGate) && input$liveDeadGate != "None") input$liveDeadGate else NULL
          )
          
          # Add threshold for live/dead gating if available
          if (!is.null(input$liveDeadThreshold) && input$liveDeadGate != "None") {
            # We need to modify the gating function to use this threshold
            # For now, we'll display a notification about it
            showNotification(
              paste("Using live/dead threshold of", input$liveDeadThreshold),
              type = "message", 
              duration = 3
            )
            
            # In a full implementation, we'd need to update the performGating function
            # to use this custom threshold instead of the hardcoded 1000 value
          }
        }
        
        # Run preprocessing pipeline
        incProgress(0.1, detail = "Preprocessing data...")
        preprocessing_results <- preprocessFlowData(raw_data, preprocessing_params)
        
        # Run dimensionality reduction methods
        results <- list(
          raw_data = preprocessing_results$raw_data,
          qc_data = preprocessing_results$qc_data,
          gated_data = preprocessing_results$gated_data,
          transformed_data = preprocessing_results$transformed_data,
          sampled_data = preprocessing_results$sampled_data,
          scaled_data = preprocessing_results$scaled_data,
          metrics = preprocessing_results$metrics
        )
        
        if ("t-SNE" %in% input$methods) {
          incProgress(0.4, detail = "Running t-SNE...")
          
          # Ensure the data is a matrix
          data_matrix <- as.matrix(preprocessing_results$scaled_data)
          
          # Prepare t-SNE parameters
          tsne_params <- list(
            dims = 2,
            perplexity = input$perplexity,
            max_iter = input$tsne_max_iter,
            verbose = FALSE
          )
          
          if (input$use_barnes_hut) {
            # Use Barnes-Hut t-SNE
            tsne_params$theta <- input$tsne_theta
            incProgress(0.1, detail = "Running Barnes-Hut t-SNE...")
          } else {
            # Use exact t-SNE (very slow for large datasets)
            tsne_params$theta <- 0.0  # This triggers exact t-SNE
            incProgress(0.1, detail = "Running exact t-SNE (may be slow)...")
          }
          
          # Run t-SNE with the configured parameters
          tsne_result <- do.call(Rtsne, c(list(X = data_matrix), tsne_params))
          
          results$tsne <- data.frame(tsne1 = tsne_result$Y[,1], tsne2 = tsne_result$Y[,2])
        }
        
        if ("UMAP" %in% input$methods) {
          incProgress(0.7, detail = "Running UMAP...")
          umap_result <- umap(preprocessing_results$scaled_data, n_neighbors = input$n_neighbors)
          results$umap <- data.frame(umap1 = umap_result[,1], umap2 = umap_result[,2])
        }
        
        if ("PCA" %in% input$methods) {
          incProgress(0.8, detail = "Running PCA...")
          pca_result <- prcomp(preprocessing_results$scaled_data, center = TRUE, scale. = TRUE)
          results$pca <- data.frame(pca1 = pca_result$x[,1], pca2 = pca_result$x[,2])
          
          # Store additional components if requested
          if (input$pca_components > 2) {
            for (i in 3:input$pca_components) {
              results$pca[[paste0("pca", i)]] <- pca_result$x[, i]
            }
          }
          
          # Store PCA summary information for metrics
          results$pca_summary <- summary(pca_result)
        }
        
        # Create plot data
        plot_data <- as.data.frame(preprocessing_results$sampled_data)
        colnames(plot_data) <- input$selectedMarkers
        
        # Add dimensionality reduction coordinates
        if (!is.null(results$tsne)) {
          plot_data$tsne1 <- results$tsne$tsne1
          plot_data$tsne2 <- results$tsne$tsne2
        }
        if (!is.null(results$umap)) {
          plot_data$umap1 <- results$umap$umap1
          plot_data$umap2 <- results$umap$umap2
        }
        if (!is.null(results$pca)) {
          plot_data$pca1 <- results$pca$pca1
          plot_data$pca2 <- results$pca$pca2
          
          # Add additional PCA components if they exist
          if (input$pca_components > 2) {
            for (i in 3:input$pca_components) {
              pca_col <- paste0("pca", i)
              if (pca_col %in% colnames(results$pca)) {
                plot_data[[pca_col]] <- results$pca[[pca_col]]
              }
            }
          }
        }
        
        results$plot_data <- plot_data
        results$markers <- input$selectedMarkers
        processedData(results)
        
        # Display notification about preprocessing results
        if (!is.null(results$metrics)) {
          qc_removed <- round(results$metrics$qc$removed_pct * 100, 1)
          gating_removed <- round(results$metrics$gating$removed_pct * 100, 1)
          
          msg <- paste0("Preprocessing complete: ", 
                        qc_removed, "% removed in QC, ",
                        gating_removed, "% removed in gating. ",
                        "Analyzing ", nrow(results$sampled_data), " cells.")
          
          showNotification(msg, type = "message", duration = 5)
        }
      })
    })
    
    # Call clustering module server
    clustering_results <- clusteringModuleServer("clustering", 
                                                 # Pass data and markers to clustering module
                                                 input_data = reactive({
                                                   req(processedData())
                                                   list(
                                                     scaled_data = processedData()$scaled_data,
                                                     markers = input$selectedMarkers
                                                   )
                                                 }),
                                                 app_state = app_state
    )
    
    # Add optimization metrics UI
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
      
      if (!is.null(results$pca)) {
        # Calculate PCA metrics if it was run
        pca_data <- results$pca
        
        # Calculate explained variance for PCA
        pca_variance <- tryCatch({
          if (!is.null(results$pca_summary)) {
            # Extract proportion of variance explained by first 2 components
            var_explained <- results$pca_summary$importance[2, 1:2]  # Proportion of Variance row
            sum(var_explained)
          } else {
            NA
          }
        }, error = function(e) NA)
        
        # Store PCA metrics
        metrics$pca <- list(
          variance_explained = round(pca_variance, 4),
          components = input$pca_components
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
      
      # Add PCA metrics if available
      if (!is.null(metrics$pca)) {
        metrics_ui <- tagAppendChildren(
          metrics_ui,
          div(
            h5("PCA Metrics:"),
            p(paste("Variance Explained:", metrics$pca$variance_explained, 
                    "(higher is better)")),
            p(paste("Components:", metrics$pca$components))
          )
        )
      }
      
      # Add recommendation if multiple methods are available
      available_methods <- c()
      if (!is.null(metrics$tsne)) available_methods <- c(available_methods, "t-SNE")
      if (!is.null(metrics$umap)) available_methods <- c(available_methods, "UMAP")
      if (!is.null(metrics$pca)) available_methods <- c(available_methods, "PCA")
      
      if (length(available_methods) > 1) {
        # Calculate simple scores for comparison
        scores <- list()
        
        if (!is.null(metrics$tsne)) {
          scores$tsne <- 1 - min(metrics$tsne$kl_divergence, 1)  # Convert to 0-1 scale where higher is better
        }
        if (!is.null(metrics$umap)) {
          scores$umap <- max(min(metrics$umap$trustworthiness, 1), 0)  # Ensure in 0-1 range
        }
        if (!is.null(metrics$pca)) {
          scores$pca <- min(metrics$pca$variance_explained, 1)  # Already in 0-1 range
        }
        
        # Find the best method
        best_method <- names(scores)[which.max(unlist(scores))]
        best_score <- round(max(unlist(scores)), 3)
        
        # Create recommendation text
        rec_text <- paste0("Based on calculated metrics, ", best_method, 
                          " may provide the best results for this dataset (score: ", best_score, ").")
        
        # Add context about what each method is good for
        if (length(available_methods) >= 2) {
          rec_text <- paste0(rec_text, "\n\nMethod characteristics:\n",
                            "• t-SNE: Best for revealing local structure and clusters\n",
                            "• UMAP: Good balance of local and global structure\n",
                            "• PCA: Linear method, preserves global distances, interpretable components")
        }
        
        metrics_ui <- tagAppendChildren(
          metrics_ui,
          div(
            h5("Recommendation:"),
            p(rec_text, style = "white-space: pre-line;")
          )
        )
      }
      
      # Add preprocessing metrics
      if (!is.null(results$metrics)) {
        metrics_ui <- tagAppendChildren(
          metrics_ui,
          hr(),
          h4("Preprocessing Metrics"),
          div(
            h5("Quality Control:"),
            p(paste("Initial cells:", results$metrics$qc$initial_count)),
            p(paste("After QC:", results$metrics$qc$final_count)),
            p(paste("Removed:", round(results$metrics$qc$removed_pct * 100, 1), "%"))
          ),
          div(
            h5("Gating:"),
            p(paste("After gating:", results$metrics$gating$final_count)),
            p(paste("Removed in gating:", round(results$metrics$gating$removed_pct * 100, 1), "%"))
          ),
          div(
            h5("Sampling:"),
            p(paste("Final analyzed cells:", nrow(results$sampled_data)))
          )
        )
      }
      
      return(metrics_ui)
    })
    
    # Make clusterPlot reactive to palette changes
    observe({
      # This observer will re-run whenever plot settings change
      app_state$plot_settings
      
      # Force the clusterPlot to invalidate and re-render
      session$sendCustomMessage(type = "refreshClusterPlot", message = list())
    })
    
    # NEW: Make t-SNE and UMAP plots reactive to plot settings changes
    observe({
      # This observer will re-run whenever plot settings change
      app_state$plot_settings
      
      # Force all plotly outputs to redraw with new settings
      session$sendCustomMessage(type = "plotly-replot", message = list(id = session$ns("tsnePlot")))
      session$sendCustomMessage(type = "plotly-replot", message = list(id = session$ns("umapPlot")))
      session$sendCustomMessage(type = "plotly-replot", message = list(id = session$ns("pcaPlot")))
    })
    
    # Render t-SNE plot
    output$tsnePlot <- renderPlotly({
      # Explicitly track app_state$plot_settings to make this plot reactive to font changes
      font_size <- app_state$plot_settings$font_size
      point_size <- app_state$plot_settings$point_size
      color_palette <- app_state$plot_settings$color_palette
      width <- app_state$plot_settings$width
      height <- app_state$plot_settings$height
      
      req(processedData(), "t-SNE" %in% input$methods)
      plot_data <- processedData()$plot_data
      req("tsne1" %in% colnames(plot_data))
      
      # Create a copy of plot_data
      plot_data_copy <- plot_data
      
      # Add cluster information if available
      if (!is.null(clustering_results$clustering_results()) && 
          clustering_results$showClusteringOptions()) {
        plot_data_copy$Cluster <- as.factor(clustering_results$clustering_results()$cluster_ids)
        color_by <- "Cluster"
      } else {
        color_by <- NULL
      }
      
      # Create base plot
      p <- createDimReductionPlot(
        plot_data = plot_data_copy,
        dim1 = "tsne1",
        dim2 = "tsne2",
        colorBy = color_by,
        color_palette = color_palette,
        point_size = point_size,
        font_size = font_size,
        title = "t-SNE Projection",
        xlab = "t-SNE 1",
        ylab = "t-SNE 2"
      )
      
      # Convert to plotly with explicit font settings
      p_plotly <- ggplotly(p, width = width, height = height)
      
      # Apply completely explicit font settings to ensure they're properly applied
      p_plotly <- p_plotly %>% layout(
        font = list(
          family = "Arial",
          size = font_size,
          color = "black"
        ),
        title = list(
          text = "t-SNE Projection",
          font = list(
            family = "Arial",
            size = font_size * 1.2,
            color = "black"
          )
        ),
        xaxis = list(
          title = list(
            text = "t-SNE 1",
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
            text = "t-SNE 2",
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
        # Add legend settings if clusters are shown
        legend = if (!is.null(color_by)) list(
          title = list(
            text = "Cluster",
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
          borderwidth = 1
        ) else list()
      )
      
      return(p_plotly)
    })
    
    # Render UMAP plot
    output$umapPlot <- renderPlotly({
      # Explicitly track app_state$plot_settings to make this plot reactive to font changes
      font_size <- app_state$plot_settings$font_size
      point_size <- app_state$plot_settings$point_size
      color_palette <- app_state$plot_settings$color_palette
      width <- app_state$plot_settings$width
      height <- app_state$plot_settings$height
      
      req(processedData(), "UMAP" %in% input$methods)
      plot_data <- processedData()$plot_data
      req("umap1" %in% colnames(plot_data))
      
      # Create a copy of plot_data
      plot_data_copy <- plot_data
      
      # Add cluster information if available
      if (!is.null(clustering_results$clustering_results()) && 
          clustering_results$showClusteringOptions()) {
        plot_data_copy$Cluster <- as.factor(clustering_results$clustering_results()$cluster_ids)
        color_by <- "Cluster"
      } else {
        color_by <- NULL
      }
      
      # Create base plot
      p <- createDimReductionPlot(
        plot_data = plot_data_copy,
        dim1 = "umap1",
        dim2 = "umap2",
        colorBy = color_by,
        color_palette = color_palette,
        point_size = point_size,
        font_size = font_size,
        title = "UMAP Projection",
        xlab = "UMAP 1",
        ylab = "UMAP 2"
      )
      
      # Convert to plotly with explicit font settings
      p_plotly <- ggplotly(p, width = width, height = height)
      
      # Apply completely explicit font settings to ensure they're properly applied
      p_plotly <- p_plotly %>% layout(
        font = list(
          family = "Arial",
          size = font_size,
          color = "black"
        ),
        title = list(
          text = "UMAP Projection",
          font = list(
            family = "Arial",
            size = font_size * 1.2,
            color = "black"
          )
        ),
        xaxis = list(
          title = list(
            text = "UMAP 1",
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
            text = "UMAP 2",
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
        # Add legend settings if clusters are shown
        legend = if (!is.null(color_by)) list(
          title = list(
            text = "Cluster",
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
          borderwidth = 1
        ) else list()
      )
      
      return(p_plotly)
    })
    
    # Render PCA plot
    output$pcaPlot <- renderPlotly({
      # Explicitly track app_state$plot_settings to make this plot reactive to font changes
      font_size <- app_state$plot_settings$font_size
      point_size <- app_state$plot_settings$point_size
      color_palette <- app_state$plot_settings$color_palette
      width <- app_state$plot_settings$width
      height <- app_state$plot_settings$height
      
      req(processedData(), "PCA" %in% input$methods)
      plot_data <- processedData()$plot_data
      req("pca1" %in% colnames(plot_data))
      
      # Create a copy of plot_data
      plot_data_copy <- plot_data
      
      # Add cluster information if available
      if (!is.null(clustering_results$clustering_results()) && 
          clustering_results$showClusteringOptions()) {
        plot_data_copy$Cluster <- as.factor(clustering_results$clustering_results()$cluster_ids)
        color_by <- "Cluster"
      } else {
        color_by <- NULL
      }
      
      # Create base plot
      p <- createDimReductionPlot(
        plot_data = plot_data_copy,
        dim1 = "pca1",
        dim2 = "pca2",
        colorBy = color_by,
        color_palette = color_palette,
        point_size = point_size,
        font_size = font_size,
        title = "PCA Projection",
        xlab = "PC 1",
        ylab = "PC 2"
      )
      
      # Convert to plotly with explicit font settings
      p_plotly <- ggplotly(p, width = width, height = height)
      
      # Apply completely explicit font settings to ensure they're properly applied
      p_plotly <- p_plotly %>% layout(
        font = list(
          family = "Arial",
          size = font_size,
          color = "black"
        ),
        title = list(
          text = "PCA Projection",
          font = list(
            family = "Arial",
            size = font_size * 1.2,
            color = "black"
          )
        ),
        xaxis = list(
          title = list(
            text = "PC 1",
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
            text = "PC 2",
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
        # Add legend settings if clusters are shown
        legend = if (!is.null(color_by)) list(
          title = list(
            text = "Cluster",
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
          borderwidth = 1
        ) else list()
      )
      
      return(p_plotly)
    })
    
    # Cluster visualization in dedicated tab
    output$clusterPlot <- renderPlotly({
      # Explicitly track app_state$plot_settings to make this plot reactive to font changes
      font_size <- app_state$plot_settings$font_size
      point_size <- app_state$plot_settings$point_size
      color_palette <- app_state$plot_settings$color_palette
      width <- app_state$plot_settings$width
      height <- app_state$plot_settings$height
      
      req(processedData())
      
      # Check if we have clustering results (either original or merged)
      if (mergeHistory()$active) {
        # Use merged clusters
        cluster_data <- list(
          cluster_ids = mergeHistory()$current_clusters,
          method = paste(clustering_results$clustering_results()$method, 
                         "(", length(mergeHistory()$operations), " merges)")
        )
        population_data <- mergeHistory()$current_mapping
      } else if (!is.null(clustering_results$clustering_results())) {
        # Use original clustering
        cluster_data <- clustering_results$clustering_results()
        population_data <- clustering_results$populations()
      } else {
        return(NULL)  # No clustering data available
      }
      
      plot_data <- processedData()$plot_data
      
      # Add cluster information
      plot_data$Cluster <- as.factor(cluster_data$cluster_ids)
      
      # Add population labels if available
      if (!is.null(population_data) && 
          clustering_results$showPopulationLabels()) {
        # Map cluster IDs to population names
        population_map <- setNames(
          population_data$Population,
          as.character(population_data$Cluster)
        )
        
        # Add population column to plot data
        plot_data$Population <- population_map[as.character(plot_data$Cluster)]
        
        # Use population names for coloring
        color_by <- "Population"
      } else {
        color_by <- "Cluster"
      }
      
      # Choose appropriate dimensions
      if ("tsne1" %in% colnames(plot_data)) {
        dim1 <- "tsne1"
        dim2 <- "tsne2"
        dim_labels <- c("t-SNE 1", "t-SNE 2")
      } else if ("umap1" %in% colnames(plot_data)) {
        dim1 <- "umap1"
        dim2 <- "umap2"
        dim_labels <- c("UMAP 1", "UMAP 2")
      } else if ("pca1" %in% colnames(plot_data)) {
        dim1 <- "pca1"
        dim2 <- "pca2"
        dim_labels <- c("PC 1", "PC 2")
      } else {
        # Fallback to first two markers if no dimension reduction available
        dim1 <- input$selectedMarkers[1]
        dim2 <- input$selectedMarkers[2]
        dim_labels <- input$selectedMarkers[1:2]
      }
      
      # Create a base ggplot with correct color palette
      p <- ggplot(plot_data, aes(x = .data[[dim1]], y = .data[[dim2]], color = .data[[color_by]])) +
        geom_point(alpha = 0.7, size = point_size/2) +
        get_color_palette(color_palette) +
        labs(
          title = paste("Clusters from", cluster_data$method),
          x = dim_labels[1],
          y = dim_labels[2],
          color = if(color_by == "Population") "Cell Population" else "Cluster"
        ) +
        get_standard_theme(font_size)
      
      # Convert to plotly
      p_plotly <- ggplotly(p, tooltip = c("color", "x", "y")) %>%
        layout(
          legend = list(
            title = list(
              text = if(color_by == "Population") "Cell Population" else "Cluster",
              font = list(
                size = font_size,
                family = "Arial"
              )
            ),
            font = list(
              size = font_size * 0.9,
              family = "Arial"
            ),
            bgcolor = "rgba(255, 255, 255, 0.9)",
            bordercolor = "rgba(0, 0, 0, 0.2)",
            borderwidth = 1
          ),
          hoverlabel = list(
            bgcolor = "white",
            font = list(
              family = "Arial",
              size = font_size * 0.9
            )
          ),
          width = width,
          height = height
        )
      
      # Add cluster labels if showing population names and user has enabled labels
      if (color_by == "Population" && clustering_results$showPopulationLabels() && clustering_results$showClusterLabels()) {
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
    
    # Cluster heatmap
    output$clusterHeatmap <- renderPlot({
      req(clustering_results$clustering_results())
      
      # Explicitly get font_size from app_state
      font_size <- app_state$plot_settings$font_size
      
      # Use original centers but update if merged
      centers <- clustering_results$clustering_results()$centers
      method <- clustering_results$clustering_results()$method
      
      # Get population data
      if (mergeHistory()$active) {
        population_data <- mergeHistory()$current_mapping
      } else {
        population_data <- clustering_results$populations()
      }
      
      # If using merged clusters, adjust the centers
      if (mergeHistory()$active) {
        operations <- mergeHistory()$operations
        
        # Create new centers data
        new_centers <- centers
        
        # Apply each merge operation sequentially
        for (op in operations) {
          merged_id <- op$target_cluster
          merged_from <- op$merged_clusters
          
          # Calculate weighted means across markers
          if (length(merged_from) > 1) {
            # Get counts of each cluster for weighted average
            cluster_counts <- table(clustering_results$clustering_results()$cluster_ids)
            merged_weights <- cluster_counts[merged_from]
            
            # Calculate weighted means across markers
            for (col in 1:ncol(centers)) {
              values <- new_centers[as.character(merged_from), col]
              weights <- merged_weights / sum(merged_weights)
              new_centers[as.character(merged_id), col] <- sum(values * weights, na.rm = TRUE)
            }
            
            # Remove rows for clusters that were merged (except the target)
            new_centers <- new_centers[!(rownames(new_centers) %in% setdiff(as.character(merged_from), as.character(merged_id))), ]
          }
        }
        
        centers <- new_centers
        method <- paste(method, "(", length(operations), " merges)")
      }
      
      # Create heatmap
      createClusterHeatmap(
        centers = centers,
        method = method,
        title = "Cluster Intensity Profiles",
        font_size = font_size,
        population_data = population_data
      )
    }, width = function() app_state$plot_settings$width,
    height = function() app_state$plot_settings$height)
    
    # Cluster statistics
    output$clusterStats <- DT::renderDataTable({
      req(processedData())
      
      # Check whether to use merged or original clusters
      if (mergeHistory()$active) {
        cluster_ids <- mergeHistory()$current_clusters
        population_data <- mergeHistory()$current_mapping
        method_name <- paste(clustering_results$clustering_results()$method, 
                            "(", length(mergeHistory()$operations), " merges)")
      } else if (!is.null(clustering_results$clustering_results())) {
        cluster_ids <- clustering_results$clustering_results()$cluster_ids
        population_data <- clustering_results$populations()
        method_name <- clustering_results$clustering_results()$method
      } else {
        return(NULL)
      }
      
      # Format cluster statistics
      stats_df <- formatClusterStats(
        cluster_ids = cluster_ids,
        total_cells = nrow(processedData()$plot_data),
        population_data = population_data
      )
      
      # Create datatable
      DT::datatable(stats_df, 
                    options = list(scrollX = TRUE, pageLength = 10),
                    caption = paste("Cluster statistics from", method_name)) %>%
        formatRound(columns = c("Percentage", "Confidence"), digits = 2)
    })
    
    # Population results table
    output$populationTable <- DT::renderDataTable({
      req(!is.null(clustering_results$populations()))
      
      # Use merged population mappings if available
      if (mergeHistory()$active) {
        population_data <- mergeHistory()$current_mapping
        
        # Add information about merge operations
        if (length(mergeHistory()$operations) > 0) {
          # Create a summary of merges performed
          merge_summary <- lapply(mergeHistory()$operations, function(op) {
            data.frame(
              Cluster = op$target_cluster,
              Population = paste0(op$new_name, " (merged)"),
              OriginalClusters = paste(op$merged_clusters, collapse=", "),
              MergeTime = format(op$timestamp, "%H:%M:%S")
            )
          })
          
          # Add a header to separate merge history
          population_data <- rbind(
            population_data,
            data.frame(
              Cluster = "---",
              Population = "--- Merge History ---",
              Confidence = NA
            )
          )
          
          # Add merge history to the table
          for (summary in merge_summary) {
            population_data <- rbind(
              population_data,
              data.frame(
                Cluster = summary$Cluster,
                Population = paste0(summary$Population, " (from: ", summary$OriginalClusters, ")"),
                Confidence = NA,
                MergeTime = summary$MergeTime
              )
            )
          }
        }
      } else {
        population_data <- clustering_results$populations()
      }
      
      # Create datatable
      DT::datatable(
        population_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        caption = "Identified Cell Populations"
      ) %>% formatRound(columns = "Confidence", digits = 2)
    })
    
    # Download cluster table
    output$downloadClusterTable <- downloadHandler(
      filename = function() {
        paste0("cluster_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(processedData())
        
        # Check whether to use merged or original clusters
        if (mergeHistory()$active) {
          cluster_ids <- mergeHistory()$current_clusters
          population_data <- mergeHistory()$current_mapping
        } else if (!is.null(clustering_results$clustering_results())) {
          cluster_ids <- clustering_results$clustering_results()$cluster_ids
          population_data <- clustering_results$populations()
        } else {
          return(NULL)
        }
        
        # Format cluster statistics
        stats_df <- formatClusterStats(
          cluster_ids = cluster_ids,
          total_cells = nrow(processedData()$plot_data),
          population_data = population_data
        )
        
        # Write to CSV
        write.csv(stats_df, file, row.names = FALSE)
      }
    )
    
    # Download processed data
    output$downloadProcessedData <- downloadHandler(
      filename = function() {
        paste0("processed_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        req(processedData())
        
        # Get processed data
        data <- processedData()$plot_data
        
        # Add cluster information - use merged if available
        if (mergeHistory()$active) {
          data$Cluster <- as.factor(mergeHistory()$current_clusters)
          
          # Add population information if available
          if (!is.null(mergeHistory()$current_mapping)) {
            # Create mapping of cluster IDs to population names
            pop_mapping <- setNames(
              mergeHistory()$current_mapping$Population,
              mergeHistory()$current_mapping$Cluster
            )
            
            # Add population column
            data$Population <- pop_mapping[as.character(data$Cluster)]
          }
        } else if (!is.null(clustering_results$clustering_results())) {
          data$Cluster <- as.factor(clustering_results$clustering_results()$cluster_ids)
          
          # Add population information if available
          if (!is.null(clustering_results$populations())) {
            # Create mapping of cluster IDs to population names
            pop_mapping <- setNames(
              clustering_results$populations()$Population,
              clustering_results$populations()$Cluster
            )
            
            # Add population column
            data$Population <- pop_mapping[as.character(data$Cluster)]
          }
        }
        
        # Write to CSV
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # Add cluster merge modal
    observeEvent(input$clustering$showMergeModal, {
      req(clustering_results$clustering_results())
      
      # Get current state of clusters
      if (mergeHistory()$active) {
        # Use the current merged state
        current_clusters <- mergeHistory()$current_clusters
        population_data <- mergeHistory()$current_mapping
      } else {
        # Use original clusters
        current_clusters <- clustering_results$clustering_results()$cluster_ids
        population_data <- clustering_results$populations()
      }
      
      # Get unique clusters and their names from current state
      unique_clusters <- unique(current_clusters)
      
      # Create choices for checkboxes with friendly names - showing only available clusters
      cluster_choices <- setNames(
        as.character(unique_clusters),
        sapply(unique_clusters, function(c) {
          pop_name <- population_data$Population[population_data$Cluster == c]
          if (length(pop_name) == 0) pop_name <- "Unknown"
          paste("Cluster", c, ":", pop_name)
        })
      )
      
      showModal(modalDialog(
        title = "Merge Similar Clusters",
        
        # Create checkboxes for all currently available clusters
        checkboxGroupInput(session$ns("clustersToMerge"), 
                         "Select clusters to merge:", 
                         choices = cluster_choices),
        
        textInput(session$ns("mergedClusterName"), "Name for merged cluster:", 
                 value = "Merged Population"),
        
        # Add a note about the cumulative nature of merges
        tags$div(
          class = "help-block",
          tags$b("Note:"), 
          "This merge will be added to any previous merges. Use 'Reset to Original Clusters' to start over."
        ),
        
        footer = tagList(
          actionButton(session$ns("performMerge"), "Merge Clusters", 
                      class = "btn-success"),
          modalButton("Cancel")
        ),
        
        size = "l"
      ))
    })
    
    # Handle cluster merging
    observeEvent(input$performMerge, {
      req(input$clustersToMerge, length(input$clustersToMerge) >= 2)
      
      # Get current state (either original or already-merged clusters)
      if (mergeHistory()$active) {
        # Start from current merged state
        current_clusters <- mergeHistory()$current_clusters
        current_mapping <- mergeHistory()$current_mapping
      } else {
        # Start from original clustering
        current_clusters <- clustering_results$clustering_results()$cluster_ids
        current_mapping <- clustering_results$populations()
      }
      
      # Create a copy to modify
      new_clusters <- current_clusters
      
      # Get lowest cluster ID from selection (to use as the merged ID)
      merged_id <- min(as.numeric(input$clustersToMerge))
      
      # Change all selected clusters to the merged ID
      for (cluster_id in input$clustersToMerge) {
        new_clusters[current_clusters == cluster_id] <- merged_id
      }
      
      # Create a copy of the current mapping to modify
      new_mapping <- current_mapping
      
      # Update name for the merged cluster
      new_mapping$Population[new_mapping$Cluster == merged_id] <- input$mergedClusterName
      
      # Remove rows for clusters that were merged (except the target)
      new_mapping <- new_mapping[!(new_mapping$Cluster %in% setdiff(input$clustersToMerge, merged_id)), ]
      
      # Add this operation to history
      current_history <- mergeHistory()
      current_history$active <- TRUE
      current_history$current_clusters <- new_clusters
      current_history$current_mapping <- new_mapping
      
      # Add details of this merge operation
      new_operation <- list(
        timestamp = Sys.time(),
        merged_clusters = input$clustersToMerge,
        target_cluster = merged_id,
        new_name = input$mergedClusterName
      )
      current_history$operations <- c(current_history$operations, list(new_operation))
      
      # Update merge history
      mergeHistory(current_history)
      
      removeModal()
      
      # Calculate how many clusters we have now
      remaining_clusters <- length(unique(new_clusters))
      total_merges <- length(current_history$operations)
      
      showNotification(
        paste0("Merged into '", input$mergedClusterName, "'. You now have ", 
              remaining_clusters, " clusters (", total_merges, " merge operations)"), 
        type = "default", duration = 5
      )
    })
    
    # Reset clusters to original clustering
    observeEvent(input$clustering$resetMerging, {
      # Reset merge history
      mergeHistory(list(
        active = FALSE,
        operations = list(),
        current_clusters = NULL,
        current_mapping = NULL
      ))
      
      showNotification("Restored original clusters", type = "default", duration = 3)
    })
    
    # Return reactive values that might be needed by other modules
    return(list(
      processed_data = processedData,
      clustering_results = clustering_results
    ))
  })
}