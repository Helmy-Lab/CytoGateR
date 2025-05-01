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
                         choices = c("t-SNE", "UMAP"),
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
                    selected = "None")
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
        tabPanel("Data Info", verbatimTextOutput(ns("fcsInfo")), uiOutput(ns("optimizationMetricsUI"))),
        tabPanel("Cluster Analysis", 
                 conditionalPanel(
                   condition = paste0("input['", ns("clustering-showClusteringOptions"), "'] == true"),
                   tabsetPanel(
                     tabPanel("Cluster Visualization", plotlyOutput(ns("clusterPlot"))),
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
    
    # Store processed data
    processedData <- reactiveVal(NULL)
    
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
    
    # Render t-SNE plot
    output$tsnePlot <- renderPlotly({
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
        color_palette = app_state$plot_settings$color_palette,
        point_size = app_state$plot_settings$point_size,
        font_size = app_state$plot_settings$font_size,
        title = "t-SNE Projection",
        xlab = "t-SNE 1",
        ylab = "t-SNE 2"
      )
      
      # Convert to plotly
      ggplotly(p, width = app_state$plot_settings$width, height = app_state$plot_settings$height) %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(family = "Arial", size = app_state$plot_settings$font_size)
          )
        )
    })
    
    # Render UMAP plot
    output$umapPlot <- renderPlotly({
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
        color_palette = app_state$plot_settings$color_palette,
        point_size = app_state$plot_settings$point_size,
        font_size = app_state$plot_settings$font_size,
        title = "UMAP Projection",
        xlab = "UMAP 1",
        ylab = "UMAP 2"
      )
      
      # Convert to plotly
      ggplotly(p, width = app_state$plot_settings$width, height = app_state$plot_settings$height) %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(family = "Arial", size = app_state$plot_settings$font_size)
          )
        )
    })
    
    # Cluster visualization in dedicated tab
    output$clusterPlot <- renderPlotly({
      req(processedData(), clustering_results$clustering_results())
      
      plot_data <- processedData()$plot_data
      cluster_data <- clustering_results$clustering_results()
      
      # Add cluster information
      plot_data$Cluster <- as.factor(cluster_data$cluster_ids)
      
      # Add population labels if available
      if (!is.null(clustering_results$populations()) && 
          clustering_results$showPopulationLabels()) {
        # Map cluster IDs to population names
        population_map <- setNames(
          clustering_results$populations()$Population,
          clustering_results$populations()$Cluster
        )
        
        # Add population column to plot data
        plot_data$Population <- population_map[as.character(plot_data$Cluster)]
        
        hover_text <- paste(
          "Cluster:", plot_data$Cluster,
          "<br>Population:", plot_data$Population
        )
      } else {
        hover_text <- paste("Cluster:", plot_data$Cluster)
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
      } else {
        # Fallback to first two markers if no dimension reduction available
        dim1 <- input$selectedMarkers[1]
        dim2 <- input$selectedMarkers[2]
        dim_labels <- input$selectedMarkers[1:2]
      }
      
      # Add dimension info to hover text
      hover_text <- paste0(
        hover_text,
        "<br>", dim_labels[1], ":", round(plot_data[[dim1]], 2),
        "<br>", dim_labels[2], ":", round(plot_data[[dim2]], 2)
      )
      
      # Create plot
      p <- plot_ly(plot_data, 
                   x = ~.data[[dim1]], 
                   y = ~.data[[dim2]],
                   color = ~Cluster, 
                   type = "scatter", 
                   mode = "markers",
                   marker = list(size = app_state$plot_settings$point_size),
                   text = hover_text) %>%
        layout(title = paste("Clusters from", cluster_data$method),
               xaxis = list(title = dim_labels[1]),
               yaxis = list(title = dim_labels[2]),
               height = app_state$plot_settings$height,
               width = app_state$plot_settings$width)
      
      # Add cluster labels with population names if available
      if (!is.null(clustering_results$populations()) && 
          clustering_results$showPopulationLabels()) {
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
            font = list(size = app_state$plot_settings$font_size)
          )
        }
      }
      
      return(p)
    })
    
    # Cluster heatmap
    output$clusterHeatmap <- renderPlot({
      req(clustering_results$clustering_results())
      
      # Create heatmap
      createClusterHeatmap(
        centers = clustering_results$clustering_results()$centers,
        method = clustering_results$clustering_results()$method,
        title = "Cluster Intensity Profiles",
        font_size = app_state$plot_settings$font_size
      )
    }, width = function() app_state$plot_settings$width,
    height = function() app_state$plot_settings$height)
    
    # Cluster statistics
    output$clusterStats <- DT::renderDataTable({
      req(clustering_results$clustering_results(), processedData())
      
      # Get clustering results and plot data
      cluster_data <- clustering_results$clustering_results()
      plot_data <- processedData()$plot_data
      
      # Format cluster statistics
      stats_df <- formatClusterStats(
        cluster_ids = cluster_data$cluster_ids,
        total_cells = nrow(plot_data),
        population_data = clustering_results$populations()
      )
      
      # Create datatable
      DT::datatable(stats_df, 
                    options = list(scrollX = TRUE, pageLength = 10),
                    caption = paste("Cluster statistics from", cluster_data$method)) %>%
        formatRound(columns = c("Percentage", "Confidence"), digits = 2)
    })
    
    # Population results table
    output$populationTable <- DT::renderDataTable({
      req(clustering_results$populations())
      
      # Create datatable
      DT::datatable(
        clustering_results$populations(),
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
        req(clustering_results$clustering_results(), processedData())
        
        # Format cluster statistics
        stats_df <- formatClusterStats(
          cluster_ids = clustering_results$clustering_results()$cluster_ids,
          total_cells = nrow(processedData()$plot_data),
          population_data = clustering_results$populations()
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
        
        # Add cluster information if available
        if (!is.null(clustering_results$clustering_results())) {
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
    
    # Return reactive values that might be needed by other modules
    return(list(
      processed_data = processedData,
      clustering_results = clustering_results
    ))
  })
}