# Raw Data Analysis Module for Flow Cytometry Analysis Tool

#' UI for the Raw Data Analysis Module
#' @param id Module ID
#' @return UI elements for raw data analysis
rawDataModuleUI <- function(id) {
  ns <- NS(id)
  
    fluidPage(
    shinyjs::useShinyjs(),
    
    # Enhanced CSS for better styling - matching compensation and gating modules
    tags$head(
      tags$style(HTML("
        .analysis-workflow {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 15px;
          border-radius: 8px;
          margin-bottom: 20px;
        }
        .parameter-group {
          background-color: #e3f2fd;
          padding: 10px;
          border-radius: 5px;
          margin-bottom: 10px;
        }
        # .compensation-panel {
        #   background-color: #f8f9fa;
        #   padding: 15px;
        #   border-radius: 8px;
        #   margin-bottom: 10px;
        #   border: 1px solid #dee2e6;
        # }
        .method-controls {
          background-color: #f1f8e9;
          padding: 8px;
          border-radius: 5px;
          margin: 5px 0;
        }
      "))
    ),
    # Workflow Progress Header
    div(class = "analysis-workflow",
        h3(icon("chart-line"), "Raw Data Analysis Workflow"),
        p("Upload flow cytometry data, configure analysis parameters, and explore dimensional reductions")
    ),
    
    fluidRow(
      # Left Panel - Enhanced Controls with shinydashboard boxes
      column(3,
        # Data Upload Section
        shinydashboard::box(
          title = "Data Upload", status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          
          fileInput(ns("fcsFile"), "Upload FCS/CSV/TSV File", 
                    accept = c(".fcs", ".csv", ".tsv"),
                    buttonLabel = "Browse Files...",
                    placeholder = "No file selected"),
          
          helpText(icon("info-circle"), 
                   "Supported formats: FCS files, CSV, and TSV data files."),

          # §2.4 [H3]: visible tested-limits statement.
          div(class = "alert alert-info", style = "margin-top: 8px; font-size: 12px;",
              icon("microchip"), strong(" Tested limits: "),
              "files with more than 500,000 events are automatically subsampled on load to protect memory. ",
              "Maximum panel size and total event count are benchmarked on the deployment hardware - ",
              "see Methods. Replace the figures here once you have run the §7 benchmark on your machine."),
          
          # Marker Selection UI
          uiOutput(ns("markerSelectUI"))
        ),
        
        # Data Transformation Section
        shinydashboard::box(
          title = "Data Transformation", status = "success", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          
          div(class = "parameter-group",
            h5(icon("magic"), "Transformation Parameters"),
            checkboxInput(ns("transform"), "Apply arcsinh transformation", value = TRUE),
            numericInput(ns("cofactor"), "Transformation cofactor", value = 5, min = 1, max = 10),
            numericInput(ns("nEvents"), "Number of events to analyze", value = 5000, min = 100, step = 100)
          )
        ),
        
        # Dimensionality Reduction Section
        shinydashboard::box(
          title = "Dimensionality Reduction", status = "info", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          
          # Method selection
          div(class = "parameter-group",
            h5(icon("project-diagram"), "Methods"),
            checkboxGroupInput(ns("methods"), "Select Methods:",
                               choices = c("t-SNE", "UMAP", "PCA", "MDS"),
                               selected = c("t-SNE", "UMAP"))
          ),
          
          # Replace conditionalPanels with server-side rendering
          uiOutput(ns("dimensionalityMethodsUI"))
        ),
        
        # Quality Control Section (Gating moved to dedicated module)
        shinydashboard::box(
          title = "Quality Control", status = "warning", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          
          # QC options
          div(class = "parameter-group",
            h5(icon("shield-alt"), "Quality Control"),
            checkboxInput(ns("performQC"), "Perform Quality Control", value = TRUE),
            # Replace conditionalPanel with server-side rendering
            uiOutput(ns("qcSettingsUI"))
          ),
        ),
        
        # Clustering Section
        shinydashboard::box(
          title = "Clustering Analysis", status = "success", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          
          # Add clustering module UI
          clusteringModuleUI(ns("clustering"))
        ),
        
        # Run Analysis Section
        shinydashboard::box(
          title = "Execute Analysis", status = "primary", solidHeader = TRUE,
          width = 12,
          
          actionButton(ns("run"), "Run Analysis", 
                       class = "btn-primary btn-lg",
                       icon = icon("play"),
                       style = "width: 100%; font-weight: bold;"),
          
          hr(),
          
          # Download options
          h5(icon("download"), "Download Results"),
          fluidRow(
            column(12,
              downloadButton(ns("downloadClusterTable"), "Download Cluster Data",
                            class = "btn-success btn-sm",
                            icon = icon("table"),
                            style = "width: 100%; margin-bottom: 5px;")
            ),
            column(12,
              downloadButton(ns("downloadProcessedData"), "Download Processed Data",
                            class = "btn-info btn-sm", 
                            icon = icon("database"),
                            style = "width: 100%;")
            )
          )
        )
      ),
      
      # Main Analysis Panel - Enhanced with shinydashboard boxes
      column(9,
        # ----------------------------------------------------------------------
        # QC Summary Panel (Framework 2.2)
        # Placed above the analysis outputs so QC status is visible immediately
        # after file load and before any analysis is interpreted.
        # ----------------------------------------------------------------------
        shinydashboard::box(
          title = "Quality Control Summary", status = "warning", solidHeader = TRUE,
          width = 12, collapsible = TRUE,

          uiOutput(ns("qcStatusBanner")),

          DT::dataTableOutput(ns("qcSummaryTable")),

          tags$hr(),
          fluidRow(
            column(7,
              tags$small(
                style = "color:#6c757d;",
                tags$strong("Tested limits: "),
                "verified up to 500 MB per FCS file and ~1,000,000 events on ",
                "16 GB RAM. Larger files may load but are not performance-tested."
              )
            ),
            column(5,
              downloadButton(ns("downloadQCReport"),
                             "Download flowAI anomaly report",
                             class = "btn-sm btn-outline-secondary",
                             style = "width:100%;")
            )
          )
        ),

        # Dimensionality Reduction Visualizations
        shinydashboard::box(
          title = "Dimensionality Reduction Visualizations", status = "primary", solidHeader = TRUE,
          width = 12,
          
          tabsetPanel(
            tabPanel("t-SNE", 
                     br(),
                     shinycssloaders::withSpinner(plotlyOutput(ns("tsnePlot"), height = "600px"))),
            tabPanel("UMAP", 
                     br(),
                     shinycssloaders::withSpinner(plotlyOutput(ns("umapPlot"), height = "600px"))),
            tabPanel("PCA", 
                     br(),
                     shinycssloaders::withSpinner(plotlyOutput(ns("pcaPlot"), height = "600px"))),
            tabPanel("MDS", 
                     br(),
                     shinycssloaders::withSpinner(plotlyOutput(ns("mdsPlot"), height = "600px"))),
            
            # Marker Expression Heatmaps Tab
            tabPanel("Marker Heatmaps", 
                     br(),
                     fluidRow(
                       column(3,
                         div(class = "parameter-group",
                           h5(icon("fire"), "Heatmap Controls"),
                           
                           selectInput(ns("heatmapMarker"), "Select Marker:", 
                                      choices = NULL, selected = NULL),
                           
                           selectInput(ns("heatmapDimMethod"), "Dimensionality Method:",
                                      choices = NULL, selected = NULL),
                           
                           selectInput(ns("heatmapMethod"), "Visualization Method:",
                                      choices = c("Scatter Plot" = "scatter",
                                                 "Hexagonal Binning" = "hex",
                                                 "Density Contours" = "density2d", 
                                                 "Filled Contours" = "contour"),
                                      selected = "scatter"),
                           
                           # Replace conditionalPanel with server-side rendering
                           uiOutput(ns("heatmapBinsUI")),
                           
                           selectInput(ns("heatmapColorPalette"), "Color Palette:",
                                      choices = c("Plasma" = "plasma",
                                                 "Viridis" = "viridis",
                                                 "Magma" = "magma",
                                                 "Inferno" = "inferno"),
                                      selected = "plasma"),
                           
                           # Legend Style Options
                           radioButtons(ns("legendStyle"), "Legend Style:",
                                       choices = list(
                                         "Qualitative (Low/Medium/High)" = "qualitative",
                                         "Numerical (actual values)" = "numerical"
                                       ),
                                       selected = "qualitative"),
                           
                           conditionalPanel(
                             condition = paste0("input['", ns("legendStyle"), "'] == 'qualitative'"),
                             selectInput(ns("labelStyle"), "Label Detail:",
                                        choices = list(
                                          "Simple (Low/High)" = "simple",
                                          "Standard (Low/Medium/High)" = "standard",
                                          "Detailed (5 levels)" = "detailed",
                                          "Biological (Negative/Dim/Bright)" = "biological"
                                        ),
                                        selected = "standard"),
                             helpText("Uses consistent qualitative labels across all markers while preserving biological relationships.")
                           ),
                           
                           conditionalPanel(
                             condition = paste0("input['", ns("legendStyle"), "'] == 'numerical'"),
                             helpText("Shows actual transformed values. Note: ranges may differ between markers.")
                           ),
                           
                           hr(),
                           
                           # Marker Renaming Section
                           h5(icon("edit"), "Marker Renaming"),
                           actionButton(ns("showMarkerRenaming"), "Rename Markers", 
                                       class = "btn-info btn-sm", icon = icon("edit"),
                                       style = "width: 100%; margin-bottom: 10px;"),
                           
                           # Display current marker mappings
                           uiOutput(ns("markerMappingSummary")),
                           
                           hr(),
                           
                           fluidRow(
                             column(12,
                               actionButton(ns("generateAllHeatmapsFast"), "Fast Grid View", 
                                           class = "btn-primary", icon = icon("th"),
                                           style = "width: 100%;")
                             )
                           )
                         )
                       ),
                       column(9,
                         # Ultra-fast grid view or individual heatmap
                         uiOutput(ns("heatmapMainDisplay"))
                       )
                     )
            )
          )
        ),
        
        # Data Information and Metrics
        shinydashboard::box(
          title = "Data Information & Optimization Metrics", status = "info", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          
          fluidRow(
            column(6,
              h5(icon("info-circle"), "Dataset Information"),
              verbatimTextOutput(ns("fcsInfo"))
            ),
            column(6,
              h5(icon("chart-bar"), "Analysis Metrics"),
              uiOutput(ns("optimizationMetricsUI"))
            )
          )
        ),
        
        # Cluster Analysis
        shinydashboard::box(
          title = "Cluster Analysis Results", status = "success", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          
          # Replace conditionalPanels with server-side rendering
          uiOutput(ns("clusterAnalysisUI"))
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

    # Static plot settings - single source of truth for all renders.
    # get_plot_settings() is a plain function (not reactive), so referencing
    # ps$* inside renderPlotly does NOT create a reactive dependency and will
    # NOT cause cascading re-renders when any other reactive changes.
    ps <- get_plot_settings()
    # Session cleanup handler — ensures any running futures are logged when the
    # browser disconnects.
    session_closed <- FALSE
    session$onSessionEnded(function() {
      session_closed <<- TRUE
      logSessionEnded(id, sessionEndReason(session))



    })
    
    # Reactive values to store processed data and results
    rawFCS <- reactive({
      req(input$fcsFile)

      # Server-side extension allow-list (client-side accept= can be bypassed)
      allowed_exts <- c("fcs", "csv", "tsv")
      ext <- tolower(tools::file_ext(input$fcsFile$name))
      validate(need(ext %in% allowed_exts, paste("Unsupported file type:", ext, "— allowed: fcs, csv, tsv")))

      switch(ext,
             # §2.4 [H2]: strided load caps events held in memory for large files.
             "fcs" = readFCSchunked(input$fcsFile$datapath)$ff,
             "csv" = fread(input$fcsFile$datapath),
             "tsv" = fread(input$fcsFile$datapath, sep = "\t")
      )
    })

    # §2.4 [H1]: metadata-only reactive (channels/keywords, 1 event). Drives the
    # channel list and file-info panels so the FULL event matrix is not loaded
    # until an event consumer (Run Analysis / event plots) actually runs.
    fcsMeta <- reactive({
      req(input$fcsFile)
      ext <- tolower(tools::file_ext(input$fcsFile$name))
      if (ext == "fcs") {
        readFCSmetadata(input$fcsFile$datapath)
      } else {
        fread(input$fcsFile$datapath,
              sep = if (ext == "tsv") "\t" else "auto", nrows = 5)
      }
    })
    
    # Show raw data information
    output$fcsInfo <- renderPrint({
      req(input$fcsFile)
      meta <- fcsMeta()                              # §2.4 [H1]: metadata only, no full load
      cat("File name: ", input$fcsFile$name, "\n")

      if (inherits(meta, "flowFrame")) {
        n_total <- fcsEventCount(meta)
        cat("Total events: ", format(n_total, big.mark = ","),
            "  |  Channels: ", ncol(meta), "\n")
        if (!is.na(n_total) && n_total > 500000) {
          cat("Note: large file - events will be subsampled to ~500,000 on load ",
              "for memory safety (see Tested limits).\n")
        }

        cat("\nParameters (Parameter Name -> Marker Description):\n")
        cat(paste(rep("=", 60), collapse = ""), "\n")

        params <- parameters(meta)
        param_info <- data.frame(
          Parameter = params$name,
          Description = params$desc,
          stringsAsFactors = FALSE
        )

        for (i in 1:nrow(param_info)) {
          param_name <- param_info$Parameter[i]
          param_desc <- param_info$Description[i]
          if (is.na(param_desc) || param_desc == "") {
            param_desc <- "(No description)"
          }
          if (grepl("^(FSC|SSC|Time|Width|Height)", param_name)) {
            cat(sprintf("  %-8s -> %s (Scatter/Time)\n", param_name, param_desc))
          } else {
            cat(sprintf("  %-8s -> %s *** FLUORESCENCE ***\n", param_name, param_desc))
          }
        }

        cat("The parameter names ($P1N, $P2N, etc.) are internal identifiers.\n")
        cat("The descriptions show the actual marker names (e.g., CD3-FITC, CD4-PE).\n")
      } else {
        cat("Columns (preview):\n")
        print(colnames(meta))
      }
    })
    
    # Dynamic marker selection UI
    output$markerSelectUI <- renderUI({
      req(fcsMeta())                                  # §2.4 [H1]: channel list from metadata, before full load

      # Get marker choices based on data type
      if (inherits(fcsMeta(), "flowFrame")) {
        fcs_data <- fcsMeta()
        params <- parameters(fcs_data)
        parameter_names <- colnames(exprs(fcs_data))
        parameter_descriptions <- params$desc[match(parameter_names, params$name)]

        # Create choices with format: "Parameter Name - Description"
        choices <- setNames(
          parameter_names,
          sapply(1:length(parameter_names), function(i) {
            param_name <- parameter_names[i]
            param_desc <- parameter_descriptions[i]
            if (!is.na(param_desc) && param_desc != "") {
              paste0(param_name, " - ", param_desc)
            } else {
              param_name
            }
          })
        )

        # Filter to show only fluorescence channels by default, but allow all
        fluorescence_channels <- choices[!grepl("^(FSC|SSC|Time|Width|Height)", names(choices))]
        default_selection <- names(fluorescence_channels)[1:min(5, length(fluorescence_channels))]

      } else {
        exprs_data <- fcsMeta()
        choices <- colnames(exprs_data)
        default_selection <- choices[1:min(5, length(choices))]
      }

      selectInput(session$ns("selectedMarkers"), "Select Markers/Channels",
                  choices = choices,
                  selected = default_selection,
                  multiple = TRUE)
    })
    
    # Live/Dead marker selection UI
    output$liveDeadMarkerUI <- renderUI({
      req(rawFCS())
      
      # Get marker choices based on data type
      if (inherits(rawFCS(), "flowFrame")) {
        fcs_data <- rawFCS()
        params <- parameters(fcs_data)
        parameter_names <- colnames(exprs(fcs_data))
        parameter_descriptions <- params$desc[match(parameter_names, params$name)]
        
        # Look for potential live/dead markers in both parameter names and descriptions
        potential_ld_markers_names <- grep("live|dead|viability|FL3-A", 
                                          parameter_names, 
                                     ignore.case = TRUE, 
                                     value = TRUE)
        
        potential_ld_markers_desc <- grep("live|dead|viability", 
                                         parameter_descriptions, 
                                         ignore.case = TRUE)
        
        if (length(potential_ld_markers_desc) > 0) {
          potential_ld_markers_from_desc <- parameter_names[potential_ld_markers_desc]
        } else {
          potential_ld_markers_from_desc <- character(0)
        }
        
        # Combine and deduplicate potential markers
        potential_ld_markers <- unique(c(potential_ld_markers_names, potential_ld_markers_from_desc))
        
        # If no matches found, include all channels
        if (length(potential_ld_markers) == 0) {
          potential_ld_markers <- parameter_names
        }
        
        # Create named vector for selection with descriptions
        marker_choices <- setNames(
          potential_ld_markers,
          sapply(potential_ld_markers, function(param) {
            desc <- parameter_descriptions[match(param, parameter_names)]
            if (!is.na(desc) && desc != "") {
              paste0(param, " - ", desc)
            } else {
              param
            }
          })
        )
        
        choices <- c("None" = "None", marker_choices)
        
        # Find the default selected marker
        default_selection <- "None"
        live_dead_matches <- grep("live.*dead|dead.*live|viability", names(choices), ignore.case = TRUE)
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
    
    # Store marker name mappings (technical name -> display name)
    markerNameMappings <- reactiveVal(list())
    
    # Helper function to get display name for a marker
    getMarkerDisplayName <- function(technical_name, mappings = markerNameMappings()) {
      display_name <- mappings[[technical_name]]
      if (is.null(display_name) || display_name == "") {
        return(technical_name)  # Return technical name if no mapping
      }
      return(display_name)
    }
    
    # Helper function to get technical name from display name
    getTechnicalName <- function(display_name, mappings = markerNameMappings()) {
      # Find technical name that maps to this display name
      for (tech_name in names(mappings)) {
        if (mappings[[tech_name]] == display_name) {
          return(tech_name)
        }
      }
      return(display_name)  # Return as-is if no mapping found
    }
    
    # Helper function to get all display names for markers
    getMarkerChoicesWithDisplayNames <- function(technical_markers, mappings = markerNameMappings()) {
      display_names <- sapply(technical_markers, function(tech) getMarkerDisplayName(tech, mappings))
      setNames(technical_markers, display_names)
    }
    
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
        fcs_columns <- colnames(exprs(rawFCS()))
        fsc_col <- grep("^FSC-A", fcs_columns, value = TRUE)
        if (length(fsc_col) == 0) {
          # If FSC-A not found, try to find any FSC column
          fsc_col <- grep("^FSC", fcs_columns, value = TRUE)
          if (length(fsc_col) == 0) {
            # If still no FSC, use the first column
            if (length(fcs_columns) > 0) {
              fsc_col <- fcs_columns[1]
            } else {
              return(ggplot() + 
                       annotate("text", x = 0.5, y = 0.5, 
                                label = "No data columns found in FCS file") +
                       theme_void())
            }
          } else {
            fsc_col <- fsc_col[1]
          }
        } else {
          fsc_col <- fsc_col[1]
        }
        
        # Validate that both columns exist
        if (!(ld_marker %in% fcs_columns)) {
          return(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, 
                            label = paste("Marker", ld_marker, "not found in dataset")) +
                   theme_void())
        }
        
        if (!(fsc_col %in% fcs_columns)) {
          return(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, 
                            label = paste("FSC column", fsc_col, "not found in dataset")) +
                   theme_void())
        }
        
        # Create scatter plot data with error handling
        plot_data <- tryCatch({
          data.frame(
          FSC = exprs(rawFCS())[, fsc_col],
          LiveDead = exprs(rawFCS())[, ld_marker]
        )
        }, error = function(e) {
          return(NULL)
        })
        
        if (is.null(plot_data)) {
          return(ggplot() + 
                   annotate("text", x = 0.5, y = 0.5, 
                            label = "Error accessing data columns - check FCS file format") +
                   theme_void())
        }
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
    
    # Dynamic marker assignment UI for control files
    output$markerAssignmentUI <- renderUI({
      req(input$controlFiles)
      
      # Get available markers from main data
      if (!is.null(rawFCS()) && inherits(rawFCS(), "flowFrame")) {
        fcs_data <- rawFCS()
        params <- parameters(fcs_data)
        
        # Create named choices showing both parameter name and description
        parameter_names <- colnames(exprs(fcs_data))
        parameter_descriptions <- params$desc[match(parameter_names, params$name)]
        
        # Create choices with format: "Parameter Name - Description"
        choices <- setNames(
          parameter_names,
          sapply(1:length(parameter_names), function(i) {
            param_name <- parameter_names[i]
            param_desc <- parameter_descriptions[i]
            if (!is.na(param_desc) && param_desc != "") {
              paste0(param_name, " - ", param_desc)
            } else {
              param_name
            }
          })
        )
        
        # Filter out non-fluorescence channels (keep only relevant markers)
        fluorescence_channels <- choices[!grepl("^(FSC|SSC|Time|Width|Height)", names(choices))]
        
        
        
        # Placeholder for compensation functionality
        div(class = "alert alert-info",
            icon("info-circle"), 
            " Spillover compensation has been moved to the dedicated Compensation module.")
        
      } else {
        div(class = "alert alert-warning",
            "Please upload main FCS file first to see available markers")
      }
    })
    
    
    
    # ============================================================================
    # SERVER-SIDE CONDITIONAL UI RENDERING
    # ============================================================================
    
    
    # Dimensionality reduction methods UI
    output$dimensionalityMethodsUI <- renderUI({
      methods <- input$methods
      if (is.null(methods)) return(NULL)
      
      ui_elements <- list()
      
      # t-SNE parameters
      if ("t-SNE" %in% methods) {
        ui_elements[["tsne"]] <- div(class = "method-controls",
          h6(icon("cog"), "t-SNE Parameters"),
          numericInput(session$ns("perplexity"), "Perplexity", value = 30, min = 5, max = 50),
          checkboxInput(session$ns("use_barnes_hut"), "Use Barnes-Hut Approximation (faster)", value = TRUE),
          
          # Barnes-Hut theta setting
          if (isTRUE(input$use_barnes_hut)) {
            sliderInput(session$ns("tsne_theta"), "Barnes-Hut theta", 
                        min = 0.0, max = 1.0, value = 0.5, step = 0.1)
          } else if (isFALSE(input$use_barnes_hut)) {
            div(class = "alert alert-warning", style = "padding: 8px; margin: 5px 0;",
                icon("exclamation-triangle"), " Exact t-SNE is very slow for datasets > 1000 cells.")
          },
          
          numericInput(session$ns("tsne_max_iter"), "Maximum Iterations", value = 1000, min = 100, max = 10000, step = 100)
        )
      }
      
      # UMAP parameters
      if ("UMAP" %in% methods) {
        ui_elements[["umap"]] <- div(class = "method-controls",
          h6(icon("cog"), "UMAP Parameters"),
          numericInput(session$ns("n_neighbors"), "n_neighbors", value = 15, min = 2, max = 100)
        )
      }
      
      # PCA parameters
      if ("PCA" %in% methods) {
        ui_elements[["pca"]] <- div(class = "method-controls",
          h6(icon("cog"), "PCA Parameters"),
          numericInput(session$ns("pca_components"), "Number of Components", value = 2, min = 2, max = 10)
        )
      }
      
      # MDS parameters
      if ("MDS" %in% methods) {
        ui_elements[["mds"]] <- div(class = "method-controls",
          h6(icon("cog"), "MDS Parameters"),
          p("MDS uses Euclidean distances by default."),
          tags$small("Note: MDS can be slow for large datasets.")
        )
      }
      
      do.call(tagList, ui_elements)
    })
    
    # QC settings UI
    output$qcSettingsUI <- renderUI({
      if (isTRUE(input$performQC)) {
        numericInput(session$ns("maxAnomalies"), "Max Anomalies (%)", value = 10, min = 0, max = 50)
      }
    })

    # ==========================================================================
    # QC OVERRIDE CONFIRMATION (Framework 2.2)
    #
    # QC defaults to on. Disabling it is allowed, but only after the user
    # explicitly confirms -- unchecking the box alone must not silently turn
    # QC off.
    # ==========================================================================
    qcOverrideConfirmed <- reactiveVal(TRUE)

    observeEvent(input$performQC, {
      if (!isTRUE(input$performQC)) {
        qcOverrideConfirmed(FALSE)
        showModal(modalDialog(
          title = tagList(icon("exclamation-triangle"), "Disable Quality Control?"),
          div(
            p("Time-based QC (flowAI) flags acquisition anomalies -- flow-rate ",
              "irregularities, signal drift, and out-of-range events -- before ",
              "the data reaches any downstream analysis."),
            p(strong("Disabling QC means unfiltered, unreviewed events will be ",
                      "analysed and reported."))
          ),
          footer = tagList(
            actionButton(session$ns("cancelDisableQC"), "Keep QC Enabled", class = "btn-primary"),
            actionButton(session$ns("confirmDisableQC"), "Disable QC", class = "btn-danger")
          ),
          easyClose = FALSE
        ))
      } else {
        qcOverrideConfirmed(TRUE)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$confirmDisableQC, {
      qcOverrideConfirmed(TRUE)
      removeModal()
      showNotification("Quality control disabled for this session.", type = "warning", duration = 6)
    })

    observeEvent(input$cancelDisableQC, {
      updateCheckboxInput(session, "performQC", value = TRUE)
      qcOverrideConfirmed(TRUE)
      removeModal()
    })

    # ==========================================================================
    # QC SUMMARY PANEL (Framework 2.2)
    # ==========================================================================

    # Local module state (6.6 Isolated Module State).
    qcState <- reactiveVal(list(
      metrics = NULL, status = NULL, error = NULL,
      livedead = NULL, sample_name = NULL, report_path = NULL
    ))

    # Shared report directory created in the MAIN process. The multisession
    # worker has its own tempdir(), so a path created inside the future would
    # not reliably be readable here. Passing an explicit path avoids that.
    qcReportDir <- file.path(tempdir(), paste0("cytogater_qc_", session$token))
    dir.create(qcReportDir, showWarnings = FALSE, recursive = TRUE)

    # Reset QC state whenever a new file is chosen, so a previous sample's
    # status can never be mistaken for the current one.
    observeEvent(input$fcsFile, {
      qcState(list(
        metrics = NULL, status = NULL, error = NULL,
        livedead = NULL, sample_name = input$fcsFile$name, report_path = NULL
      ))
    }, ignoreInit = TRUE)

    # Status banner - the visible surface required by 2.2 / 6.4.
    output$qcStatusBanner <- renderUI({
      st <- qcState()

      if (is.null(input$fcsFile)) {
        return(div(class = "alert alert-light",
                   style = "border-left:4px solid #6c757d;",
                   icon("upload"), " Upload a file to see its QC summary."))
      }

      if (!isTRUE(input$performQC)) {
        return(div(class = "alert alert-warning",
                   style = "border-left:4px solid #fd7e14;",
                   icon("exclamation-triangle"),
                   tags$strong(" Quality control is disabled. "),
                   "Time-based QC has not been applied to this sample. ",
                   "Results are based on unfiltered data."))
      }

      if (identical(st$status, QC_STATUS$ERROR)) {
        return(div(class = "alert alert-danger",
                   style = "border-left:4px solid #dc3545;",
                   icon("times-circle"), tags$strong(" QC failed. "),
                   st$error))
      }

      if (identical(st$status, QC_STATUS$WARN)) {
        return(div(class = "alert alert-warning",
                   style = "border-left:4px solid #fd7e14;",
                   icon("exclamation-triangle"), tags$strong(" QC passed with warnings. "),
                   sprintf("%.1f%% of events were flagged as anomalous.",
                           (st$metrics$removed_pct %||% 0) * 100)))
      }

      if (identical(st$status, QC_STATUS$PASS)) {
        return(div(class = "alert alert-success",
                   style = "border-left:4px solid #28a745;",
                   icon("check-circle"), tags$strong(" QC passed. "),
                   sprintf("%.1f%% of events removed by time-based QC.",
                           (st$metrics$removed_pct %||% 0) * 100)))
      }

      div(class = "alert alert-light", style = "border-left:4px solid #6c757d;",
          icon("clock"), " QC has not run yet. Click ", tags$strong("Run Analysis"),
          " to apply quality control to this sample.")
    })

    # Per-sample QC summary table (the four rows specified in 2.2).
    output$qcSummaryTable <- DT::renderDataTable({
      st <- qcState()
      tbl <- buildQCSummaryTable(
        qc_metrics  = st$metrics,
        qc_status   = st$status,
        livedead    = st$livedead,
        sample_name = st$sample_name %||% "sample"
      )
      tbl$Status <- vapply(tbl$Status, function(s) {
        if (identical(s, EM_DASH)) EM_DASH else qcStatusBadge(s)
      }, character(1))

      DT::datatable(
        tbl,
        rownames  = FALSE,
        escape    = FALSE,           # required for the status badges / em dashes
        colnames  = c("Metric", "Events", "% of Loaded", "Status"),
        options   = list(dom = "t", ordering = FALSE, paging = FALSE)
      )
    })

    # flowAI anomaly report download (2.2).
    output$downloadQCReport <- downloadHandler(
      filename = function() {
        base <- tools::file_path_sans_ext(qcState()$sample_name %||% "sample")
        paste0(gsub("[^A-Za-z0-9_.-]", "_", base), "_flowAI_QC_report.html")
      },
      content = function(file) {
        path <- qcState()$report_path
        if (is.null(path) || !file.exists(path)) {
          # No silent empty file - explain why there is nothing to download.
          writeLines(c(
            "<html><body style='font-family:sans-serif;padding:2em;'>",
            "<h3>No flowAI anomaly report available</h3>",
            "<p>A report is generated only after quality control has run",
            "successfully on an FCS file. Run the analysis with",
            "'Perform Quality Control' enabled, then download again.</p>",
            "</body></html>"
          ), file)
          showNotification("No QC report available yet - run QC first.",
                           type = "warning", duration = 6)
          return(invisible(NULL))
        }
        file.copy(path, file, overwrite = TRUE)
      }
    )
    
    
    
    # Heatmap bins UI
    output$heatmapBinsUI <- renderUI({
      if (!is.null(input$heatmapMethod) && input$heatmapMethod == "hex") {
        numericInput(session$ns("heatmapBins"), "Number of Bins:", 
                     value = 50, min = 10, max = 100, step = 5)
      }
    })
    
    # Marker mapping summary UI
    output$markerMappingSummary <- renderUI({
      mappings <- markerNameMappings()
      if (length(mappings) == 0) {
        return(div(style = "color: #6c757d; font-style: italic; text-align: center; padding: 5px;",
                   "No markers renamed yet"))
      }
      
      # Create a compact summary of mappings
      mapping_text <- paste(
        sapply(names(mappings), function(tech) {
          display <- mappings[[tech]]
          if (tech != display) {
            paste0(tech, " → ", display)
          } else {
            NULL
          }
        }),
        collapse = "\n"
      )
      
      if (mapping_text == "") {
        return(div(style = "color: #6c757d; font-style: italic; text-align: center; padding: 5px;",
                   "No markers renamed yet"))
      }
      
      div(
        style = "background-color: #f8f9fa; padding: 8px; border-radius: 4px; border: 1px solid #dee2e6; font-size: 0.85em;",
        h6("Current Mappings:", style = "margin-bottom: 5px; color: #495057;"),
        pre(mapping_text, style = "margin: 0; white-space: pre-wrap; color: #495057;")
      )
    })
    
    # Show marker renaming modal
    observeEvent(input$showMarkerRenaming, {
      req(processedData())
      markers <- processedData()$markers
      current_mappings <- markerNameMappings()
      
      marker_inputs <- lapply(seq_along(markers), function(i) {
        marker <- markers[i]
        current_display <- getMarkerDisplayName(marker, current_mappings)
        
        div(
          style = "margin-bottom: 10px; padding: 10px; border: 1px solid #dee2e6; border-radius: 4px; background-color: #f8f9fa;",
          fluidRow(
            column(5,
                   strong("Technical Name:"),
                   br(),
                   code(marker, style = "background-color: #e9ecef; padding: 2px 4px; border-radius: 3px;")
            ),
            column(7,
                   textInput(
                     inputId = session$ns(paste0("marker_display_", i)),
                     label = "Display Name:",
                     value = current_display,
                     placeholder = "Enter friendly name..."
                   )
            )
          )
        )
      })
      
      showModal(modalDialog(
        title = div(icon("edit"), " Rename Markers"),
        size = "l",
        
        div(
          class = "alert alert-info",
          style = "margin-bottom: 15px;",
          icon("info-circle"),
          strong(" Instructions: "),
          "Enter friendly names for your markers. Leave blank to use the technical name. ",
          "Examples: FL1-A -> CD3, FL2-A -> CD4, FJComp-FL1-A -> CD8, etc."
        ),
        
        div(
          style = "max-height: 400px; overflow-y: auto; padding: 5px;",
          do.call(tagList, marker_inputs)
        ),
        
        footer = tagList(
          actionButton(session$ns("resetMarkerNames"), "Reset All",
                       class = "btn-warning", icon = icon("undo")),
          downloadButton(session$ns("downloadMarkerMappings"), "Export Mappings",
                         class = "btn-secondary", icon = icon("download")),
          fileInput(session$ns("uploadMarkerMappings"), NULL,
                    buttonLabel = "Import Mappings",
                    accept = c(".csv"),
                    placeholder = "CSV file"),
          modalButton("Cancel"),
          actionButton(session$ns("saveMarkerNames"), "Save Changes",
                       class = "btn-success", icon = icon("save"))
        )
      ))
    })
    
    # Save marker name mappings
    observeEvent(input$saveMarkerNames, {
      req(processedData())
      markers <- processedData()$markers
      
      new_mappings <- list()
      for (i in seq_along(markers)) {
        marker   <- markers[i]
        input_id <- paste0("marker_display_", i)
        new_name <- input[[input_id]]
        
        if (!is.null(new_name) && trimws(new_name) != "") {
          new_mappings[[marker]] <- trimws(new_name)
        } else {
          new_mappings[[marker]] <- marker
        }
      }
      
      markerNameMappings(new_mappings)
      removeModal()
      showNotification("Marker names updated successfully!", type = "message", duration = 3)
    })
    
    # Reset all marker names to technical names
    observeEvent(input$resetMarkerNames, {
      markerNameMappings(list())
      removeModal()
      showNotification("All marker names reset to technical names", type = "message", duration = 3)
    })
    
    # Export marker name mappings as CSV
    output$downloadMarkerMappings <- downloadHandler(
      filename = function() {
        paste0("marker_mappings_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        mappings <- markerNameMappings()
        mapping_df <- if (length(mappings) == 0) {
          data.frame(TechnicalName = character(0), DisplayName = character(0),
                     stringsAsFactors = FALSE)
        } else {
          data.frame(TechnicalName = names(mappings), DisplayName = unlist(mappings),
                     stringsAsFactors = FALSE)
        }
        write.csv(mapping_df, file, row.names = FALSE)
      }
    )
    
    # Import marker name mappings from CSV
    observeEvent(input$uploadMarkerMappings, {
      req(input$uploadMarkerMappings)
      
      tryCatch({
        mapping_df <- read.csv(input$uploadMarkerMappings$datapath, stringsAsFactors = FALSE)
        
        if (!all(c("TechnicalName", "DisplayName") %in% colnames(mapping_df))) {
          showNotification("CSV must have 'TechnicalName' and 'DisplayName' columns",
                           type = "error", duration = 5)
          return()
        }
        
        markerNameMappings(setNames(as.list(mapping_df$DisplayName), mapping_df$TechnicalName))
        showNotification(paste("Imported", nrow(mapping_df), "marker mappings"),
                         type = "message", duration = 3)
        
      }, error = function(e) {
        showNotification(paste("Error reading mappings file:", e$message),
                         type = "error", duration = 5)
      })
    })
    
    # ============================================================================
    # SERVER-SIDE CONDITIONAL UI RENDERING
    # ============================================================================
    
    # Live/Dead threshold controls
    output$liveDeadControlsUI <- renderUI({
      if (!is.null(input$liveDeadMarkerSelect) && input$liveDeadMarkerSelect != "None") {
        tagList(
          sliderInput(session$ns("liveDeadThresholdSlider"), "Threshold",
                      min = 0, max = 5000, value = 1000, step = 100),
          checkboxInput(session$ns("useLogScale"), "Use Log Scale for Visualization", value = TRUE)
        )
      }
    })
    
    # Live/Dead scatter plot display
    output$liveDeadScatterUI <- renderUI({
      if (!is.null(input$liveDeadMarkerSelect) && input$liveDeadMarkerSelect != "None") {
        shinydashboard::box(
          title = "2D Live/Dead Visualization", status = "success", solidHeader = TRUE,
          width = 12,
          plotOutput(session$ns("liveDeadScatter"), height = "400px")
        )
      }
    })
    
    # Cluster analysis results panel
    output$clusterAnalysisUI <- renderUI({
      if (!is.null(input[["clustering-showClusteringOptions"]]) &&
          input[["clustering-showClusteringOptions"]]) {
        tabsetPanel(
          tabPanel("Cluster Visualization",
                   br(),
                   div(style = "position: relative;",
                       plotlyOutput(session$ns("clusterPlot"), height = "600px"))
          ),
          tabPanel("Cluster Profiles",
                   br(),
                   shinycssloaders::withSpinner(plotOutput(session$ns("clusterHeatmap")))),
          tabPanel("Cluster Statistics",
                   br(),
                   DT::dataTableOutput(session$ns("clusterStats"))),
          tabPanel("Identified Populations",
                   br(),
                   DT::dataTableOutput(session$ns("populationTable")))
        )
      } else {
        div(class = "alert alert-info", style = "margin: 15px;",
            icon("info-circle"),
            h5("Clustering Analysis Disabled"),
            p("Enable clustering analysis in the sidebar to view cluster results.")
        )
      }
    })
    
    # ============================================================================
    # END OF SERVER-SIDE CONDITIONAL UI RENDERING
    # ============================================================================
    
    # ============================================================================
    # MAIN ANALYSIS — ASYNC (Change 2)
    # ============================================================================
    
    # Analysis is triggered explicitly by the Run button.
    # Heavy computation (t-SNE, UMAP, PCA, MDS) runs inside future_promise() so
    # the R session remains responsive during processing. Shiny drops connections
    # after ~90s of unresponsive session; the previous synchronous withProgress()
    # block was the direct cause of server disconnections.
    #
    # IMPORTANT: all input$* values must be captured as local variables BEFORE
    # entering future_promise(). Shiny inputs are not accessible inside the async
    # worker process — reading them there returns NULL silently.
    observeEvent(input$run, {
      req(input$selectedMarkers)
      
      # §6.1: capture file path/name before async -- the rawFCS() reactive is
      # not accessible inside future_promise(), and calling it here (rather
      # than deferring the actual read into the worker) would block the main
      # thread during file I/O for large files. The full read now happens
      # inside the future below.
      fcs_path <- input$fcsFile$datapath
      fcs_name <- input$fcsFile$name
      fcs_ext  <- tolower(tools::file_ext(fcs_name))

      # Pre-computation guard: warn before launching expensive algorithms on
      # large datasets. Uses metadata-only inspection (fcsEventCount(), §2.4
      # [H1]) or a cheap line count for csv/tsv -- never a full load -- so
      # this check itself does not block the main thread.
      n_events_total <- tryCatch({
        if (fcs_ext == "fcs") {
          fcsEventCount(fcs_path)
        } else {
          length(readLines(fcs_path, warn = FALSE)) - 1L
        }
      }, error = function(e) NA_integer_)

      if (!is.na(n_events_total) && n_events_total > 50000) {
        showNotification(
          paste0("Large dataset: ", format(n_events_total, big.mark = ","),
                 " events detected. Analysis may take several minutes."),
          type = "warning", duration = 8
        )
      }
      
      # Capture all inputs BEFORE entering async context
      # Server-side bounds enforce safe ranges regardless of UI constraints
      selected_markers <- input$selectedMarkers
      methods          <- input$methods
      do_transform     <- input$transform
      cofactor         <- max(1, min(1000, as.numeric(input$cofactor)))
      n_events_sample  <- max(100, min(500000, as.integer(input$nEvents)))
      perplexity       <- max(5, min(500, as.numeric(input$perplexity)))
      use_bh           <- isTRUE(input$use_barnes_hut)
      tsne_theta       <- if (use_bh) max(0, min(1, as.numeric(input$tsne_theta))) else 0.0
      tsne_max_iter    <- max(100, min(5000, as.integer(input$tsne_max_iter)))
      n_neighbors      <- max(2, min(200, as.integer(input$n_neighbors)))
      pca_components   <- max(2, min(50, as.integer(input$pca_components)))
      perform_qc       <- isTRUE(input$performQC)
      max_anomalies    <- max(0, min(100, as.numeric(input$maxAnomalies)))
      qc_sample_name   <- input$fcsFile$name
      qc_report_dir    <- qcReportDir
      
      showNotification("Analysis running...", id = "run_msg", duration = NULL)
      
      # Runs in a separate R process (plan(multisession) set in global.R).
      # The main Shiny session stays alive and responsive during computation.
      # All objects referenced inside must be self-contained — no reactive reads.
      future_promise(seed = TRUE, {
        
        tryCatch({
          
          # §6.1: read the file inside the worker, not the main thread.
          # readFCSchunked() (not flowCore::read.FCS() directly) is used for
          # FCS so the 500k-event stride-sampling cap (§2.4 [H2]) is
          # preserved; it returns list(ff = <flowFrame>, ...), so $ff is
          # extracted to match what rawFCS() returned previously.
          raw_data <- switch(fcs_ext,
            "fcs" = readFCSchunked(fcs_path)$ff,
            "csv" = data.table::fread(fcs_path),
            "tsv" = data.table::fread(fcs_path, sep = "\t"),
            stop("Unsupported file format: ", fcs_ext)
          )
          
          preprocessing_params <- list(
            markers              = selected_markers,
            transform            = do_transform,
            cofactor             = cofactor,
            n_events             = n_events_sample,
            perform_qc           = perform_qc,
            perform_gating       = FALSE,
            perform_compensation = FALSE,
            spillover_matrix     = NULL,
            scale_data           = TRUE,
            seed                 = 123,
            qc_settings          = if (perform_qc) list(
                                       max_anomalies = max_anomalies / 100,
                                       sample_name   = qc_sample_name,
                                       report_dir    = qc_report_dir,
                                       write_report  = TRUE
                                     ) else NULL
          )
          
          preprocessing_results <- preprocessFlowData(raw_data, preprocessing_params)
          
          results <- list(
            raw_data     = preprocessing_results$raw_data,
            sampled_data = preprocessing_results$sampled_data,
            scaled_data  = preprocessing_results$scaled_data,
            metrics      = preprocessing_results$metrics,
            markers      = selected_markers
          )
          
          if ("t-SNE" %in% methods) {
            set.seed(123)
            data_matrix <- as.matrix(preprocessing_results$scaled_data)
            tsne_out <- Rtsne::Rtsne(
              data_matrix,
              dims             = 2,
              perplexity       = perplexity,
              theta            = tsne_theta,
              max_iter         = tsne_max_iter,
              verbose          = FALSE,
              check_duplicates = FALSE
            )
            results$tsne <- data.frame(tsne1 = tsne_out$Y[, 1], tsne2 = tsne_out$Y[, 2])
            rm(tsne_out, data_matrix); gc(verbose = FALSE)
          }
          
          if ("UMAP" %in% methods) {
            set.seed(123)
            umap_out <- uwot::umap(preprocessing_results$scaled_data, n_neighbors = n_neighbors)
            results$umap <- data.frame(umap1 = umap_out[, 1], umap2 = umap_out[, 2])
            rm(umap_out); gc(verbose = FALSE)
          }
          
          if ("PCA" %in% methods) {
            pca_out <- prcomp(preprocessing_results$scaled_data, center = TRUE, scale. = TRUE)
            n_comp  <- min(pca_components, ncol(pca_out$x))
            results$pca <- as.data.frame(pca_out$x[, 1:n_comp])
            colnames(results$pca) <- paste0("pca", seq_len(n_comp))
            results$pca_summary   <- summary(pca_out)
            rm(pca_out); gc(verbose = FALSE)
          }
          
          if ("MDS" %in% methods) {
            # Hard cap MDS at 5000 events — distance matrix is O(N^2) in memory.
            # 50,000 events would require ~20GB RAM without this guard.
            mds_mat <- preprocessing_results$scaled_data
            if (nrow(mds_mat) > 5000) {
              mds_mat <- mds_mat[sample(nrow(mds_mat), 5000), ]
            }
            mds_out <- cmdscale(dist(mds_mat), k = 2)
            results$mds <- data.frame(mds1 = mds_out[, 1], mds2 = mds_out[, 2])
            rm(mds_out, mds_mat); gc(verbose = FALSE)
          }
          
          results
          
        }, error = function(e) {
          stop(paste("Computation error:", conditionMessage(e)))
        })
        
        # Promise success handler — executes back on the main Shiny thread once the
        # future completes. Only here is it safe to write to reactiveVal() and
        # update the UI. Do not move reactive writes inside future_promise() above.
      }) %...>% (function(results) {
        if (isSessionClosedGuard(session_closed)) {
          logDiscardedAfterSessionEnd(id, outcome = "success")
          return(invisible(NULL))
        }
        
        plot_data <- as.data.frame(results$sampled_data)
        colnames(plot_data) <- results$markers
        
        if (!is.null(results$tsne)) {
          plot_data$tsne1 <- results$tsne$tsne1
          plot_data$tsne2 <- results$tsne$tsne2
        }
        if (!is.null(results$umap)) {
          plot_data$umap1 <- results$umap$umap1
          plot_data$umap2 <- results$umap$umap2
        }
        if (!is.null(results$pca)) {
          for (col in colnames(results$pca)) plot_data[[col]] <- results$pca[[col]]
        }
        if (!is.null(results$mds)) {
          plot_data$mds1 <- results$mds$mds1
          plot_data$mds2 <- results$mds$mds2
        }
        
        results$plot_data <- plot_data
        
        # Preserve flowSet structure for downstream gating module compatibility
        if (inherits(results$raw_data, "flowFrame")) {
          results$raw_data <- flowSet(results$raw_data)
        }
        
        # Populate the QC summary panel from the pipeline metrics (2.2).
        qc_metrics <- results$metrics$qc
        if (!is.null(qc_metrics)) {
          qcState(list(
            metrics     = qc_metrics,
            status      = qc_metrics$status %||% QC_STATUS$PASS,
            error       = NULL,
            livedead    = results$metrics$livedead,
            sample_name = qc_metrics$sample_name %||% isolate(input$fcsFile$name),
            report_path = qc_metrics$report_path
          ))
        }

        processedData(results)
        removeNotification("run_msg")
        
        if (!is.null(results$metrics)) {
          qc_removed <- round(results$metrics$qc$removed_pct * 100, 1)
          showNotification(
            paste0("Analysis complete. QC removed ", qc_removed, "%. Analysing ",
                   nrow(results$sampled_data), " cells."),
            type = "message", duration = 5
          )
        } else {
          showNotification("Analysis complete.", type = "message", duration = 4)
        }
        
        # Promise error handler — catches any error thrown inside future_promise()
        # and surfaces it to the user. Without this, failures are silent and the
        # loading spinner spins indefinitely with no feedback.
      }) %...!% (function(err) {
        if (isSessionClosedGuard(session_closed)) {
          logDiscardedAfterSessionEnd(id, outcome = "failure", detail = conditionMessage(err))


          return(invisible(NULL))
        }
        removeNotification("run_msg")
        msg <- conditionMessage(err)

        # QC failures are recorded in the panel as well as notified, so the
        # reason persists after the toast disappears (6.4 No Silent Fallbacks).
        if (grepl("quality control|time-based QC|anomalous|QC failed",
                  msg, ignore.case = TRUE)) {
          cur <- qcState()
          cur$status <- QC_STATUS$ERROR
          cur$error  <- msg
          qcState(cur)
        }

        showNotification(
          paste("Analysis failed:", msg),
          type = "error", duration = 10
        )
      })
    })
    
    # ============================================================================
    # CLUSTERING MODULE
    # ============================================================================
    
    clustering_results <- clusteringModuleServer(
      "clustering",
      input_data = reactive({
        req(processedData())
        list(
          scaled_data = processedData()$scaled_data,
          markers     = input$selectedMarkers
        )
      }),
      app_state = app_state
    )
    
    # ============================================================================
    # OPTIMIZATION METRICS
    # ============================================================================
    
    output$optimizationMetricsUI <- renderUI({
      req(processedData())
      
      results <- processedData()
      metrics <- list()
      
      if (!is.null(results$tsne)) {
        tsne_kl <- tryCatch({
          original_dist  <- dist(scale(results$scaled_data))
          embedding_dist <- dist(as.matrix(results$tsne))
          original_dist  <- original_dist  / max(original_dist)
          embedding_dist <- embedding_dist / max(embedding_dist)
          mean((as.matrix(original_dist) - as.matrix(embedding_dist))^2)
        }, error = function(e) NA)
        
        metrics$tsne <- list(kl_divergence = round(tsne_kl, 4), perplexity = input$perplexity)
      }
      
      if (!is.null(results$umap)) {
        umap_trust <- tryCatch({
          original_dist  <- dist(scale(results$scaled_data))
          embedding_dist <- dist(as.matrix(results$umap))
          original_dist  <- original_dist  / max(original_dist)
          embedding_dist <- embedding_dist / max(embedding_dist)
          cor(as.vector(as.matrix(original_dist)), as.vector(as.matrix(embedding_dist)))
        }, error = function(e) NA)
        
        metrics$umap <- list(trustworthiness = round(umap_trust, 4), n_neighbors = input$n_neighbors)
      }
      
      if (!is.null(results$pca)) {
        pca_variance <- tryCatch({
          if (!is.null(results$pca_summary)) {
            sum(results$pca_summary$importance[2, 1:2])
          } else NA
        }, error = function(e) NA)
        
        metrics$pca <- list(variance_explained = round(pca_variance, 4), components = input$pca_components)
      }
      
      if (!is.null(results$mds)) {
        mds_stress <- tryCatch({
          original_dist  <- dist(scale(results$scaled_data))
          embedding_dist <- dist(as.matrix(results$mds))
          original_dist  <- original_dist  / max(original_dist)
          embedding_dist <- embedding_dist / max(embedding_dist)
          mean((as.matrix(original_dist) - as.matrix(embedding_dist))^2)
        }, error = function(e) NA)
        
        metrics$mds <- list(stress = round(mds_stress, 4), method = "Classical MDS")
      }
      
      metrics_ui <- tagList(hr(), h4("Optimization Metrics"),
                            p("These metrics help evaluate the quality of dimensionality reduction results."))
      
      if (!is.null(metrics$tsne)) {
        metrics_ui <- tagAppendChildren(metrics_ui, div(
          h5("t-SNE Metrics:"),
          p(paste("Perplexity:", metrics$tsne$perplexity)),
          p(paste("Approximate KL Divergence:", metrics$tsne$kl_divergence, "(lower is better)"))
        ))
      }
      
      if (!is.null(metrics$umap)) {
        metrics_ui <- tagAppendChildren(metrics_ui, div(
          h5("UMAP Metrics:"),
          p(paste("n_neighbors:", metrics$umap$n_neighbors)),
          p(paste("Distance Correlation:", metrics$umap$trustworthiness, "(higher is better)"))
        ))
      }
      
      if (!is.null(metrics$pca)) {
        metrics_ui <- tagAppendChildren(metrics_ui, div(
          h5("PCA Metrics:"),
          p(paste("Variance Explained:", metrics$pca$variance_explained, "(higher is better)")),
          p(paste("Components:", metrics$pca$components))
        ))
      }
      
      if (!is.null(metrics$mds)) {
        metrics_ui <- tagAppendChildren(metrics_ui, div(
          h5("MDS Metrics:"),
          p(paste("Stress (distance error):", metrics$mds$stress, "(lower is better)")),
          p(paste("Method:", metrics$mds$method))
        ))
      }
      
      if (!is.null(results$metrics)) {
        metrics_ui <- tagAppendChildren(
          metrics_ui,
          hr(),
          h4("Preprocessing Metrics"),
          div(
            h5("Quality Control:"),
            p(paste("Initial cells:", results$metrics$qc$initial_count)),
            p(paste("After QC:",     results$metrics$qc$final_count)),
            p(paste("Removed:", round(results$metrics$qc$removed_pct * 100, 1), "%"))
          ),
          div(
            h5("Sampling:"),
            p(paste("Final analysed cells:", nrow(results$sampled_data)))
          )
        )
      }
      
      return(metrics_ui)
    })
    
    # ============================================================================
    # PLOT RENDERING — DIMENSIONALITY REDUCTION
    #
    # Change 3: The two observe() blocks that called session$sendCustomMessage()
    # to force plot redraws on font/colour changes have been removed.
    # They caused every plot to re-render twice on any settings change —
    # once from sendCustomMessage and once from the reactive dependency on
    # app_state$plot_settings inside each renderPlotly.
    #
    # Change 4: Static theme values replace app_state$plot_settings reads.
    # Reading plot_settings inside renderPlotly made each plot a direct reactive
    # dependent of the settings system, causing re-renders on every font or
    # colour change regardless of whether the underlying data had changed.
    # All appearance values now come from get_plot_settings() in global.R —
    # one function call, one place to change values app-wide.
    # ============================================================================
    
    output$tsnePlot <- renderPlotly({
      req(processedData(), "t-SNE" %in% input$methods)
      
      plot_data <- processedData()$plot_data
      req("tsne1" %in% colnames(plot_data))
      
      plot_data_copy <- plot_data
      
      if (!is.null(clustering_results$clustering_results()) &&
          clustering_results$showClusteringOptions()) {
        plot_data_copy$Cluster <- as.factor(clustering_results$clustering_results()$cluster_ids)
        color_by <- "Cluster"
      } else {
        color_by <- NULL
      }
      
      p <- createDimReductionPlot(
        plot_data     = plot_data_copy,
        dim1          = "tsne1",
        dim2          = "tsne2",
        colorBy       = color_by,
        color_palette = ps$color_palette,
        point_size    = ps$point_size,
        font_size     = ps$font_size,
        title         = "t-SNE Projection",
        xlab          = "t-SNE 1",
        ylab          = "t-SNE 2"
      )
      
      ggplotly(p, width = ps$width, height = ps$height) %>%
        layout(
          font   = list(family = "Arial", size = ps$font_size, color = "black"),
          title  = list(text = "t-SNE Projection",
                        font = list(family = "Arial", size = ps$font_size * 1.2, color = "black")),
          xaxis  = list(title    = list(text = "t-SNE 1",
                                        font = list(family = "Arial", size = ps$font_size * 1.1)),
                        tickfont = list(family = "Arial", size = ps$font_size),
                        scaleanchor = "y", scaleratio = 1),
          yaxis  = list(title    = list(text = "t-SNE 2",
                                        font = list(family = "Arial", size = ps$font_size * 1.1)),
                        tickfont = list(family = "Arial", size = ps$font_size)),
          hoverlabel = list(bgcolor = "white",
                            font    = list(family = "Arial", size = ps$font_size * 0.9)),
          legend = if (!is.null(color_by)) list(
            title      = list(text = "Cluster",
                              font = list(family = "Arial", size = ps$font_size, color = "black")),
            font       = list(family = "Arial", size = ps$font_size * 0.9, color = "black"),
            bgcolor    = "rgba(255,255,255,0.9)",
            bordercolor = "rgba(0,0,0,0.2)",
            borderwidth = 1
          ) else list()
        )
    })
    
    output$umapPlot <- renderPlotly({
      req(processedData(), "UMAP" %in% input$methods)
      
      plot_data <- processedData()$plot_data
      req("umap1" %in% colnames(plot_data))
      
      plot_data_copy <- plot_data
      
      if (!is.null(clustering_results$clustering_results()) &&
          clustering_results$showClusteringOptions()) {
        plot_data_copy$Cluster <- as.factor(clustering_results$clustering_results()$cluster_ids)
        color_by <- "Cluster"
      } else {
        color_by <- NULL
      }
      
      p <- createDimReductionPlot(
        plot_data     = plot_data_copy,
        dim1          = "umap1",
        dim2          = "umap2",
        colorBy       = color_by,
        color_palette = ps$color_palette,
        point_size    = ps$point_size,
        font_size     = ps$font_size,
        title         = "UMAP Projection",
        xlab          = "UMAP 1",
        ylab          = "UMAP 2"
      )
      
      ggplotly(p, width = ps$width, height = ps$height) %>%
        layout(
          font   = list(family = "Arial", size = ps$font_size, color = "black"),
          title  = list(text = "UMAP Projection",
                        font = list(family = "Arial", size = ps$font_size * 1.2, color = "black")),
          xaxis  = list(title    = list(text = "UMAP 1",
                                        font = list(family = "Arial", size = ps$font_size * 1.1)),
                        tickfont = list(family = "Arial", size = ps$font_size),
                        scaleanchor = "y", scaleratio = 1),
          yaxis  = list(title    = list(text = "UMAP 2",
                                        font = list(family = "Arial", size = ps$font_size * 1.1)),
                        tickfont = list(family = "Arial", size = ps$font_size)),
          hoverlabel = list(bgcolor = "white",
                            font    = list(family = "Arial", size = ps$font_size * 0.9)),
          legend = if (!is.null(color_by)) list(
            title      = list(text = "Cluster",
                              font = list(family = "Arial", size = ps$font_size, color = "black")),
            font       = list(family = "Arial", size = ps$font_size * 0.9, color = "black"),
            bgcolor    = "rgba(255,255,255,0.9)",
            bordercolor = "rgba(0,0,0,0.2)",
            borderwidth = 1
          ) else list()
        )
    })
    
    output$pcaPlot <- renderPlotly({
      req(processedData(), "PCA" %in% input$methods)
      
      plot_data <- processedData()$plot_data
      req("pca1" %in% colnames(plot_data))
      
      plot_data_copy <- plot_data
      
      if (!is.null(clustering_results$clustering_results()) &&
          clustering_results$showClusteringOptions()) {
        plot_data_copy$Cluster <- as.factor(clustering_results$clustering_results()$cluster_ids)
        color_by <- "Cluster"
      } else {
        color_by <- NULL
      }
      
      p <- createDimReductionPlot(
        plot_data     = plot_data_copy,
        dim1          = "pca1",
        dim2          = "pca2",
        colorBy       = color_by,
        color_palette = ps$color_palette,
        point_size    = ps$point_size,
        font_size     = ps$font_size,
        title         = "PCA Projection",
        xlab          = "PC 1",
        ylab          = "PC 2"
      )
      
      ggplotly(p, width = ps$width, height = ps$height) %>%
        layout(
          font   = list(family = "Arial", size = ps$font_size, color = "black"),
          title  = list(text = "PCA Projection",
                        font = list(family = "Arial", size = ps$font_size * 1.2, color = "black")),
          xaxis  = list(title    = list(text = "PC 1",
                                        font = list(family = "Arial", size = ps$font_size * 1.1)),
                        tickfont = list(family = "Arial", size = ps$font_size),
                        scaleanchor = "y", scaleratio = 1),
          yaxis  = list(title    = list(text = "PC 2",
                                        font = list(family = "Arial", size = ps$font_size * 1.1)),
                        tickfont = list(family = "Arial", size = ps$font_size)),
          hoverlabel = list(bgcolor = "white",
                            font    = list(family = "Arial", size = ps$font_size * 0.9)),
          legend = if (!is.null(color_by)) list(
            title      = list(text = "Cluster",
                              font = list(family = "Arial", size = ps$font_size, color = "black")),
            font       = list(family = "Arial", size = ps$font_size * 0.9, color = "black"),
            bgcolor    = "rgba(255,255,255,0.9)",
            bordercolor = "rgba(0,0,0,0.2)",
            borderwidth = 1
          ) else list()
        )
    })
    
    output$mdsPlot <- renderPlotly({
      req(processedData(), "MDS" %in% input$methods)
      
      plot_data <- processedData()$plot_data
      req("mds1" %in% colnames(plot_data))
      
      plot_data_copy <- plot_data
      
      if (!is.null(clustering_results$clustering_results()) &&
          clustering_results$showClusteringOptions()) {
        plot_data_copy$Cluster <- as.factor(clustering_results$clustering_results()$cluster_ids)
        color_by <- "Cluster"
      } else {
        color_by <- NULL
      }
      
      p <- createDimReductionPlot(
        plot_data     = plot_data_copy,
        dim1          = "mds1",
        dim2          = "mds2",
        colorBy       = color_by,
        color_palette = ps$color_palette,
        point_size    = ps$point_size,
        font_size     = ps$font_size,
        title         = "MDS Projection",
        xlab          = "MDS 1",
        ylab          = "MDS 2"
      )
      
      ggplotly(p, width = ps$width, height = ps$height) %>%
        layout(
          font   = list(family = "Arial", size = ps$font_size, color = "black"),
          title  = list(text = "MDS Projection",
                        font = list(family = "Arial", size = ps$font_size * 1.2, color = "black")),
          xaxis  = list(title    = list(text = "MDS 1",
                                        font = list(family = "Arial", size = ps$font_size * 1.1)),
                        tickfont = list(family = "Arial", size = ps$font_size),
                        scaleanchor = "y", scaleratio = 1),
          yaxis  = list(title    = list(text = "MDS 2",
                                        font = list(family = "Arial", size = ps$font_size * 1.1)),
                        tickfont = list(family = "Arial", size = ps$font_size)),
          hoverlabel = list(bgcolor = "white",
                            font    = list(family = "Arial", size = ps$font_size * 0.9)),
          legend = if (!is.null(color_by)) list(
            title      = list(text = "Cluster",
                              font = list(family = "Arial", size = ps$font_size, color = "black")),
            font       = list(family = "Arial", size = ps$font_size * 0.9, color = "black"),
            bgcolor    = "rgba(255,255,255,0.9)",
            bordercolor = "rgba(0,0,0,0.2)",
            borderwidth = 1
          ) else list()
        )
    })
    
    # ============================================================================
    # MARKER EXPRESSION HEATMAP FUNCTIONALITY
    # ============================================================================
    
    # Update marker dropdown when data or name mappings change
    observe({
      req(processedData())
      markers        <- processedData()$markers
      marker_choices <- getMarkerChoicesWithDisplayNames(markers, markerNameMappings())
      updateSelectInput(session, "heatmapMarker",
                        choices  = marker_choices,
                        selected = if (length(markers) > 0) markers[1] else NULL)
    })
    
    observe({
      req(processedData())
      markerNameMappings()
      markers           <- processedData()$markers
      current_selection <- input$heatmapMarker
      marker_choices    <- getMarkerChoicesWithDisplayNames(markers, markerNameMappings())
      updateSelectInput(session, "heatmapMarker",
                        choices = marker_choices, selected = current_selection)
    })
    
    # Update dimensionality method dropdown when data changes
    observe({
      req(processedData())
      plot_data        <- processedData()$plot_data
      available_methods <- getAvailableDimMethods(plot_data)
      updateSelectInput(session, "heatmapDimMethod",
                        choices  = names(available_methods),
                        selected = if (length(available_methods) > 0) names(available_methods)[1] else NULL)
    })
    
    # Individual marker heatmap (interactive plotly)
    output$markerHeatmapPlot <- renderPlotly({
      req(processedData(), input$heatmapMarker, input$heatmapDimMethod)
      
      plot_data         <- processedData()$plot_data
      available_methods <- getAvailableDimMethods(plot_data)
      if (!input$heatmapDimMethod %in% names(available_methods)) return(NULL)
      
      dim_coords <- available_methods[[input$heatmapDimMethod]]
      dim_labels <- getDimAxisLabels(input$heatmapDimMethod)
      opt_data   <- optimizeHeatmapRendering(plot_data)
      bins_value <- if (!is.null(input$heatmapBins)) input$heatmapBins else 50
      
      p <- createMarkerExpressionHeatmap(
        plot_data             = opt_data,
        marker                = input$heatmapMarker,
        dim1                  = dim_coords[1],
        dim2                  = dim_coords[2],
        method                = input$heatmapMethod,
        bins                  = bins_value,
        title                 = paste(input$heatmapMarker, "Expression on", input$heatmapDimMethod),
        font_size             = ps$font_size,
        color_palette         = input$heatmapColorPalette,
        use_qualitative_labels = (input$legendStyle == "qualitative"),
        label_style           = if (input$legendStyle == "qualitative") input$labelStyle else "standard"
      )
      
      p <- p + labs(x = dim_labels[1], y = dim_labels[2])
      
      ggplotly(p, width = ps$width, height = ps$height) %>%
        layout(
          title  = list(text = paste(input$heatmapMarker, "Expression on", input$heatmapDimMethod),
                        font = list(family = "Arial", size = ps$font_size * 1.2, color = "black")),
          xaxis  = list(title    = list(text = dim_labels[1],
                                        font = list(family = "Arial", size = ps$font_size * 1.1)),
                        tickfont = list(family = "Arial", size = ps$font_size)),
          yaxis  = list(title    = list(text = dim_labels[2],
                                        font = list(family = "Arial", size = ps$font_size * 1.1)),
                        tickfont = list(family = "Arial", size = ps$font_size)),
          hoverlabel = list(bgcolor = "white",
                            font    = list(family = "Arial", size = ps$font_size * 0.9))
        )
    })
    
    # Individual marker heatmap (static, used in individual view)
    output$markerHeatmapPlotStatic <- renderPlot({
      req(processedData(), input$heatmapMarker, input$heatmapDimMethod)
      
      plot_data         <- processedData()$plot_data
      available_methods <- getAvailableDimMethods(plot_data)
      if (!input$heatmapDimMethod %in% names(available_methods)) return(NULL)
      
      dim_coords          <- available_methods[[input$heatmapDimMethod]]
      dim_labels          <- getDimAxisLabels(input$heatmapDimMethod)
      opt_data            <- optimizeHeatmapRendering(plot_data)
      bins_value          <- if (!is.null(input$heatmapBins)) input$heatmapBins else 50
      marker_display_name <- getMarkerDisplayName(input$heatmapMarker, markerNameMappings())
      
      p <- createMarkerExpressionHeatmap(
        plot_data             = opt_data,
        marker                = input$heatmapMarker,
        dim1                  = dim_coords[1],
        dim2                  = dim_coords[2],
        method                = input$heatmapMethod,
        bins                  = bins_value,
        title                 = paste(marker_display_name, "Expression on", input$heatmapDimMethod),
        font_size             = ps$font_size,
        color_palette         = input$heatmapColorPalette,
        use_qualitative_labels = (input$legendStyle == "qualitative"),
        label_style           = if (input$legendStyle == "qualitative") input$labelStyle else "standard"
      )
      
      p + labs(x = dim_labels[1], y = dim_labels[2])
      
    }, width = function() ps$width, height = function() ps$height)
    
    smart_analysis_results <- reactiveVal(NULL)
    heatmap_display_mode   <- reactiveVal("individual")
    
    observeEvent(input$generateAllHeatmapsFast, {
      req(processedData(), input$heatmapDimMethod)
      heatmap_display_mode("grid")
      showNotification("Switched to fast grid view", type = "message", duration = 3)
    })
    
    observeEvent(input$heatmapMarker, {
      if (heatmap_display_mode() == "grid") heatmap_display_mode("individual")
    })
    
    output$heatmapMainDisplay <- renderUI({
      if (heatmap_display_mode() == "grid") {
        tagList(
          div(class = "alert alert-success", style = "margin-bottom: 10px;",
              icon("lightning-bolt"), " Fast Grid Mode: All markers rendered simultaneously"),
          actionButton(session$ns("backToIndividual"), "<- Back to Individual View",
                       class = "btn-secondary btn-sm", style = "margin-bottom: 15px;"),
          shinycssloaders::withSpinner(uiOutput(session$ns("ultraFastGrid")))
        )
      } else {
        tagList(
          shinycssloaders::withSpinner(
            plotOutput(session$ns("markerHeatmapPlotStatic"), height = "600px")
          )
        )
      }
    })
    
    observeEvent(input$backToIndividual, {
      heatmap_display_mode("individual")
    })
    
    output$ultraFastGrid <- renderUI({
      req(processedData(), input$heatmapDimMethod, heatmap_display_mode() == "grid")
      
      markers           <- processedData()$markers
      plot_data         <- processedData()$plot_data
      available_methods <- getAvailableDimMethods(plot_data)
      dim_coords        <- available_methods[[input$heatmapDimMethod]]
      dim_labels        <- getDimAxisLabels(input$heatmapDimMethod)
      bins_value        <- if (!is.null(input$heatmapBins)) input$heatmapBins else 30
      
      plot_list <- lapply(seq_along(markers), function(i) {
        marker              <- markers[i]
        plot_id             <- paste0("ultraFastHeatmap_", i)
        marker_display_name <- getMarkerDisplayName(marker, markerNameMappings())
        
        output[[plot_id]] <- renderPlot({
          opt_data <- optimizeHeatmapRendering(plot_data)
          
          p <- createMarkerExpressionHeatmap(
            plot_data             = opt_data,
            marker                = marker,
            dim1                  = dim_coords[1],
            dim2                  = dim_coords[2],
            method                = input$heatmapMethod,
            bins                  = bins_value,
            title                 = marker_display_name,
            font_size             = 18,
            color_palette         = input$heatmapColorPalette,
            use_qualitative_labels = (input$legendStyle == "qualitative"),
            label_style           = if (input$legendStyle == "qualitative") input$labelStyle else "standard"
          )
          
          p + labs(x = dim_labels[1], y = dim_labels[2]) +
            theme(plot.title  = element_text(size = 18, face = "bold"),
                  axis.title  = element_text(size = 15),
                  axis.text   = element_text(size = 14),
                  legend.title = element_text(size = 12),
                  legend.text  = element_text(size = 10))
        })
        
        column(6, plotOutput(session$ns(plot_id), height = "500px"))
      })
      
      download_section <- fluidRow(
        column(12,
               div(class = "text-center", style = "margin-bottom: 20px;",
                   downloadButton(session$ns("downloadGridPlots"), "Download All Plots as PNG",
                                  class = "btn-success btn-lg", icon = icon("download"),
                                  style = "margin-bottom: 15px;"),
                   br(),
                   tags$small(class = "text-muted",
                              paste("Will save", length(markers),
                                    "individual plots + 1 combined overview as high-resolution PNG files"))
               )
        )
      )
      
      grid_rows <- list()
      for (i in seq(1, length(plot_list), by = 2)) {
        row_plots <- plot_list[i:min(i + 1, length(plot_list))]
        grid_rows[[length(grid_rows) + 1]] <- fluidRow(do.call(tagList, row_plots))
      }
      
      do.call(tagList, c(list(download_section), grid_rows))
    })
    
    output$downloadGridPlots <- downloadHandler(
      filename = function() {
        paste0("heatmap_grid_plots_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        req(processedData(), input$heatmapDimMethod)
        
        # Framework 2.1 note: this is intentionally NOT wrapped in
        # future_promise(). Shiny's downloadHandler() content() function must
        # write synchronously to `file` and return before the HTTP response
        # can be sent -- it cannot await a promise. This loop is bounded by
        # the number of markers selected (typically well under the ~90s
        # disconnect timeout); the guard below warns the user if the marker
        # count makes that unlikely.
        if (length(processedData()$markers) > 40) {
          showNotification(
            paste0("Generating ", length(processedData()$markers),
                   " marker plots -- this may take a while and cannot run in the background."),
            type = "warning", duration = 8
          )
        }
        
        withProgress(message = "Generating high-resolution plots...", value = 0, {
          
          markers           <- processedData()$markers
          plot_data         <- processedData()$plot_data
          available_methods <- getAvailableDimMethods(plot_data)
          dim_coords        <- available_methods[[input$heatmapDimMethod]]
          dim_labels        <- getDimAxisLabels(input$heatmapDimMethod)
          bins_value        <- if (!is.null(input$heatmapBins)) input$heatmapBins else 30
          opt_data          <- optimizeHeatmapRendering(plot_data)
          
          png_dir <- file.path(tempdir(), "heatmap_plots")
          if (dir.exists(png_dir)) unlink(png_dir, recursive = TRUE)
          dir.create(png_dir, recursive = TRUE)
          
          png_files        <- character()
          individual_plots <- list()
          
          for (i in seq_along(markers)) {
            incProgress(0.8 / length(markers),
                        detail = paste("Generating plot", i, "of", length(markers)))
            
            marker              <- markers[i]
            marker_display_name <- getMarkerDisplayName(marker, markerNameMappings())
            safe_name           <- gsub("[^A-Za-z0-9._-]", "_", marker_display_name)
            png_filename        <- file.path(png_dir, paste0(sprintf("%02d", i), "_", safe_name, ".png"))
            
            tryCatch({
              p <- createMarkerExpressionHeatmap(
                plot_data             = opt_data,
                marker                = marker,
                dim1                  = dim_coords[1],
                dim2                  = dim_coords[2],
                method                = input$heatmapMethod,
                bins                  = bins_value,
                title                 = marker_display_name,
                font_size             = 18,
                color_palette         = input$heatmapColorPalette,
                use_qualitative_labels = (input$legendStyle == "qualitative"),
                label_style           = if (input$legendStyle == "qualitative") input$labelStyle else "standard"
              )
              
              p_final <- p + labs(x = dim_labels[1], y = dim_labels[2]) +
                theme(plot.title        = element_text(size = 18, face = "bold"),
                      axis.title        = element_text(size = 15),
                      axis.text         = element_text(size = 14),
                      legend.title      = element_text(size = 12),
                      legend.text       = element_text(size = 10),
                      panel.background  = element_rect(fill = "white", color = NA),
                      plot.background   = element_rect(fill = "white", color = NA),
                      legend.background = element_rect(fill = "white", color = NA))
              
              ggsave(png_filename, plot = p_final,
                     width = 10, height = 8, dpi = 300, device = "png", bg = "white")
              
              png_files           <- c(png_files, png_filename)
              individual_plots[[i]] <- p_final
              
            }, error = function(e) {
              message("Error generating plot for ", marker, ": ", e$message)
            })
          }
          
          incProgress(0.15, detail = "Creating combined overview plot...")
          
          valid_plots <- individual_plots[!sapply(individual_plots, is.null)]
          if (length(valid_plots) > 0) {
            tryCatch({
              n_cols           <- 4
              n_rows           <- ceiling(length(valid_plots) / n_cols)
              combined_filename <- file.path(png_dir, "00_COMBINED_All_Markers.png")
              
              combined_plot <- gridExtra::grid.arrange(grobs = valid_plots, ncol = n_cols)
              ggsave(combined_filename, plot = combined_plot,
                     width = n_cols * 8, height = n_rows * 6,
                     dpi = 300, device = "png", bg = "white")
              
              png_files <- c(combined_filename, png_files)
            }, error = function(e) message("Error creating combined plot: ", e$message))
          }
          
          if (length(png_files) > 0) {
            old_wd <- setwd(png_dir)
            on.exit(setwd(old_wd))
            zip(file, files = basename(png_files), flags = "-r9X")
            
            has_combined    <- any(grepl("COMBINED", basename(png_files)))
            individual_count <- length(png_files) - ifelse(has_combined, 1, 0)
            
            showNotification(
              paste("Exported", individual_count, "plots",
                    if (has_combined) "+ 1 combined overview" else ""),
              type = "message", duration = 6
            )
          } else {
            writeLines("No plots could be generated", file)
            showNotification("No plots could be generated", type = "error")
          }
        })
      }
    )
    
    # ============================================================================
    # CLUSTER VISUALIZATION
    # ============================================================================
    
    output$clusterPlot <- renderPlotly({
      req(processedData())
      
      if (mergeHistory()$active) {
        cluster_data   <- list(
          cluster_ids = mergeHistory()$current_clusters,
          method      = paste(clustering_results$clustering_results()$method,
                              "(", length(mergeHistory()$operations), " merges)")
        )
        population_data <- mergeHistory()$current_mapping
      } else if (!is.null(clustering_results$clustering_results())) {
        cluster_data    <- clustering_results$clustering_results()
        population_data <- clustering_results$populations()
      } else {
        return(NULL)
      }
      
      plot_data         <- processedData()$plot_data
      plot_data$Cluster <- as.factor(cluster_data$cluster_ids)
      
      if (!is.null(population_data) && clustering_results$showPopulationLabels()) {
        population_map     <- setNames(population_data$Population,
                                       as.character(population_data$Cluster))
        plot_data$Population <- population_map[as.character(plot_data$Cluster)]
        color_by <- "Population"
      } else {
        color_by <- "Cluster"
      }
      
      if ("tsne1" %in% colnames(plot_data)) {
        dim1 <- "tsne1"; dim2 <- "tsne2"; dim_labels <- c("t-SNE 1", "t-SNE 2")
      } else if ("umap1" %in% colnames(plot_data)) {
        dim1 <- "umap1"; dim2 <- "umap2"; dim_labels <- c("UMAP 1", "UMAP 2")
      } else if ("pca1" %in% colnames(plot_data)) {
        dim1 <- "pca1"; dim2 <- "pca2"; dim_labels <- c("PC 1", "PC 2")
      } else if ("mds1" %in% colnames(plot_data)) {
        dim1 <- "mds1"; dim2 <- "mds2"; dim_labels <- c("MDS 1", "MDS 2")
      } else {
        dim1 <- input$selectedMarkers[1]; dim2 <- input$selectedMarkers[2]
        dim_labels <- input$selectedMarkers[1:2]
      }
      
      p <- ggplot(plot_data, aes(x = .data[[dim1]], y = .data[[dim2]], color = .data[[color_by]])) +
        geom_point(alpha = 0.7, size = ps$point_size / 2) +
        getDiscreteColorScale(ps$color_palette) +
        labs(title = paste("Clusters from", cluster_data$method),
             x     = dim_labels[1],
             y     = dim_labels[2],
             color = if (color_by == "Population") "Cell Population" else "Cluster") +
        get_standard_theme(ps$font_size)
      
      p_plotly <- ggplotly(p, tooltip = c("color", "x", "y")) %>%
        layout(
          legend     = list(title      = list(text = if (color_by == "Population") "Cell Population" else "Cluster",
                                              font = list(size = ps$font_size, family = "Arial")),
                            font       = list(size = ps$font_size * 0.9, family = "Arial"),
                            bgcolor    = "rgba(255,255,255,0.9)",
                            bordercolor = "rgba(0,0,0,0.2)",
                            borderwidth = 1),
          hoverlabel = list(bgcolor = "white",
                            font    = list(family = "Arial", size = ps$font_size * 0.9)),
          width  = ps$width,
          height = ps$height
        )
      
      if (color_by == "Population" && clustering_results$showPopulationLabels() &&
          clustering_results$showClusterLabels()) {
        
        cluster_centers <- plot_data %>%
          group_by(Cluster, Population) %>%
          summarize(x = mean(.data[[dim1]], na.rm = TRUE),
                    y = mean(.data[[dim2]], na.rm = TRUE),
                    .groups = "drop")
        
        for (i in seq_len(nrow(cluster_centers))) {
          p_plotly <- p_plotly %>% add_annotations(
            x = cluster_centers$x[i], y = cluster_centers$y[i],
            text = cluster_centers$Population[i],
            showarrow = TRUE, arrowhead = 0.5, arrowsize = 0.5, arrowwidth = 1,
            ax = 20, ay = -20,
            bgcolor = "rgba(255,255,255,0.8)", bordercolor = "rgba(0,0,0,0.5)",
            borderwidth = 1, font = list(size = ps$font_size)
          )
        }
      }
      
      return(p_plotly)
    })
    
    output$clusterHeatmap <- renderPlot({
      req(clustering_results$clustering_results())
      
      centers <- clustering_results$clustering_results()$centers
      method  <- clustering_results$clustering_results()$method
      
      population_data <- if (mergeHistory()$active) mergeHistory()$current_mapping else
        clustering_results$populations()
      
      if (mergeHistory()$active) {
        new_centers <- centers
        for (op in mergeHistory()$operations) {
          merged_id   <- op$target_cluster
          merged_from <- op$merged_clusters
          if (length(merged_from) > 1) {
            cluster_counts  <- table(clustering_results$clustering_results()$cluster_ids)
            merged_weights  <- cluster_counts[merged_from]
            for (col in seq_len(ncol(centers))) {
              values <- new_centers[as.character(merged_from), col]
              weights <- merged_weights / sum(merged_weights)
              new_centers[as.character(merged_id), col] <- sum(values * weights, na.rm = TRUE)
            }
            new_centers <- new_centers[
              !(rownames(new_centers) %in% setdiff(as.character(merged_from), as.character(merged_id))), ]
          }
        }
        centers <- new_centers
        method  <- paste(method, "(", length(mergeHistory()$operations), " merges)")
      }
      
      createClusterHeatmap(centers = centers, method = method,
                           title = "Cluster Intensity Profiles",
                           font_size = ps$font_size, population_data = population_data)
      
    }, width = function() ps$width, height = function() ps$height)
    
    # ============================================================================
    # CLUSTER STATISTICS AND DOWNLOAD
    # ============================================================================
    
    output$clusterStats <- DT::renderDataTable({
      req(processedData())
      
      if (mergeHistory()$active) {
        cluster_ids     <- mergeHistory()$current_clusters
        population_data <- mergeHistory()$current_mapping
        method_name     <- paste(clustering_results$clustering_results()$method,
                                 "(", length(mergeHistory()$operations), " merges)")
      } else if (!is.null(clustering_results$clustering_results())) {
        cluster_ids     <- clustering_results$clustering_results()$cluster_ids
        population_data <- clustering_results$populations()
        method_name     <- clustering_results$clustering_results()$method
      } else {
        return(NULL)
      }
      
      stats_df <- formatClusterStats(cluster_ids   = cluster_ids,
                                     total_cells   = nrow(processedData()$plot_data),
                                     population_data = population_data)
      
      DT::datatable(stats_df, options = list(scrollX = TRUE, pageLength = 10),
                    caption = paste("Cluster statistics from", method_name)) %>%
        formatRound(columns = c("Percentage", "Confidence"), digits = 2)
    })
    
    output$populationTable <- DT::renderDataTable({
      req(!is.null(clustering_results$populations()))
      
      population_data <- if (mergeHistory()$active) {
        pd <- mergeHistory()$current_mapping
        if (length(mergeHistory()$operations) > 0) {
          pd <- rbind(pd, data.frame(Cluster = "---", Population = "--- Merge History ---",
                                     Confidence = NA))
          for (op in mergeHistory()$operations) {
            pd <- rbind(pd, data.frame(
              Cluster    = op$target_cluster,
              Population = paste0(op$new_name, " (from: ", paste(op$merged_clusters, collapse = ", "), ")"),
              Confidence = NA
            ))
          }
        }
        pd
      } else {
        clustering_results$populations()
      }
      
      DT::datatable(population_data, options = list(pageLength = 10, scrollX = TRUE),
                    caption = "Identified Cell Populations") %>%
        formatRound(columns = "Confidence", digits = 2)
    })
    
    output$downloadClusterTable <- downloadHandler(
      filename = function() paste0("cluster_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content  = function(file) {
        req(processedData())
        
        if (mergeHistory()$active) {
          cluster_ids     <- mergeHistory()$current_clusters
          population_data <- mergeHistory()$current_mapping
        } else if (!is.null(clustering_results$clustering_results())) {
          cluster_ids     <- clustering_results$clustering_results()$cluster_ids
          population_data <- clustering_results$populations()
        } else return(NULL)
        
        stats_df <- formatClusterStats(cluster_ids   = cluster_ids,
                                       total_cells   = nrow(processedData()$plot_data),
                                       population_data = population_data)
        write.csv(stats_df, file, row.names = FALSE)
      }
    )
    
    output$downloadProcessedData <- downloadHandler(
      filename = function() paste0("processed_data_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content  = function(file) {
        req(processedData())
        data <- processedData()$plot_data
        
        if (mergeHistory()$active) {
          data$Cluster <- as.factor(mergeHistory()$current_clusters)
          if (!is.null(mergeHistory()$current_mapping)) {
            pop_mapping  <- setNames(mergeHistory()$current_mapping$Population,
                                     mergeHistory()$current_mapping$Cluster)
            data$Population <- pop_mapping[as.character(data$Cluster)]
          }
        } else if (!is.null(clustering_results$clustering_results())) {
          data$Cluster <- as.factor(clustering_results$clustering_results()$cluster_ids)
          if (!is.null(clustering_results$populations())) {
            pop_mapping  <- setNames(clustering_results$populations()$Population,
                                     clustering_results$populations()$Cluster)
            data$Population <- pop_mapping[as.character(data$Cluster)]
          }
        }
        
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    # ============================================================================
    # CLUSTER MERGING
    # ============================================================================
    
    observeEvent(input$clustering$showMergeModal, {
      req(clustering_results$clustering_results())
      
      current_clusters <- if (mergeHistory()$active) mergeHistory()$current_clusters else
        clustering_results$clustering_results()$cluster_ids
      population_data  <- if (mergeHistory()$active) mergeHistory()$current_mapping else
        clustering_results$populations()
      
      unique_clusters <- unique(current_clusters)
      
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
        checkboxGroupInput(session$ns("clustersToMerge"), "Select clusters to merge:",
                           choices = cluster_choices),
        textInput(session$ns("mergedClusterName"), "Name for merged cluster:",
                  value = "Merged Population"),
        tags$div(class = "help-block",
                 tags$b("Note:"),
                 "This merge is cumulative. Use 'Reset to Original Clusters' to start over."),
        footer = tagList(
          actionButton(session$ns("performMerge"), "Merge Clusters", class = "btn-success"),
          modalButton("Cancel")
        ),
        size = "l"
      ))
    })
    
    observeEvent(input$performMerge, {
      req(input$clustersToMerge, length(input$clustersToMerge) >= 2)
      
      current_clusters <- if (mergeHistory()$active) mergeHistory()$current_clusters else
        clustering_results$clustering_results()$cluster_ids
      current_mapping  <- if (mergeHistory()$active) mergeHistory()$current_mapping else
        clustering_results$populations()
      
      new_clusters <- current_clusters
      merged_id    <- min(as.numeric(input$clustersToMerge))
      
      for (cluster_id in input$clustersToMerge) {
        new_clusters[current_clusters == cluster_id] <- merged_id
      }
      
      new_mapping <- current_mapping
      new_mapping$Population[new_mapping$Cluster == merged_id] <- input$mergedClusterName
      new_mapping <- new_mapping[
        !(new_mapping$Cluster %in% setdiff(input$clustersToMerge, merged_id)), ]
      
      current_history                   <- mergeHistory()
      current_history$active            <- TRUE
      current_history$current_clusters  <- new_clusters
      current_history$current_mapping   <- new_mapping
      current_history$operations        <- c(current_history$operations, list(list(
        timestamp       = Sys.time(),
        merged_clusters = input$clustersToMerge,
        target_cluster  = merged_id,
        new_name        = input$mergedClusterName
      )))
      
      mergeHistory(current_history)
      removeModal()
      
      showNotification(
        paste0("Merged into '", input$mergedClusterName, "'. ",
               length(unique(new_clusters)), " clusters remaining (",
               length(current_history$operations), " merge operations total)."),
        type = "default", duration = 5
      )
    })
    
    observeEvent(input$clustering$resetMerging, {
      mergeHistory(list(active = FALSE, operations = list(),
                        current_clusters = NULL, current_mapping = NULL))
      showNotification("Restored original clusters", type = "default", duration = 3)
    })
    
    # ============================================================================
    # MODULE RETURN
    # ============================================================================
    
    return(list(
      processed_data     = processedData,
      clustering_results = clustering_results
    ))
  })   # closes moduleServer(id, function(input, output, session) {
}      # closes rawDataModuleServer <- function(id, app_state) {
    
    
