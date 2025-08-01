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
          
          # Marker Selection UI
          uiOutput(ns("markerSelectUI"))
        ),
        
        # # Spillover Compensation Section
        # shinydashboard::box(
        #   title = "Spillover Compensation", status = "warning", solidHeader = TRUE,
        #   width = 12, collapsible = TRUE, collapsed = TRUE,
        #   
        #   checkboxInput(ns("enableCompensation"), "Enable Spillover Compensation", value = FALSE),
        #   
        #   # Replace conditionalPanel with server-side rendering
        #   uiOutput(ns("compensationSettingsUI"))
        # ),
        
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
          
          # Gating options - COMMENTED OUT: Using dedicated gating module now
          # div(class = "parameter-group",
          #   h5(icon("filter"), "Cell Gating"),
          #   checkboxInput(ns("performGating"), "Perform Debris/Dead Cell Gating", value = TRUE),
          #   # Replace conditionalPanel with server-side rendering
          #   uiOutput(ns("gatingSettingsUI"))
          # )
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
        
        # # Spillover Compensation Analysis
        # shinydashboard::box(
        #   title = "Spillover Compensation Analysis", status = "warning", solidHeader = TRUE,
        #   width = 12, collapsible = TRUE, collapsed = TRUE,
        #   
        #   # Replace conditionalPanels with server-side rendering
        #   uiOutput(ns("compensationAnalysisUI"))
        # ),
        
        # # Live/Dead Analysis
        # shinydashboard::box(
        #   title = "Live/Dead Cell Analysis", status = "success", solidHeader = TRUE,
        #   width = 12, collapsible = TRUE, collapsed = TRUE,
          
        #   fluidRow(
        #     column(8, 
        #       shinydashboard::box(
        #         title = "Live/Dead Marker Distribution", status = "info", solidHeader = TRUE,
        #         width = 12,
        #         shinycssloaders::withSpinner(plotOutput(ns("liveDeadHistogram"), height = "400px"))
        #       )
        #     ),
        #     column(4, 
        #       shinydashboard::box(
        #         title = "Gating Controls", status = "warning", solidHeader = TRUE,
        #         width = 12,
        #         uiOutput(ns("liveDeadMarkerUI")),
        #         # Replace conditionalPanel with server-side rendering
        #         uiOutput(ns("liveDeadControlsUI"))
        #       )
        #     )
        #   ),
        #   # Replace conditionalPanel with server-side rendering
        #   uiOutput(ns("liveDeadScatterUI"))
        # ),
        
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
        cat("\nParameters (Parameter Name -> Marker Description):\n")
        cat(paste(rep("=", 60), collapse = ""), "\n")
        
        params <- parameters(fcs)
        param_info <- data.frame(
          Parameter = params$name,
          Description = params$desc,
          stringsAsFactors = FALSE
        )
        
        # Clean up descriptions and show mapping
        for (i in 1:nrow(param_info)) {
          param_name <- param_info$Parameter[i]
          param_desc <- param_info$Description[i]
          
          if (is.na(param_desc) || param_desc == "") {
            param_desc <- "(No description)"
          }
          
          # Highlight fluorescence channels
          if (grepl("^(FSC|SSC|Time|Width|Height)", param_name)) {
            cat(sprintf("  %-8s -> %s (Scatter/Time)\n", param_name, param_desc))
          } else {
            cat(sprintf("  %-8s -> %s *** FLUORESCENCE ***\n", param_name, param_desc))
          }
        }
        
        # cat("\nNote: For spillover compensation, use the fluorescence channels marked above.\n")  # COMMENTED OUT: Using dedicated compensation module
        cat("The parameter names ($P1N, $P2N, etc.) are internal identifiers.\n")
        cat("The descriptions show the actual marker names (e.g., CD3-FITC, CD4-PE).\n")
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
        fcs_data <- rawFCS()
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
        exprs_data <- rawFCS()
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
    
    # # Store spillover compensation data  # COMMENTED OUT: Using dedicated compensation module
    # spilloverData <- reactiveVal(NULL)
    
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
    
    # Live/Dead stats output - COMMENTED OUT: Using dedicated gating module
    # output$liveDeadStats <- renderText({
    #   req(rawFCS(), input$liveDeadMarkerSelect)
    #   
    #   # Only proceed if a marker is selected
    #   if (input$liveDeadMarkerSelect == "None") {
    #     return("No Live/Dead marker selected")
    #   }
    #   
    #   # Extract values for the selected marker
    #   if (inherits(rawFCS(), "flowFrame")) {
    #     ld_marker <- input$liveDeadMarkerSelect
    #     if (grepl(" - ", ld_marker)) {
    #       ld_marker <- strsplit(ld_marker, " - ")[[1]][1]
    #     }
    #     
    #     if (!(ld_marker %in% colnames(exprs(rawFCS())))) {
    #       return(paste("Marker", ld_marker, "not found in dataset"))
    #     }
    #     
    #     ld_values <- exprs(rawFCS())[, ld_marker]
    #   } else {
    #     # For non-FCS files
    #     if (!(input$liveDeadMarkerSelect %in% colnames(rawFCS()))) {
    #       return(paste("Marker", input$liveDeadMarkerSelect, "not found in dataset"))
    #     }
    #     
    #     ld_values <- rawFCS()[[input$liveDeadMarkerSelect]]
    #   }
    #   
    #   # Calculate percentage of cells classified as live
    #   total_cells <- length(ld_values)
    #   live_cells <- sum(ld_values <= input$liveDeadThresholdSlider, na.rm = TRUE)
    #   live_pct <- live_cells / total_cells * 100
    #   
    #   # Create a summary text
    #   paste0(
    #     "Total Cells: ", format(total_cells, big.mark = ","), "\n",
    #     "Live Cells (below threshold): ", format(live_cells, big.mark = ","), "\n",
    #     "Percentage of Live Cells: ", round(live_pct, 1), "%\n\n",
    #     "Statistics for this marker:\n",
    #     "Min: ", round(min(ld_values, na.rm = TRUE), 1), "\n",
    #     "Max: ", round(max(ld_values, na.rm = TRUE), 1), "\n",
    #     "Mean: ", round(mean(ld_values, na.rm = TRUE), 1), "\n",
    #     "Median: ", round(median(ld_values, na.rm = TRUE), 1)
    #   )
    # })
    
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
        
        # # Add instructional text  # COMMENTED OUT: Using dedicated compensation module
        # instructions <- div(
        #   class = "alert alert-info",
        #   h5("Spillover Compensation Setup Instructions:"),
        #   tags$ol(
        #     tags$li("Upload your ", tags$strong("unstained control"), " file above"),
        #     tags$li("Upload your ", tags$strong("single-stain control"), " files (one per fluorophore)"),
        #     tags$li("Assign each single-stain control to its corresponding marker below"),
        #     tags$li("Click 'Compute Spillover Matrix' when all files are assigned")
        #   ),
        #   p(tags$em("Note: You need one single-stain control for each fluorescent marker you want to compensate."))
        # )
        
        # # Create UI for each uploaded control file  # COMMENTED OUT: Using dedicated compensation module
        # control_file_names <- input$controlFiles$name
        # 
        # ui_elements <- lapply(1:length(control_file_names), function(i) {
        #   file_name <- control_file_names[i]
        #   div(
        #     style = "margin-bottom: 10px; padding: 10px; border: 1px solid #ccc; border-radius: 5px;",
        #     h5(paste("Single-Stain Control:", file_name)),
        #     p(style = "font-size: 0.9em; color: #666;", 
        #       "This file should contain cells stained with ONLY ONE fluorophore. Assign it to the corresponding marker:"),
        #     selectInput(
        #       inputId = session$ns(paste0("marker_", i)),
        #       label = "Assign to Marker:",
        #       choices = c("Select marker..." = "", fluorescence_channels),
        #       selected = "",
        #       width = "100%"
        #     )
        #   )
        # })
        # 
        # do.call(tagList, c(list(instructions), ui_elements))
        
        # Placeholder for compensation functionality
        div(class = "alert alert-info",
            icon("info-circle"), 
            " Spillover compensation has been moved to the dedicated Compensation module.")
        
      } else {
        div(class = "alert alert-warning",
            "Please upload main FCS file first to see available markers")
      }
    })
    
    # # Compute spillover matrix  # COMMENTED OUT: Using dedicated compensation module
    # observeEvent(input$computeSpillover, {
    #   req(input$controlFiles, input$unstainedControlFile)
    #   
    #   # Check if unstained control is uploaded
    #   if (is.null(input$unstainedControlFile)) {
    #     showNotification("Please upload an unstained control file", type = "error")
    #     return()
    #   }
    #   
    #   # Get marker assignments
    #   marker_assignments <- list()
    #   control_file_paths <- list()
    #   
    #   for (i in 1:nrow(input$controlFiles)) {
    #     marker_input_id <- paste0("marker_", i)
    #     if (!is.null(input[[marker_input_id]]) && input[[marker_input_id]] != "") {
    #       file_name <- input$controlFiles$name[i]
    #       file_path <- input$controlFiles$datapath[i]
    #       
    #       marker_assignments[[file_name]] <- input[[marker_input_id]]
    #       control_file_paths[[file_name]] <- file_path
    #     }
    #   }
    #   
    #   if (length(marker_assignments) < 2) {
    #     showNotification("Please assign at least 2 control files to markers", type = "error")
    #     return()
    #   }
    #   
    #   # Add unstained control
    #   unstained_file_path <- input$unstainedControlFile$datapath
    #   unstained_file_name <- input$unstainedControlFile$name
    #   
    #   # Compute spillover matrix
    #   withProgress(message = 'Computing spillover matrix...', value = 0, {
    #     incProgress(0.5, detail = "Processing control files...")
    #     
    #     spillover_result <- computeSpilloverMatrix(control_file_paths, marker_assignments, 
    #                                              unstained_file_path, unstained_file_name)
    #     
    #     incProgress(0.5, detail = "Finalizing matrix...")
    #     
    #     if (spillover_result$success) {
    #       # Store the spillover data
    #       spilloverData(spillover_result)
    #       
    #       showNotification(spillover_result$message, type = "message", duration = 5)
    #     } else {
    #       showNotification(spillover_result$message, type = "error", duration = 10)
    #     }
    #   })
    # })
    
    # # Handle pre-computed spillover matrix upload  # COMMENTED OUT: Using dedicated compensation module
    # observeEvent(input$spilloverMatrixFile, {
    #   req(input$spilloverMatrixFile)
    #   
    #   tryCatch({
    #     # Read CSV file
    #     spillover_matrix <- read.csv(input$spilloverMatrixFile$datapath, row.names = 1)
    #     spillover_matrix <- as.matrix(spillover_matrix)
    #     
    #     # Validate the matrix
    #     if (!is.null(rawFCS()) && inherits(rawFCS(), "flowFrame")) {
    #       validation_result <- validateSpilloverMatrix(spillover_matrix, colnames(exprs(rawFCS())))
    #       
    #       if (validation_result$valid) {
    #         # Store the spillover data
    #         spilloverData(list(
    #           spillover_matrix = spillover_matrix,
    #           success = TRUE,
    #           message = paste("Pre-computed spillover matrix loaded:", validation_result$message)
    #         ))
    #         
    #         showNotification("Spillover matrix loaded successfully", type = "message")
    #       } else {
    #         showNotification(validation_result$message, type = "error")
    #       }
    #     } else {
    #       # Store without validation if no main data loaded yet
    #       spilloverData(list(
    #         spillover_matrix = spillover_matrix,
    #         success = TRUE,
    #         message = "Pre-computed spillover matrix loaded (not yet validated)"
    #       ))
    #       
    #       showNotification("Spillover matrix loaded (will be validated when main data is loaded)", type = "message")
    #     }
    #     
    #   }, error = function(e) {
    #     showNotification(paste("Error loading spillover matrix:", e$message), type = "error")
    #   })
    # })
    
    # # Display spillover status  # COMMENTED OUT: Using dedicated compensation module
    # output$spilloverStatus <- renderText({
    #   if (!is.null(spilloverData())) {
    #     spilloverData()$message
    #   } else {
    #     "No spillover matrix computed or loaded yet."
    #   }
    # })
    # 
    # # Display spillover matrix in sidebar
    # output$spilloverMatrixDisplay <- DT::renderDataTable({
    #   req(spilloverData(), spilloverData()$success)
    #   
    #   DT::datatable(
    #     round(spilloverData()$spillover_matrix, 4),
    #     options = list(scrollX = TRUE, pageLength = 10)
    #   )
    # })
    # 
    # # Display spillover matrix in main panel
    # output$spilloverMatrixTable <- DT::renderDataTable({
    #   req(spilloverData(), spilloverData()$success)
    #   
    #   DT::datatable(
    #     round(spilloverData()$spillover_matrix, 4),
    #     options = list(scrollX = TRUE, pageLength = 15),
    #     caption = "Spillover Matrix (values represent spillover from column to row)"
    #   ) %>% formatRound(columns = 1:ncol(spilloverData()$spillover_matrix), digits = 4)
    # })
    
    # # Plot compensation effects
    # output$compensationEffectsPlot <- renderPlot({
    #   req(spilloverData(), spilloverData()$success)
    #   
    #   # Get font size from app settings
    #   font_size <- app_state$plot_settings$font_size
    #   
    #   spillover_matrix <- spilloverData()$spillover_matrix
    #   
    #   # Use the utility function to create the heatmap
    #   createSpilloverHeatmap(spillover_matrix, 
    #                         title = "Spillover Matrix Heatmap", 
    #                         font_size = font_size)
    # }, width = function() app_state$plot_settings$width,
    # height = function() app_state$plot_settings$height)
    # 
    # # Plot before vs after compensation comparison
    # output$beforeAfterCompensationPlot <- renderPlot({
    #   req(rawFCS(), spilloverData(), spilloverData()$success)
    #   
    #   # Get font size from app settings
    #   font_size <- app_state$plot_settings$font_size
    #   
    #   tryCatch({
    #     # Get a sample of data for visualization
    #     fcs_data <- rawFCS()
    #     if (!inherits(fcs_data, "flowFrame")) {
    #       return(ggplot() + annotate("text", x = 0.5, y = 0.5, 
    #                                label = "Compensation comparison only available for FCS files") +
    #              theme_void())
    #     }
    #     
    #     # Sample data for faster plotting
    #     n_sample <- min(5000, nrow(fcs_data))
    #     sample_indices <- sample(nrow(fcs_data), n_sample)
    #     
    #     # Get original data
    #     original_data <- exprs(fcs_data)[sample_indices, ]
    #     
    #     # Apply compensation
    #     comp_result <- applyCompensation(fcs_data, spilloverData()$spillover_matrix)
    #     compensated_data <- exprs(comp_result$data)[sample_indices, ]
    #     
    #     # Get channels that are in the spillover matrix
    #     common_channels <- intersect(colnames(original_data), colnames(spilloverData()$spillover_matrix))
    #     
    #     if (length(common_channels) >= 2) {
    #       # Create comparison plot for first two channels
    #       ch1 <- common_channels[1]
    #       ch2 <- common_channels[2]
    #       
    #       # Use the utility function to create the comparison plot
    #       createCompensationComparisonPlot(original_data, compensated_data, 
    #                                      ch1, ch2, 
    #                                      n_sample = n_sample, 
    #                                      font_size = font_size)
    #     } else {
    #       ggplot() + annotate("text", x = 0.5, y = 0.5, 
    #                          label = "Not enough matching channels for comparison") +
    #         theme_void()
    #     }
    #     
    #   }, error = function(e) {
    #     ggplot() + annotate("text", x = 0.5, y = 0.5, 
    #                        label = paste("Error creating comparison plot:", e$message)) +
    #       theme_void()
    #   })
    # }, width = function() app_state$plot_settings$width,
    # height = function() app_state$plot_settings$height)
    
    # Apply live/dead gating threshold to the main analysis - COMMENTED OUT: Using dedicated gating module
    # observeEvent(input$applyLiveDeadGating, {
    #   req(input$liveDeadMarkerSelect, input$liveDeadMarkerSelect != "None")
    #   
    #   # Extract correct marker name (remove the description part)
    #   ld_marker <- input$liveDeadMarkerSelect
    #   if (grepl(" - ", ld_marker)) {
    #     ld_marker <- strsplit(ld_marker, " - ")[[1]][1]
    #   }
    #   
    #   # Update liveDeadGate selection
    #   updateSelectInput(session, "liveDeadGate", selected = ld_marker)
    #   
    #   # Update liveDeadThreshold value
    #   updateNumericInput(session, "liveDeadThreshold", value = input$liveDeadThresholdSlider)
    #   
    #   # Make sure gating is enabled
    #   updateCheckboxInput(session, "performGating", value = TRUE)
    #   
    #   # Show notification
    #   showNotification(
    #     paste("Live/Dead threshold updated to", input$liveDeadThresholdSlider),
    #     type = "message", 
    #     duration = 3
    #   )
    # })
    
    # ============================================================================
    # SERVER-SIDE CONDITIONAL UI RENDERING
    # ============================================================================
    
    # # Compensation settings UI
    # output$compensationSettingsUI <- renderUI({
    #   if (isTRUE(input$enableCompensation)) {
    #     tagList(
    #       div(class = "compensation-panel",
    #         h5(icon("upload"), "Control Files"),
    #         
    #         # Upload multiple control files
    #         fileInput(session$ns("controlFiles"), "Upload Control Files (FCS only)", 
    #                   accept = c(".fcs"), multiple = TRUE),
    #         
    #         # Upload unstained control file
    #         fileInput(session$ns("unstainedControlFile"), "Upload Unstained Control File (FCS only)", 
    #                   accept = c(".fcs"), multiple = FALSE),
    #         
    #         # UI for assigning markers to control files
    #         uiOutput(session$ns("markerAssignmentUI")),
    #         
    #         # Button to compute spillover matrix
    #         actionButton(session$ns("computeSpillover"), "Compute Spillover Matrix", 
    #                      class = "btn-info btn-block", 
    #                      icon = icon("calculator"),
    #                      style = "margin-bottom: 10px;"),
    #         
    #         # Display spillover computation status
    #         verbatimTextOutput(session$ns("spilloverStatus"))
    #       ),
    #       
    #       hr(),
    #       
    #       # Option to upload pre-computed spillover matrix
    #       h5(icon("file-import"), "Import Existing Matrix"),
    #       fileInput(session$ns("spilloverMatrixFile"), "Upload Pre-computed Spillover Matrix (CSV)", 
    #                 accept = c(".csv")),
    #       
    #       # Display current spillover matrix conditionally
    #       if (!is.null(spilloverData()) && spilloverData()$success) {
    #         tagList(
    #           h5("Current Spillover Matrix:"),
    #           DT::dataTableOutput(session$ns("spilloverMatrixDisplay"))
    #         )
    #       }
    #     )
    #   }
    # })
    
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
    
    # Gating settings UI - COMMENTED OUT: Using dedicated gating module now
    # output$gatingSettingsUI <- renderUI({
    #   if (isTRUE(input$performGating)) {
    #     tagList(
    #       textInput(session$ns("debrisGate"), "FSC/SSC Parameters (comma-separated)", 
    #                 value = "FSC-A,SSC-A"),
    #       selectInput(session$ns("liveDeadGate"), "Live/Dead Parameter", 
    #                   choices = c("None", "Live Dead BV570 Violet-610-A"),
    #                   selected = "None"),
    #       
    #       # Live/dead threshold parameter
    #       if (!is.null(input$liveDeadGate) && input$liveDeadGate != "None") {
    #         tagList(
    #           numericInput(session$ns("liveDeadThreshold"), "Live/Dead Threshold", 
    #                        value = 1000, min = 0, max = 10000, step = 100),
    #           helpText("Cells with values below this threshold will be considered live")
    #         )
    #       }
    #     )
    #   }
    # })
    
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
      
      # Create input fields for each marker
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
          "Examples: FL1-A → CD3, FL2-A → CD4, FJComp-FL1-A → CD8, etc."
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
      
      # Collect all the new names
      new_mappings <- list()
      for (i in seq_along(markers)) {
        marker <- markers[i]
        input_id <- paste0("marker_display_", i)
        new_name <- input[[input_id]]
        
        if (!is.null(new_name) && trimws(new_name) != "") {
          new_mappings[[marker]] <- trimws(new_name)
        } else {
          new_mappings[[marker]] <- marker  # Use technical name if no display name
        }
      }
      
      # Update the mappings
      markerNameMappings(new_mappings)
      
      removeModal()
      showNotification("Marker names updated successfully!", type = "message", duration = 3)
    })
    
    # Reset marker names
    observeEvent(input$resetMarkerNames, {
      markerNameMappings(list())
      removeModal()
      showNotification("All marker names reset to technical names", type = "message", duration = 3)
    })
    
    # Download marker mappings
    output$downloadMarkerMappings <- downloadHandler(
      filename = function() {
        paste0("marker_mappings_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
      },
      content = function(file) {
        mappings <- markerNameMappings()
        if (length(mappings) == 0) {
          # Create empty file with headers
          mapping_df <- data.frame(
            TechnicalName = character(0),
            DisplayName = character(0),
            stringsAsFactors = FALSE
          )
        } else {
          # Create data frame from mappings
          mapping_df <- data.frame(
            TechnicalName = names(mappings),
            DisplayName = unlist(mappings),
            stringsAsFactors = FALSE
          )
        }
        write.csv(mapping_df, file, row.names = FALSE)
      }
    )
    
    # Upload marker mappings
    observeEvent(input$uploadMarkerMappings, {
      req(input$uploadMarkerMappings)
      
      tryCatch({
        # Read the CSV file
        mapping_df <- read.csv(input$uploadMarkerMappings$datapath, stringsAsFactors = FALSE)
        
        # Validate the CSV structure
        if (!all(c("TechnicalName", "DisplayName") %in% colnames(mapping_df))) {
          showNotification("CSV file must have 'TechnicalName' and 'DisplayName' columns", 
                          type = "error", duration = 5)
          return()
        }
        
        # Convert to list format
        new_mappings <- setNames(
          as.list(mapping_df$DisplayName),
          mapping_df$TechnicalName
        )
        
        # Update the mappings
        markerNameMappings(new_mappings)
        
      showNotification(
          paste("Successfully imported", nrow(mapping_df), "marker mappings"), 
          type = "message", duration = 3
        )
        
      }, error = function(e) {
        showNotification(
          paste("Error reading mappings file:", e$message), 
          type = "error", duration = 5
        )
      })
    })
    
    # # Compensation analysis UI
    # output$compensationAnalysisUI <- renderUI({
    #   if (isTRUE(input$enableCompensation)) {
    #     tagList(
    #       fluidRow(
    #         column(6,
    #                shinydashboard::box(
    #                  title = "Spillover Matrix", status = "info", solidHeader = TRUE,
    #                  width = 12,
    #                  DT::dataTableOutput(session$ns("spilloverMatrixTable"))
    #                )
    #         ),
    #         column(6,
    #                shinydashboard::box(
    #                  title = "Compensation Effects", status = "success", solidHeader = TRUE,
    #                  width = 12,
    #                  plotOutput(session$ns("compensationEffectsPlot"), height = "400px")
    #                )
    #         )
    #       ),
    #       shinydashboard::box(
    #         title = "Before vs After Compensation", status = "primary", solidHeader = TRUE,
    #         width = 12,
    #         plotOutput(session$ns("beforeAfterCompensationPlot"), height = "500px")
    #       )
    #     )
    #   } else {
    #     div(class = "alert alert-info", style = "margin: 15px;",
    #         icon("info-circle"), 
    #         h5("Spillover Compensation Disabled"),
    #         p("Enable spillover compensation in the sidebar to view compensation analysis.")
    #     )
    #   }
    # })
    
    # Live/Dead controls UI
    output$liveDeadControlsUI <- renderUI({
      if (!is.null(input$liveDeadMarkerSelect) && input$liveDeadMarkerSelect != "None") {
        tagList(
          sliderInput(session$ns("liveDeadThresholdSlider"), "Threshold", 
                      min = 0, max = 5000, value = 1000, step = 100),
          checkboxInput(session$ns("useLogScale"), "Use Log Scale for Visualization", value = TRUE)
          # hr(),
          # h5("Gating Results"),  # COMMENTED OUT: Using dedicated gating module
          # verbatimTextOutput(session$ns("liveDeadStats")),
          # actionButton(session$ns("applyLiveDeadGating"), "Apply to Preprocessing", 
          #              class = "btn-primary", style = "width: 100%;")
        )
      }
    })
    
    # Live/Dead scatter UI
    output$liveDeadScatterUI <- renderUI({
      if (!is.null(input$liveDeadMarkerSelect) && input$liveDeadMarkerSelect != "None") {
        shinydashboard::box(
          title = "2D Live/Dead Visualization", status = "success", solidHeader = TRUE,
          width = 12,
          plotOutput(session$ns("liveDeadScatter"), height = "400px")
        )
      }
    })
    
    # Cluster analysis UI
    output$clusterAnalysisUI <- renderUI({
      if (!is.null(input[["clustering-showClusteringOptions"]]) && input[["clustering-showClusteringOptions"]]) {
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
          # perform_gating = isTRUE(input$performGating),  # COMMENTED OUT: Using dedicated gating module
          perform_gating = FALSE,  # Disabled - using dedicated gating module
          # perform_compensation = isTRUE(input$enableCompensation),
          perform_compensation = FALSE,  # COMMENTED OUT: Using dedicated compensation module
          # spillover_matrix = if (isTRUE(input$enableCompensation) && !is.null(spilloverData()) && spilloverData()$success) {
          #   spilloverData()$spillover_matrix
          # } else {
          #   NULL
          # },
          spillover_matrix = NULL,  # COMMENTED OUT: Using dedicated compensation module
          scale_data = TRUE,
          seed = 123
        )
        
        # Add QC settings if QC is enabled
        if (isTRUE(input$performQC)) {
          preprocessing_params$qc_settings <- list(
            max_anomalies = input$maxAnomalies / 100  # Convert from percentage to proportion
          )
        }
        
        # Add gating parameters if gating is enabled - COMMENTED OUT: Using dedicated gating module
        # if (isTRUE(input$performGating)) {
        #   # Parse debris gate parameters from comma-separated string
        #   debris_gate_params <- unlist(strsplit(input$debrisGate, ",\\s*"))
        #   
        #   preprocessing_params$gates <- list(
        #     debris_gate = if (length(debris_gate_params) >= 2) debris_gate_params[1:2] else NULL,
        #     live_dead_gate = if (!is.null(input$liveDeadGate) && input$liveDeadGate != "None") input$liveDeadGate else NULL
        #   )
        #   
        #   # Add threshold for live/dead gating if available
        #   if (!is.null(input$liveDeadThreshold) && input$liveDeadGate != "None") {
        #     # We need to modify the gating function to use this threshold
        #     # For now, we'll display a notification about it
        #     showNotification(
        #       paste("Using live/dead threshold of", input$liveDeadThreshold),
        #       type = "message", 
        #       duration = 3
        #     )
        #     
        #     # In a full implementation, we'd need to update the performGating function
        #     # to use this custom threshold instead of the hardcoded 1000 value
        #   }
        # }
        
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
          
          # MEMORY OPTIMIZATION: Clear t-SNE intermediate objects immediately
          tsne_result <- NULL
          data_matrix <- NULL
          tsne_params <- NULL
          gc(verbose = FALSE)
        }
        
        if ("UMAP" %in% input$methods) {
          incProgress(0.7, detail = "Running UMAP...")
          umap_result <- umap(preprocessing_results$scaled_data, n_neighbors = input$n_neighbors)
          results$umap <- data.frame(umap1 = umap_result[,1], umap2 = umap_result[,2])
          
          # MEMORY OPTIMIZATION: Clear UMAP intermediate objects immediately
          umap_result <- NULL
          gc(verbose = FALSE)
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
          
          # MEMORY OPTIMIZATION: Clear PCA intermediate objects immediately
          pca_result <- NULL
          gc(verbose = FALSE)
        }
        
        if ("MDS" %in% input$methods) {
          incProgress(0.9, detail = "Running MDS...")
          data_matrix <- as.matrix(preprocessing_results$scaled_data)
          dist_matrix <- dist(data_matrix)
          mds_result <- cmdscale(dist_matrix, k = 2)
          results$mds <- data.frame(mds1 = mds_result[,1], mds2 = mds_result[,2])
          
          # MEMORY OPTIMIZATION: Clear MDS intermediate objects immediately (these can be very large)
          data_matrix <- NULL
          dist_matrix <- NULL
          mds_result <- NULL
          gc(verbose = FALSE)
        }
        
        # Create plot data
        plot_data <- as.data.frame(preprocessing_results$sampled_data)
        colnames(plot_data) <- input$selectedMarkers
        
        # Add dimensionality reduction coordinates and clear each result immediately after use
        if (!is.null(results$tsne)) {
          plot_data$tsne1 <- results$tsne$tsne1
          plot_data$tsne2 <- results$tsne$tsne2
          # MEMORY OPTIMIZATION: Clear t-SNE results after adding to plot_data
          results$tsne <- NULL
          gc(verbose = FALSE)
        }
        
        if (!is.null(results$umap)) {
          plot_data$umap1 <- results$umap$umap1
          plot_data$umap2 <- results$umap$umap2
          # MEMORY OPTIMIZATION: Clear UMAP results after adding to plot_data
          results$umap <- NULL
          gc(verbose = FALSE)
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
          # MEMORY OPTIMIZATION: Clear PCA results after adding to plot_data
          results$pca <- NULL
          gc(verbose = FALSE)
        }
        
        if (!is.null(results$mds)) {
          plot_data$mds1 <- results$mds$mds1
          plot_data$mds2 <- results$mds$mds2
          # MEMORY OPTIMIZATION: Clear MDS results after adding to plot_data
          results$mds <- NULL
          gc(verbose = FALSE)
        }
        
        results$plot_data <- plot_data
        results$markers <- input$selectedMarkers
        
        # MEMORY OPTIMIZATION: Clear preprocessing intermediate data that's no longer needed
        # Keep only essential results for downstream analysis
        preprocessing_results$qc_data <- NULL
        preprocessing_results$gated_data <- NULL
        preprocessing_results$transformed_data <- NULL
        # Keep sampled_data and scaled_data as they're needed for clustering
        gc(verbose = FALSE)
        
        # CRITICAL: Preserve original flowSet for gating module compatibility
        if (inherits(raw_data, "flowFrame")) {
          # Convert single flowFrame to flowSet
          results$raw_data <- flowSet(raw_data)
        } else if (inherits(raw_data, "flowSet")) {
          # Store original flowSet
          results$raw_data <- raw_data  
        }
        
        processedData(results)
        
        # MEMORY OPTIMIZATION: Final cleanup after storing results
        plot_data <- NULL
        preprocessing_results <- NULL
        gc(verbose = FALSE)
        
        # Display notification about preprocessing results
        if (!is.null(results$metrics)) {
          qc_removed <- round(results$metrics$qc$removed_pct * 100, 1)
          # gating_removed <- round(results$metrics$gating$removed_pct * 100, 1)  # COMMENTED OUT: Using dedicated gating module
          # compensation_applied <- results$metrics$compensation$applied  # COMMENTED OUT: Using dedicated compensation module
          
          msg <- paste0("Preprocessing complete: ", 
                        # if (compensation_applied) "Compensation applied, " else "",  # COMMENTED OUT: Using dedicated compensation module
                        qc_removed, "% removed in QC. ",
                        # gating_removed, "% removed in gating. ",  # COMMENTED OUT: Using dedicated gating module
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
      
      if (!is.null(results$mds)) {
        # Calculate MDS metrics if it was run
        mds_data <- results$mds
        
        # Calculate a simple MDS stress or distance preservation metric
        mds_stress <- tryCatch({
          original_dist <- dist(scale(results$scaled_data))
          embedding_dist <- dist(as.matrix(mds_data))
          
          # Normalize distances
          original_dist <- original_dist / max(original_dist)
          embedding_dist <- embedding_dist / max(embedding_dist)
          
          # Calculate mean squared error between distance matrices
          mean((as.matrix(original_dist) - as.matrix(embedding_dist))^2)
        }, error = function(e) NA)
        
        # Store MDS metrics
        metrics$mds <- list(
          stress = round(mds_stress, 4),
          method = "Classical MDS"
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
      
      # Add MDS metrics if available
      if (!is.null(metrics$mds)) {
        metrics_ui <- tagAppendChildren(
          metrics_ui,
          div(
            h5("MDS Metrics:"),
            p(paste("Stress (distance error):", metrics$mds$stress, 
                    "(lower is better)")),
            p(paste("Method:", metrics$mds$method))
          )
        )
      }
      
      # Add recommendation if multiple methods are available
      available_methods <- c()
      if (!is.null(metrics$tsne)) available_methods <- c(available_methods, "t-SNE")
      if (!is.null(metrics$umap)) available_methods <- c(available_methods, "UMAP")
      if (!is.null(metrics$pca)) available_methods <- c(available_methods, "PCA")
      if (!is.null(metrics$mds)) available_methods <- c(available_methods, "MDS")
      
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
        if (!is.null(metrics$mds)) {
          stress_scaled <- min(metrics$mds$stress / 0.2, 1)
          scores$mds <- 1 - stress_scaled  # Higher score means better
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
                            "• PCA: Linear method, preserves global distances, interpretable components\n",
                            "• MDS: Preserves pairwise distances, good for visualizing overall geometry")
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
          # div(
          #   h5("Compensation:"),
          #   p(paste("Applied:", results$metrics$compensation$applied)),
          #   p(paste("Status:", results$metrics$compensation$message))
          # ),
          div(
            h5("Quality Control:"),
            p(paste("Initial cells:", results$metrics$qc$initial_count)),
            p(paste("After QC:", results$metrics$qc$final_count)),
            p(paste("Removed:", round(results$metrics$qc$removed_pct * 100, 1), "%"))
          ),
          # div(  # COMMENTED OUT: Using dedicated gating module
          #   h5("Gating:"),
          #   p(paste("After gating:", results$metrics$gating$final_count)),
          #   p(paste("Removed in gating:", round(results$metrics$gating$removed_pct * 100, 1), "%"))
          # ),
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
    
    # Make t-SNE and UMAP plots reactive to plot settings changes
    observe({
      # This observer will re-run whenever plot settings change
      app_state$plot_settings
      
      # Force all plotly outputs to redraw with new settings
      session$sendCustomMessage(type = "plotly-replot", message = list(id = session$ns("tsnePlot")))
      session$sendCustomMessage(type = "plotly-replot", message = list(id = session$ns("umapPlot")))
      session$sendCustomMessage(type = "plotly-replot", message = list(id = session$ns("pcaPlot")))
      session$sendCustomMessage(type = "plotly-replot", message = list(id = session$ns("mdsPlot")))
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
    
    output$mdsPlot <- renderPlotly({
      font_size <- app_state$plot_settings$font_size
      point_size <- app_state$plot_settings$point_size
      color_palette <- app_state$plot_settings$color_palette
      width <- app_state$plot_settings$width
      height <- app_state$plot_settings$height
      
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
        plot_data = plot_data_copy,
        dim1 = "mds1",
        dim2 = "mds2",
        colorBy = color_by,
        color_palette = color_palette,
        point_size = point_size,
        font_size = font_size,
        title = "MDS Projection",
        xlab = "MDS 1",
        ylab = "MDS 2"
      )
      
      p_plotly <- ggplotly(p, width = width, height = height)
      
      p_plotly <- p_plotly %>% layout(
        font = list(
          family = "Arial",
          size = font_size,
          color = "black"
        ),
        title = list(
          text = "MDS Projection",
          font = list(
            family = "Arial",
            size = font_size * 1.2,
            color = "black"
          )
        ),
        xaxis = list(
          title = list(
            text = "MDS 1",
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
            text = "MDS 2",
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
    
    # ============================================================================
    # MARKER EXPRESSION HEATMAP FUNCTIONALITY
    # ============================================================================
    
    # Update marker choices when data changes
    observe({
      req(processedData())
      markers <- processedData()$markers
      
      # Get marker choices with display names
      marker_choices <- getMarkerChoicesWithDisplayNames(markers, markerNameMappings())
      
      updateSelectInput(session, "heatmapMarker", 
                       choices = marker_choices, selected = if(length(markers) > 0) markers[1] else NULL)
    })
    
    # Update marker choices when mappings change
    observe({
      req(processedData())
      markerNameMappings()  # This dependency ensures the observer runs when mappings change
      
      markers <- processedData()$markers
      current_selection <- input$heatmapMarker
      
      # Get marker choices with updated display names
      marker_choices <- getMarkerChoicesWithDisplayNames(markers, markerNameMappings())
      
      updateSelectInput(session, "heatmapMarker", 
                       choices = marker_choices, selected = current_selection)
    })
    
    # Update dimensionality method choices
    observe({
      req(processedData())
      plot_data <- processedData()$plot_data
      available_methods <- getAvailableDimMethods(plot_data)
      
      updateSelectInput(session, "heatmapDimMethod",
                       choices = names(available_methods),
                       selected = if(length(available_methods) > 0) names(available_methods)[1] else NULL)
    })
    
    # Render individual marker heatmap
    output$markerHeatmapPlot <- renderPlotly({
      req(processedData(), input$heatmapMarker, input$heatmapDimMethod)
      
      plot_data <- processedData()$plot_data
      available_methods <- getAvailableDimMethods(plot_data)
      
      if (!input$heatmapDimMethod %in% names(available_methods)) return(NULL)
      
      dim_coords <- available_methods[[input$heatmapDimMethod]]
      dim_labels <- getDimAxisLabels(input$heatmapDimMethod)
      
      # Optimize data for rendering
      opt_data <- optimizeHeatmapRendering(plot_data)
      
      bins_value <- if (!is.null(input$heatmapBins)) input$heatmapBins else 50
      
      p <- createMarkerExpressionHeatmap(
        plot_data = opt_data,
        marker = input$heatmapMarker,
        dim1 = dim_coords[1],
        dim2 = dim_coords[2],
        method = input$heatmapMethod,
        bins = bins_value,
        title = paste(input$heatmapMarker, "Expression on", input$heatmapDimMethod),
        font_size = app_state$plot_settings$font_size,
        color_palette = input$heatmapColorPalette,
        use_qualitative_labels = (input$legendStyle == "qualitative"),
        label_style = if(input$legendStyle == "qualitative") input$labelStyle else "standard"
      )
      
      # Customize axis labels
      p <- p + labs(x = dim_labels[1], y = dim_labels[2])
      
      p_plotly <- ggplotly(p, width = app_state$plot_settings$width, 
                          height = app_state$plot_settings$height)
      
      # Apply font settings
      p_plotly <- p_plotly %>% layout(
        title = list(
          text = paste(input$heatmapMarker, "Expression on", input$heatmapDimMethod),
          font = list(
            family = "Arial",
            size = app_state$plot_settings$font_size * 1.2,
            color = "black"
          )
        ),
        xaxis = list(
          title = list(
            text = dim_labels[1],
            font = list(
              family = "Arial",
              size = app_state$plot_settings$font_size * 1.1,
              color = "black"
            )
          ),
          tickfont = list(
            family = "Arial",
            size = app_state$plot_settings$font_size
          )
        ),
        yaxis = list(
          title = list(
            text = dim_labels[2],
            font = list(
              family = "Arial",
              size = app_state$plot_settings$font_size * 1.1,
              color = "black"
            )
          ),
          tickfont = list(
            family = "Arial",
            size = app_state$plot_settings$font_size
          )
        ),
        hoverlabel = list(
          bgcolor = "white",
          font = list(
            family = "Arial",
            size = app_state$plot_settings$font_size * 0.9
          )
        )
      )
      
      return(p_plotly)
    })
    
    # Render static marker heatmap with scaling support
    output$markerHeatmapPlotStatic <- renderPlot({
      req(processedData(), input$heatmapMarker, input$heatmapDimMethod)
      
      plot_data <- processedData()$plot_data
      available_methods <- getAvailableDimMethods(plot_data)
      
      if (!input$heatmapDimMethod %in% names(available_methods)) return(NULL)
      
      dim_coords <- available_methods[[input$heatmapDimMethod]]
      dim_labels <- getDimAxisLabels(input$heatmapDimMethod)
      
      # Optimize data for rendering
      opt_data <- optimizeHeatmapRendering(plot_data)
      
      bins_value <- if (!is.null(input$heatmapBins)) input$heatmapBins else 50
      
      # Get display name for the marker
      marker_display_name <- getMarkerDisplayName(input$heatmapMarker, markerNameMappings())
      
      p <- createMarkerExpressionHeatmap(
        plot_data = opt_data,
        marker = input$heatmapMarker,
        dim1 = dim_coords[1],
        dim2 = dim_coords[2],
        method = input$heatmapMethod,
        bins = bins_value,
        title = paste(marker_display_name, "Expression on", input$heatmapDimMethod),
        font_size = app_state$plot_settings$font_size,
        color_palette = input$heatmapColorPalette,
        use_qualitative_labels = (input$legendStyle == "qualitative"),
        label_style = if(input$legendStyle == "qualitative") input$labelStyle else "standard"
      )
      
      # Customize axis labels
      p + labs(x = dim_labels[1], y = dim_labels[2])
      
    }, width = function() app_state$plot_settings$width,
       height = function() app_state$plot_settings$height)
    
    # Store analysis results for smart features
    smart_analysis_results <- reactiveVal(NULL)
    
    # Track heatmap display mode
    heatmap_display_mode <- reactiveVal("individual")  # "individual" or "grid"
    
    # Run smart analysis when button is clicked
    observeEvent(input$runSmartAnalysis, {
      req(processedData(), input$heatmapDimMethod)
      
      withProgress(message = 'Running smart analysis...', value = 0, {
        
        plot_data <- processedData()$plot_data
        markers <- processedData()$markers
        available_methods <- getAvailableDimMethods(plot_data)
        
        if (!input$heatmapDimMethod %in% names(available_methods)) {
          showNotification("Selected dimensionality method not available", type = "error")
          return()
        }
        
        # Run comprehensive analysis
        incProgress(0.3, detail = "Analyzing expression hotspots...")
        
        analysis_results <- runComprehensiveAnalysis(
          plot_data = plot_data,
          markers = markers,
          dim_methods = list(selected = available_methods[[input$heatmapDimMethod]]),
          hotspot_threshold = input$hotspotThreshold / 100,
          population_threshold_high = 0.75,
          population_threshold_low = 0.25
        )
        
        incProgress(0.7, detail = "Generating population suggestions...")
        
        # Store results
        smart_analysis_results(analysis_results)
        
        # Update global app state with suggestions if enabled
        if (input$enablePopulationSuggestions && !is.null(analysis_results$selected$populations)) {
          suggested_populations <- convertSuggestionsToPopulations(analysis_results$selected$populations)
          app_state$cell_identification$suggested_populations <- suggested_populations
          
          incProgress(1.0, detail = "Analysis complete!")
          
          showNotification(
            paste("Found", length(analysis_results$selected$populations), "population suggestions and",
                  analysis_results$selected$summary$markers_with_hotspots, "markers with hotspots!"),
            type = "message", duration = 5
          )
        } else {
          incProgress(1.0, detail = "Analysis complete!")
          
          showNotification(
            paste("Analysis complete:", analysis_results$selected$summary$markers_with_hotspots, 
                  "markers with expression hotspots detected"),
            type = "message", duration = 5
          )
        }
      })
    })
    
    # Render smart analysis results
    output$smartAnalysisResults <- renderUI({
      req(smart_analysis_results())
      
      results <- smart_analysis_results()$selected
      
      if (is.null(results)) {
        return(div(class = "alert alert-info", "Run smart analysis to see results here."))
      }
      
      # Create summary cards
      summary_cards <- fluidRow(
        column(3,
          div(class = "info-box",
              style = "background-color: #e3f2fd; padding: 15px; border-radius: 8px; text-align: center;",
              h4(results$summary$total_markers, style = "margin: 0; color: #1976d2;"),
              p("Total Markers", style = "margin: 0; font-weight: bold;")
          )
        ),
        column(3,
          div(class = "info-box",
              style = "background-color: #f3e5f5; padding: 15px; border-radius: 8px; text-align: center;",
              h4(results$summary$markers_with_hotspots, style = "margin: 0; color: #7b1fa2;"),
              p("Markers with Hotspots", style = "margin: 0; font-weight: bold;")
          )
        ),
        column(3,
          div(class = "info-box",
              style = "background-color: #e8f5e8; padding: 15px; border-radius: 8px; text-align: center;",
              h4(results$summary$total_populations, style = "margin: 0; color: #388e3c;"),
              p("Population Suggestions", style = "margin: 0; font-weight: bold;")
          )
        ),
        column(3,
          div(class = "info-box",
              style = "background-color: #fff3e0; padding: 15px; border-radius: 8px; text-align: center;",
              h4(input$heatmapDimMethod, style = "margin: 0; color: #f57c00;"),
              p("Analysis Method", style = "margin: 0; font-weight: bold;")
          )
        )
      )
      
      # Create detailed results
      detailed_results <- tagList()
      
      # Hotspot detection results
      if (input$enableHotspotDetection) {
        hotspot_summary <- div(
          h6(icon("fire"), "Expression Hotspots by Marker"),
          DT::dataTableOutput(session$ns("hotspotSummaryTable"))
        )
        detailed_results <- tagAppendChild(detailed_results, hotspot_summary)
      }
      
      # Population suggestions
      if (input$enablePopulationSuggestions) {
        population_summary <- div(
          h6(icon("users"), "Population Suggestions"),
          DT::dataTableOutput(session$ns("populationSuggestionsTable"))
        )
        detailed_results <- tagAppendChild(detailed_results, population_summary)
      }
      
      return(tagList(
        summary_cards,
        br(),
        detailed_results
      ))
    })
    
    # Render hotspot summary table
    output$hotspotSummaryTable <- DT::renderDataTable({
      req(smart_analysis_results(), input$enableHotspotDetection)
      
      results <- smart_analysis_results()$selected$markers
      
      # Create summary table
      hotspot_data <- data.frame(
        Marker = names(results),
        NumHotspots = sapply(results, function(x) x$num_hotspots),
        HighExpressionPercent = round(sapply(results, function(x) x$high_expression_percent), 1),
        Threshold = round(sapply(results, function(x) x$threshold), 3),
        Message = sapply(results, function(x) x$message),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(
        hotspot_data,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 't'
        ),
        caption = "Expression hotspot detection results"
      )
    })
    
    # Render population suggestions table
    output$populationSuggestionsTable <- DT::renderDataTable({
      req(smart_analysis_results(), input$enablePopulationSuggestions)
      
      results <- smart_analysis_results()$selected
      population_table <- createPopulationSummaryTable(list(selected = results), "selected")
      
      if (nrow(population_table) == 0 || "Message" %in% colnames(population_table)) {
        return(DT::datatable(
          data.frame(Message = "No population suggestions found"),
          options = list(dom = 't')
        ))
      }
      
      DT::datatable(
        population_table,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 't'
        ),
        caption = "Automatically suggested cell populations"
      ) %>%
        DT::formatRound(columns = c("Percentage", "Confidence"), digits = 1)
    })
    

    
    # Ultra-fast grid view button handler
    observeEvent(input$generateAllHeatmapsFast, {
      req(processedData(), input$heatmapDimMethod)
      heatmap_display_mode("grid")
      
      showNotification("Switched to fast grid view", type = "message", duration = 3)
    })
    
    # Switch back to individual view when marker selection changes
    observeEvent(input$heatmapMarker, {
      if (heatmap_display_mode() == "grid") {
        heatmap_display_mode("individual")
      }
    })
    
    # Dynamic main display area
    output$heatmapMainDisplay <- renderUI({
      if (heatmap_display_mode() == "grid") {
        # Ultra-fast grid view
        tagList(
          div(class = "alert alert-success", style = "margin-bottom: 10px;",
              icon("lightning-bolt"), " Fast Grid Mode: All markers rendered simultaneously"),
          
          # Back to individual button
          actionButton(session$ns("backToIndividual"), "← Back to Individual View", 
                      class = "btn-secondary btn-sm", style = "margin-bottom: 15px;"),
          
          # Ultra-fast grid
          shinycssloaders::withSpinner(
            uiOutput(session$ns("ultraFastGrid"))
          )
        )
      } else {
        # Individual heatmap view - STATIC ONLY
        tagList(
          # Static plot output only
          shinycssloaders::withSpinner(
            plotOutput(session$ns("markerHeatmapPlotStatic"), height = "600px")
          )
        )
      }
    })
    
    # Back to individual view button handler
    observeEvent(input$backToIndividual, {
      heatmap_display_mode("individual")
    })
    
    # Ultra-fast grid rendering
    output$ultraFastGrid <- renderUI({
      req(processedData(), input$heatmapDimMethod, heatmap_display_mode() == "grid")
      
      markers <- processedData()$markers
      plot_data <- processedData()$plot_data
      available_methods <- getAvailableDimMethods(plot_data)
      dim_coords <- available_methods[[input$heatmapDimMethod]]
      dim_labels <- getDimAxisLabels(input$heatmapDimMethod)
      
      # Create grid layout (2 plots per row for larger size)
      cols <- 2
      bins_value <- if (!is.null(input$heatmapBins)) input$heatmapBins else 30
      
      # Larger plots with increased height
      plot_height <- "500px"
      
      plot_list <- lapply(seq_along(markers), function(i) {
        marker <- markers[i]
        plot_id <- paste0("ultraFastHeatmap_", i)
        
        # Get display name for this marker
        marker_display_name <- getMarkerDisplayName(marker, markerNameMappings())
        
        output[[plot_id]] <- renderPlot({
          # No point restriction for static plots
          opt_data <- optimizeHeatmapRendering(plot_data)
          
          p <- createMarkerExpressionHeatmap(
            plot_data = opt_data,
            marker = marker,
            dim1 = dim_coords[1], 
            dim2 = dim_coords[2],
            method = input$heatmapMethod,
            bins = bins_value,
            title = marker_display_name,
            font_size = 18,  # Smaller font for grid
            color_palette = input$heatmapColorPalette,
            use_qualitative_labels = (input$legendStyle == "qualitative"),
            label_style = if(input$legendStyle == "qualitative") input$labelStyle else "standard"
          )
          
          # Minimal styling for speed
          p + labs(x = dim_labels[1], y = dim_labels[2]) +
            theme(
              plot.title = element_text(size = 18, face = "bold"),
              axis.title = element_text(size = 15),
              axis.text = element_text(size = 14),
              legend.title = element_text(size = 12),
              legend.text = element_text(size = 10)
            )
        })
        
        column(6,
          plotOutput(session$ns(plot_id), height = plot_height)
        )
      })
      
      # Add download button at the top
      download_section <- fluidRow(
        column(12,
          div(class = "text-center", style = "margin-bottom: 20px;",
            downloadButton(session$ns("downloadGridPlots"),
                          "Download All Plots as PNG",
                          class = "btn-success btn-lg",
                          icon = icon("download"),
                          style = "margin-bottom: 15px;"),
            br(),
            tags$small(class = "text-muted", 
                      paste("Will save", length(markers), "individual plots + 1 combined overview (4-column layout) as high-resolution PNG files"))
          )
        )
      )
      
      # Arrange plots in rows
      grid_rows <- list()
      for (i in seq(1, length(plot_list), by = cols)) {
        row_plots <- plot_list[i:min(i + cols - 1, length(plot_list))]
        grid_rows[[length(grid_rows) + 1]] <- fluidRow(do.call(tagList, row_plots))
      }
      
      do.call(tagList, c(list(download_section), grid_rows))
    })
    
    # Download handler for fast grid plots
    output$downloadGridPlots <- downloadHandler(
      filename = function() {
        paste0("heatmap_grid_plots_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
      },
      content = function(file) {
        req(processedData(), input$heatmapDimMethod)
        
        # Show progress
        withProgress(message = "Generating high-resolution plots...", value = 0, {
          
          # Get current settings
          markers <- processedData()$markers
          plot_data <- processedData()$plot_data
          available_methods <- getAvailableDimMethods(plot_data)
          dim_coords <- available_methods[[input$heatmapDimMethod]]
          dim_labels <- getDimAxisLabels(input$heatmapDimMethod)
          bins_value <- if (!is.null(input$heatmapBins)) input$heatmapBins else 30
          
          # Optimize data for rendering (no point restriction for static plots)
          opt_data <- optimizeHeatmapRendering(plot_data)
          
          # Create temporary directory for PNG files
          temp_dir <- tempdir()
          png_dir <- file.path(temp_dir, "heatmap_plots")
          if (dir.exists(png_dir)) unlink(png_dir, recursive = TRUE)
          dir.create(png_dir, recursive = TRUE)
          
                     # Generate each plot and store for combined plot
           png_files <- character()
           individual_plots <- list()
           
           for (i in seq_along(markers)) {
             incProgress(0.8/length(markers), detail = paste("Generating plot", i, "of", length(markers)))
             
             marker <- markers[i]
             marker_display_name <- getMarkerDisplayName(marker, markerNameMappings())
             
             # Create safe filename
             safe_name <- gsub("[^A-Za-z0-9._-]", "_", marker_display_name)
             png_filename <- file.path(png_dir, paste0(sprintf("%02d", i), "_", safe_name, ".png"))
             
             # Create high-resolution plot
             tryCatch({
               p <- createMarkerExpressionHeatmap(
                 plot_data = opt_data,
                 marker = marker,
                 dim1 = dim_coords[1], 
                 dim2 = dim_coords[2],
                 method = input$heatmapMethod,
                 bins = bins_value,
                 title = marker_display_name,
                 font_size = 18,  # Use same font size as display
                 color_palette = input$heatmapColorPalette,
                 use_qualitative_labels = (input$legendStyle == "qualitative"),
                 label_style = if(input$legendStyle == "qualitative") input$labelStyle else "standard"
               )
               
               # Apply theme matching the display with white background
               p_final <- p + labs(x = dim_labels[1], y = dim_labels[2]) +
                 theme(
                   plot.title = element_text(size = 18, face = "bold"),
                   axis.title = element_text(size = 15),
                   axis.text = element_text(size = 14),
                   legend.title = element_text(size = 12),
                   legend.text = element_text(size = 10),
                   panel.background = element_rect(fill = "white", color = NA),
                   plot.background = element_rect(fill = "white", color = NA),
                   legend.background = element_rect(fill = "white", color = NA)
                 )
               
               # Save individual plot
               ggsave(png_filename, plot = p_final, 
                      width = 10, height = 8, dpi = 300, device = "png", bg = "white")
               
               png_files <- c(png_files, png_filename)
               
               # Store plot for combined layout (with smaller font for grid)
               p_grid <- p + labs(x = dim_labels[1], y = dim_labels[2]) +
                 theme(
                   plot.title = element_text(size = 14, face = "bold"),
                   axis.title = element_text(size = 11),
                   axis.text = element_text(size = 10),
                   legend.title = element_text(size = 10),
                   legend.text = element_text(size = 8),
                   panel.background = element_rect(fill = "white", color = NA),
                   plot.background = element_rect(fill = "white", color = NA),
                   legend.background = element_rect(fill = "white", color = NA),
                   legend.key.size = unit(0.4, "cm")  # Smaller legend for grid
                 )
               
               individual_plots[[i]] <- p_grid
               
             }, error = function(e) {
               message("Error generating plot for ", marker, ": ", e$message)
               individual_plots[[i]] <- NULL
             })
           }
           
           # Create combined plot with 4-column layout
           incProgress(0.15, detail = "Creating combined overview plot...")
           if (length(individual_plots) > 0) {
             # Remove any NULL plots
             valid_plots <- individual_plots[!sapply(individual_plots, is.null)]
             
             if (length(valid_plots) > 0) {
               tryCatch({
                 # Calculate grid dimensions (4 columns)
                 n_plots <- length(valid_plots)
                 n_cols <- 4
                 n_rows <- ceiling(n_plots / n_cols)
                 
                 # Create combined plot filename
                 combined_filename <- file.path(png_dir, "00_COMBINED_All_Markers.png")
                 
                 # Calculate dimensions for combined plot
                 plot_width_per_panel <- 8  # inches per panel
                 plot_height_per_panel <- 6  # inches per panel
                 total_width <- n_cols * plot_width_per_panel
                 total_height <- n_rows * plot_height_per_panel
                 
                 # Create combined plot using gridExtra
                 combined_plot <- gridExtra::grid.arrange(grobs = valid_plots, ncol = n_cols)
                 
                 # Save combined plot
                 ggsave(combined_filename, plot = combined_plot,
                        width = total_width, height = total_height, 
                        dpi = 300, device = "png", bg = "white")
                 
                 png_files <- c(combined_filename, png_files)  # Add to beginning of list
                 
               }, error = function(e) {
                 message("Error creating combined plot: ", e$message)
               })
             }
           }
          
          # Create ZIP file
          if (length(png_files) > 0) {
            # Change to the plot directory to avoid including full paths in ZIP
            old_wd <- setwd(png_dir)
            on.exit(setwd(old_wd))
            
            zip_files <- basename(png_files)
            zip(file, files = zip_files, flags = "-r9X")
            
                         incProgress(0.05, detail = paste("Created ZIP with", length(png_files), "files"))
             
             # Check if combined plot was created
             has_combined <- any(grepl("COMBINED", basename(png_files)))
             individual_count <- length(png_files) - ifelse(has_combined, 1, 0)
             
             if (has_combined) {
               showNotification(paste("Successfully exported", individual_count, "individual plots + 1 combined overview plot"), 
                              type = "message", duration = 6)
             } else {
               showNotification(paste("Successfully exported", individual_count, "individual plots"), 
                              type = "message", duration = 5)
             }
          } else {
            # Create empty file if no plots generated
            writeLines("No plots could be generated", file)
            showNotification("No plots could be generated", type = "error")
          }
        })
      }
    )
    
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
      } else if ("mds1" %in% colnames(plot_data)) {
        dim1 <- "mds1"
        dim2 <- "mds2"
        dim_labels <- c("MDS 1", "MDS 2")
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