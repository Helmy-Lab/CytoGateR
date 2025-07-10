# Enhanced Gating Module with GatingHierarchy Support
# All libraries loaded in global.R

# Note: Helper functions now provided by advanced_gating_helpers.R

# STATIC PLOTTING OPTION FOR LARGE DATASETS
# Alternative to plotly for better performance with full datasets

# Helper function to add current gate visualization
addCurrentGateVisualization <- function(p, gate_coords, gate_type, x_channel, y_channel) {
  if (is.null(gate_coords) || nrow(gate_coords) == 0) {
    return(p)
  }
  
  tryCatch({
    if (gate_type == "polygon") {
      # Show polygon shape - connect points with lines
      if (nrow(gate_coords) >= 2) {
        p <- p + geom_path(data = gate_coords, aes(x = x, y = y), 
                          color = "red", size = 2, alpha = 0.8, inherit.aes = FALSE)
        # If more than 2 points, show the closing line
        if (nrow(gate_coords) >= 3) {
          closing_line <- rbind(gate_coords[nrow(gate_coords), ], gate_coords[1, ])
          p <- p + geom_path(data = closing_line, aes(x = x, y = y), 
                            color = "red", size = 2, alpha = 0.5, linetype = "dashed", inherit.aes = FALSE)
        }
      }
      # Show individual points
      p <- p + geom_point(data = gate_coords, aes(x = x, y = y), 
                         color = "red", size = 4, inherit.aes = FALSE)
      
    } else if (gate_type == "rectangle") {
      # Show rectangle bounding box
      if (nrow(gate_coords) >= 2) {
        x_range <- range(gate_coords$x)
        y_range <- range(gate_coords$y)
        rect_coords <- data.frame(
          x = c(x_range[1], x_range[2], x_range[2], x_range[1], x_range[1]),
          y = c(y_range[1], y_range[1], y_range[2], y_range[2], y_range[1])
        )
        p <- p + geom_path(data = rect_coords, aes(x = x, y = y), 
                          color = "red", size = 2, alpha = 0.8, inherit.aes = FALSE)
      }
      # Show corner points
      p <- p + geom_point(data = gate_coords, aes(x = x, y = y), 
                         color = "red", size = 4, inherit.aes = FALSE)
      
    } else if (gate_type == "ellipse") {
      # Show ellipse approximation
      if (nrow(gate_coords) >= 3) {
        tryCatch({
          # Calculate ellipse center and basic shape
          center_x <- mean(gate_coords$x)
          center_y <- mean(gate_coords$y)
          
          # Simple ellipse approximation using distance from center
          max_dist_x <- max(abs(gate_coords$x - center_x))
          max_dist_y <- max(abs(gate_coords$y - center_y))
          
          # Generate ellipse points
          theta <- seq(0, 2*pi, length.out = 100)
          ellipse_coords <- data.frame(
            x = center_x + max_dist_x * cos(theta),
            y = center_y + max_dist_y * sin(theta)
          )
          
          p <- p + geom_path(data = ellipse_coords, aes(x = x, y = y), 
                            color = "red", size = 2, alpha = 0.8, inherit.aes = FALSE)
        }, error = function(e) {
          # If ellipse calculation fails, just show points
        })
      }
      # Show defining points
      p <- p + geom_point(data = gate_coords, aes(x = x, y = y), 
                         color = "red", size = 4, inherit.aes = FALSE)
      
    } else if (gate_type %in% c("interval", "threshold", "boundary")) {
      # Show vertical or horizontal lines for 1D gates
      if (!is.null(y_channel) && y_channel != "") {
        # 2D plot - show vertical lines for X-axis gates
        for (i in 1:nrow(gate_coords)) {
          p <- p + geom_vline(xintercept = gate_coords$x[i], 
                             color = "red", size = 2, alpha = 0.8)
        }
      } else {
        # 1D plot - show horizontal lines
        for (i in 1:nrow(gate_coords)) {
          p <- p + geom_vline(xintercept = gate_coords$x[i], 
                             color = "red", size = 2, alpha = 0.8)
        }
      }
      # Show points
      p <- p + geom_point(data = gate_coords, aes(x = x, y = y), 
                         color = "red", size = 4, inherit.aes = FALSE)
      
    } else if (gate_type == "quadrant") {
      # Show crosshairs for quadrant gates
      if (nrow(gate_coords) >= 1) {
        center_x <- gate_coords$x[nrow(gate_coords)]
        center_y <- gate_coords$y[nrow(gate_coords)]
        
        p <- p + 
          geom_vline(xintercept = center_x, color = "red", size = 2, alpha = 0.8) +
          geom_hline(yintercept = center_y, color = "red", size = 2, alpha = 0.8) +
          geom_point(data = gate_coords, aes(x = x, y = y), 
                    color = "red", size = 6, shape = 3, inherit.aes = FALSE)  # Cross shape
      }
      
    } else {
      # Default: just show points for other gate types
      p <- p + geom_point(data = gate_coords, aes(x = x, y = y), 
                         color = "red", size = 4, inherit.aes = FALSE)
    }
    
    return(p)
    
  }, error = function(e) {
    message("Error adding gate visualization: ", e$message)
    return(p)
  })
}

createStaticGatingPlot <- function(plot_data_full, x_channel, y_channel = NULL, 
                                  sample_name, parent_population, input_vals, 
                                  existing_gates = NULL, current_gate_coords = NULL, 
                                  current_gate_type = NULL) {
  tryCatch({
    # Use ALL data - no subsampling for static plots
    message("=== STATIC PLOT FUNCTION DEBUG ===")
    message("- Input data rows: ", nrow(plot_data_full))
    message("- Input data cols: ", ncol(plot_data_full))
    message("- X channel: ", x_channel)
    message("- Y channel: ", ifelse(is.null(y_channel), "NULL", y_channel))
    message("- Sample name: ", sample_name)
    message("- Parent population: ", parent_population)
    message("- Plot type: ", input_vals$plot_type)
    
    # Create base plot using full data
    if (!is.null(y_channel) && y_channel != "") {
      # 2D plot
      if (!is.null(input_vals$color_channel) && input_vals$color_channel != "density" && 
          input_vals$color_channel %in% names(plot_data_full)) {
        # Color by specific channel
        p <- ggplot(plot_data_full, aes(x = .data[[x_channel]], 
                                       y = .data[[y_channel]], 
                                       color = .data[[input_vals$color_channel]]))
        color_legend_title <- input_vals$color_channel
      } else {
        # Standard 2D plot without specific coloring
        p <- ggplot(plot_data_full, aes(x = .data[[x_channel]], 
                                       y = .data[[y_channel]]))
        color_legend_title <- "Density"
      }
      
      p <- p + theme_minimal() +
        labs(title = paste("Sample:", sample_name, "| Parent:", parent_population, 
                          "| Events:", format(nrow(plot_data_full), big.mark = ",")),
             x = x_channel, y = y_channel)
      
      # Add plot type specific geometry - optimized for static rendering with flow cytometry colors
      if (input_vals$plot_type == "scatter") {
        # Use flow cytometry blue for scatter plots
        p <- p + geom_point(alpha = input_vals$alpha_level, size = input_vals$point_size, color = "#0066CC")
        
      } else if (input_vals$plot_type == "scatter_colored") {
        if (!is.null(input_vals$color_channel) && input_vals$color_channel != "density" && 
            input_vals$color_channel %in% names(plot_data_full)) {
          # Color by specific channel - use flow cytometry color scale
          color_scale_name <- if(!is.null(input_vals$color_scale)) input_vals$color_scale else "flow_classic"
          p <- p + geom_point(alpha = input_vals$alpha_level, size = input_vals$point_size) +
            scale_color_gradientn(colors = getFlowCytometryColors(color_scale_name), 
                                  name = input_vals$color_channel)
        } else {
          # Point density coloring using ggpointdensity - hotspots where cells overlap
          color_scale_name <- if(!is.null(input_vals$color_scale)) input_vals$color_scale else "density_optimized"
          
          # Recreate plot with density aesthetic mapping - following ggpointdensity vignette pattern
          p <- ggplot(plot_data_full, aes(x = .data[[x_channel]], y = .data[[y_channel]], 
                                         color = after_stat(ndensity))) +
            theme_minimal() +
            labs(title = paste("Sample:", sample_name, "| Parent:", parent_population,
                              "| Events:", format(nrow(plot_data_full), big.mark = ",")),
                 x = x_channel, y = y_channel) +
            geom_pointdensity(alpha = input_vals$alpha_level, 
                            size = input_vals$point_size, 
                            adjust = input_vals$density_adjust) +
            scale_color_gradientn(colors = getFlowCytometryColors(color_scale_name), 
                                name = "Point Density")
        }
        
      } else if (input_vals$plot_type == "hex") {
        # Hexagon binning with flow cytometry colors - excellent for large datasets
        color_scale_name <- if(!is.null(input_vals$color_scale)) input_vals$color_scale else "flow_classic"
        p <- p + geom_hex(alpha = input_vals$alpha_level, bins = 50) +
          getColorScale(color_scale_name, type = "fill", name = "Count", data_values = NULL, force_discrete = FALSE)
          
      } else {
        # Default to hex for performance with flow cytometry colors
        color_scale_name <- if(!is.null(input_vals$color_scale)) input_vals$color_scale else "flow_classic"
        p <- p + geom_hex(alpha = input_vals$alpha_level, bins = 50) +
          getColorScale(color_scale_name, type = "fill", name = "Count", data_values = NULL, force_discrete = FALSE)
      }
      
    } else {
      # 1D histogram - very fast for large datasets with flow cytometry colors
      color_scale_name <- if(!is.null(input_vals$color_scale)) input_vals$color_scale else "flow_classic"
      flow_colors <- getFlowCytometryColors(color_scale_name)
      
      p <- ggplot(plot_data_full, aes(x = .data[[x_channel]])) +
        geom_histogram(bins = 100, alpha = input_vals$alpha_level, fill = flow_colors[3]) +  # Use mid-range flow color
        theme_minimal() +
        labs(title = paste("Sample:", sample_name, "| Parent:", parent_population,
                          "| Events:", format(nrow(plot_data_full), big.mark = ",")),
             x = x_channel, y = "Count")
    }
    
    # Add existing gates if any
    if (!is.null(existing_gates)) {
      p <- addExistingGates(p, existing_gates)
      message("- Added existing gates to plot")
    }
    
    # Add current gate being drawn (real-time visualization)
    if (!is.null(current_gate_coords) && !is.null(current_gate_type)) {
      p <- addCurrentGateVisualization(p, current_gate_coords, current_gate_type, x_channel, y_channel)
      message("- Added current gate visualization (", current_gate_type, " with ", nrow(current_gate_coords), " points)")
      
      # Add drawing mode indicator and helpful legend
      required_points <- switch(current_gate_type,
        "rectangle" = 2, "polygon" = 3, "ellipse" = 3, "interval" = 2,
        "threshold" = 1, "boundary" = 1, "quadrant" = 1, 2)
      
      progress_text <- if (nrow(current_gate_coords) >= required_points) {
        "Ready to save!"
      } else {
        paste("Need", required_points - nrow(current_gate_coords), "more points")
      }
      
      p <- p + labs(subtitle = paste("Drawing", current_gate_type, "gate -", 
                                     nrow(current_gate_coords), "points selected |", progress_text))
    }
    
    message("- Static plot created successfully")
    return(p)
    
  }, error = function(e) {
    # Return empty plot on error
    ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
      theme_void()
  })
}

# UI Function
gatingModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    useShinyjs(),
    
    # Enhanced CSS for better styling
    tags$head(
      tags$style(HTML("
        .gate-button { 
          margin: 2px; 
          min-height: 40px;
          border-radius: 5px;
        }
        .gate-tools { 
          background-color: #f8f9fa; 
          padding: 15px; 
          border-radius: 8px; 
          margin-bottom: 10px;
          border: 1px solid #dee2e6;
        }
        .hierarchy-panel { 
          background-color: #ffffff; 
          border: 1px solid #dee2e6; 
          border-radius: 8px;
          padding: 10px;
        }
        .gate-type-selector {
          background-color: #e3f2fd;
          padding: 10px;
          border-radius: 5px;
          margin-bottom: 10px;
        }
        .population-stats {
          background-color: #f1f8e9;
          padding: 8px;
          border-radius: 5px;
          margin: 5px 0;
        }
        .analysis-workflow {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 15px;
          border-radius: 8px;
          margin-bottom: 20px;
        }
      "))
    ),
    
    # Workflow Progress Header
    div(class = "analysis-workflow",
        h3(icon("vector-square"), "Interactive Gating Workflow"),
        p("Create gates to define cell populations, build gating hierarchies, and extract gated data for analysis")
    ),
    
    fluidRow(
      # Left Panel - Enhanced Controls
      column(3,
        shinydashboard::box(
          title = "Gating Controls", status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          
          # Data Source Section
          div(class = "gate-type-selector",
            h5(icon("database"), "Data Source"),
            radioButtons(ns("data_source"), NULL,
                         choices = list(
                           "Use data from Raw Data tab" = "existing",
                           "Load new FCS files" = "new"
                         ),
                         selected = "existing"),
            
            # File upload (conditional)
            conditionalPanel(
              condition = paste0("input['", ns("data_source"), "'] == 'new'"),
              fileInput(ns("fcs_files"), "Upload FCS Files:",
                        multiple = TRUE,
                        accept = c(".fcs")),
              helpText("Upload multiple FCS files for gating analysis")
            )
          ),
          
          # Data Status and Sample Selection
          conditionalPanel(
            condition = paste0("output['", ns("data_status"), "'] == 'ready'"),
            div(class = "alert alert-success", style = "padding: 8px; margin: 10px 0;",
                icon("check-circle"), " Data loaded and ready for gating")
          ),
          
          conditionalPanel(
            condition = paste0("output['", ns("data_status"), "'] == 'no_data'"),
            div(class = "alert alert-warning", style = "padding: 8px; margin: 10px 0;",
                icon("exclamation-triangle"), " No data available - load data first")
          ),
          
          # Sample Selection
          selectInput(ns("sample_select"), "Select Sample:",
                      choices = NULL),
          
          # Parent Population Selection
          selectInput(ns("parent_population"), "Parent Population:",
                      choices = c("root" = "root"),
                      selected = "root"),
          
          # Channel Selection
          selectInput(ns("x_channel"), "X-Axis Channel:",
                      choices = NULL),
          selectInput(ns("y_channel"), "Y-Axis Channel (2D plots):",
                      choices = NULL),
          
          # Gate Type Selection with Descriptions
          div(class = "gate-type-selector",
            h5(icon("vector-square"), "Gate Type"),
            selectInput(ns("gate_type"), NULL,
                        choices = list(
                          "Polygon" = "polygon",
                          "Rectangle" = "rectangle", 
                          "Ellipse" = "ellipse",
                          "Interval" = "interval",
                          "Threshold" = "threshold",
                          "Boundary" = "boundary",
                          "Quadrant" = "quadrant",
                          "Web" = "web"
                        ),
                        selected = "polygon"),
            
            # Gate type description (dynamic)
            uiOutput(ns("gate_type_description"))
          ),
          
          # Population Name Input
          textInput(ns("population_name"), "Population Name:",
                    placeholder = "Enter population name"),
          
          # Advanced Options
          checkboxInput(ns("show_statistics"), "Show Population Statistics", TRUE),
          checkboxInput(ns("auto_label"), "Auto-label Populations", TRUE),
          
          # Gate Drawing Actions
          div(class = "gate-tools",
            h5(icon("mouse-pointer"), "Gate Actions"),
            actionButton(ns("start_gating"), "Start Gating", 
                         class = "btn-success gate-button",
                         icon = icon("play")),
            br(),
            actionButton(ns("save_gate"), "Save Gate", 
                         class = "btn-primary gate-button",
                         icon = icon("save")),
            actionButton(ns("cancel_gate"), "Cancel Gate", 
                         class = "btn-warning gate-button", 
                         icon = icon("times")),
            br(),
            actionButton(ns("edit_gate"), "Edit Selected Gate", 
                         class = "btn-info gate-button",
                         icon = icon("edit")),
            actionButton(ns("delete_gate"), "Delete Selected Gate", 
                         class = "btn-danger gate-button",
                         icon = icon("trash")),
          br(),
          actionButton(ns("refresh_hierarchy"), "Refresh Display", 
                       class = "btn-info gate-button",
                       icon = icon("refresh"),
                       title = "Force refresh of population display")
          )
        ),
        

      ),
      
      # Center Panel - Enhanced Plot Area
      column(6,
        shinydashboard::box(
          title = "Interactive Gating Plot", status = "primary", solidHeader = TRUE,
          width = 12,
          
          # Plot Controls
          fluidRow(
            column(3,
              selectInput(ns("plot_mode"), "Plot Mode:",
                          choices = list(
                            "Interactive (Plotly)" = "interactive",
                            "Static (Full Dataset)" = "static"
                          ),
                          selected = "static")
            ),
            column(4,
              selectInput(ns("plot_type"), "Plot Type:",
                          choices = list(
                            "Scatter Plot" = "scatter",
                            "Density Hotspots" = "scatter_colored",
                            "Hotspots + Contours" = "contour",
                            "Hexagon Plot" = "hex"
                          ),
                          selected = "hex")  # Default to hex for better performance
            ),
            column(3,
              selectInput(ns("color_channel"), "Color Channel:",
                          choices = NULL,
                          selected = NULL)
            ),
            column(2,
              numericInput(ns("point_size"), "Point Size:", 
                           value = 0.5, min = 0.1, max = 2, step = 0.1)
            ),
            
            # Density bandwidth control (only for density plot types)
            conditionalPanel(
              condition = paste0("input['", ns("plot_type"), "'] == 'scatter_colored' || input['", ns("plot_type"), "'] == 'contour'"),
              column(2,
                numericInput(ns("density_adjust"), "Density Bandwidth:",
                             value = 0.5, min = 0.1, max = 2.0, step = 0.1),
                helpText(style = "font-size: 10px; margin-top: -5px;", 
                        "Lower = tighter hotspots")
              )
            )
          ),
          
          # Mode-specific information
          conditionalPanel(
            condition = paste0("input['", ns("plot_mode"), "'] == 'static'"),
            div(class = "alert alert-info", style = "margin: 5px 0; padding: 8px;",
                icon("info-circle"), " Static Mode: Displays ALL events, optimized for large datasets. 
                Use brush selection or click for gating.")
          ),
          conditionalPanel(
            condition = paste0("input['", ns("plot_mode"), "'] == 'interactive'"),
            div(class = "alert alert-warning", style = "margin: 5px 0; padding: 8px;",
                icon("exclamation-triangle"), " Interactive Mode: Limited to 10,000 events for performance. 
                Full plotly toolbar available.")
          ),
          
          # Plot type information
          div(class = "alert alert-light", style = "margin: 5px 0; padding: 6px; font-size: 11px;",
              HTML("<strong>Plot Types:</strong> 
                    <em>Scatter</em> - Basic points, 
                    <em>Density Hotspots</em> - Points colored by local density (shows cell clusters), 
                    <em>Hotspots + Contours</em> - Combines hotspots with population structure lines, 
                    <em>Hexagon</em> - Binned counts for large datasets")),
           
           # Additional Color Options
          fluidRow(
            column(6,
              selectInput(ns("color_scale"), "Color Scale:",
                          choices = list(
                            "Blue-Green-Yellow" = "flow_classic",
                            "Blue-White-Red" = "flowjo_style", 
                            "Density" = "density_optimized",
                            "Viridis" = "viridis",
                            "Turbo" = "turbo",
                            "Plasma" = "plasma",
                            "Spectral" = "spectral",
                            "Heat" = "heat"
                          ),
                          selected = "flow_classic"),
              helpText(icon("palette"), "Flow cytometry palettes optimize cell population visualization with proper density gradients")
            ),
            column(6,
              sliderInput(ns("alpha_level"), "Transparency:",
                          min = 0.1, max = 1.0, value = 0.6, step = 0.1)
            )
          ),
          
          # Plot Dimensions Control
          fluidRow(
            column(3,
              numericInput(ns("plot_width"), "Width (px):",
                           value = 600, min = 300, max = 1200, step = 50)
            ),
            column(3,
              numericInput(ns("plot_height"), "Height (px):",
                           value = 600, min = 300, max = 1200, step = 50)
            ),
            column(3,
              checkboxInput(ns("lock_aspect"), "Lock Square (1:1)", value = FALSE)
            ),
            column(3,
              actionButton(ns("reset_dimensions"), "Reset Size",
                           class = "btn-outline-secondary btn-sm",
                           style = "margin-top: 25px;")
            )
          ),
          
          # Main Plot Output (dynamic height controlled by user inputs)
          uiOutput(ns("dynamic_plot_container")),
          
          # Plot Status and Instructions
          div(id = ns("plot_instructions"), class = "alert alert-info",
              style = "margin-top: 10px;",
              icon("lightbulb"), 
              textOutput(ns("instruction_text"), inline = TRUE)),
          
          # Debug: Show current plot mode
          div(class = "alert alert-secondary", style = "margin-top: 5px; font-size: 11px; padding: 5px;",
              strong("Debug - Plot Mode: "), textOutput(ns("debug_plot_mode"), inline = TRUE)),
          
          # Debug/Status Display
          conditionalPanel(
            condition = paste0("input['", ns("data_source"), "'] == 'existing' || input['", ns("fcs_files"), "'] != null"),
            div(class = "alert alert-secondary", style = "margin-top: 5px; font-size: 12px;",
                strong("Status: "), textOutput(ns("selection_status"), inline = TRUE)),
            
            # Data Range Display
            div(class = "alert alert-light", style = "margin-top: 5px; font-size: 11px; padding: 8px;",
                strong("Data Ranges: "), textOutput(ns("data_ranges"), inline = TRUE))
          ),
          
          # Population Statistics Display
          conditionalPanel(
            condition = paste0("input['", ns("show_statistics"), "']"),
            div(class = "population-stats",
                h5(icon("chart-bar"), "Population Statistics"),
                verbatimTextOutput(ns("gate_statistics"))
            )
          )
        )
      ),
      
      # Right Panel - Enhanced Hierarchy and Analysis
      column(3,
        # Gating Hierarchy
        shinydashboard::box(
          title = "Gating Hierarchy", status = "success", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          
          div(class = "hierarchy-panel",
            # Hierarchy Tree Display
            htmlOutput(ns("hierarchy_tree")),
            
            # Hierarchy Actions
            hr(),
            fluidRow(
              column(6,
                actionButton(ns("collapse_all"), "Collapse All",
                             class = "btn-sm btn-outline-secondary",
                             icon = icon("minus"))
              ),
              column(6,
                actionButton(ns("expand_all"), "Expand All", 
                             class = "btn-sm btn-outline-secondary",
                             icon = icon("plus"))
              )
            )
          )
        ),
        
        # Population Analysis
        shinydashboard::box(
          title = "Population Analysis", status = "info", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          
          # Population Count Table
          DT::dataTableOutput(ns("population_table")),
          
          # Gated Data Management
          hr(),
          h5(icon("database"), "Gated Data Management"),
          
          # Select population for extraction
          selectInput(ns("extract_population"), "Select Population to Extract:",
                      choices = NULL, width = "100%"),
          
          fluidRow(
            column(6,
              actionButton(ns("apply_gates"), "Apply All Gates",
                           class = "btn-primary btn-sm",
                           icon = icon("check-circle"),
                           title = "Apply all gates and update data flow")
            ),
            column(6,
              actionButton(ns("extract_gated_data"), "Extract Gated Data",
                           class = "btn-success btn-sm", 
                           icon = icon("database"),
                           title = "Extract selected population as flowSet")
            )
          ),
          
          br(),
          
          # Export Options
          h5(icon("download"), "Export Options"),
          fluidRow(
            column(6,
              downloadButton(ns("export_gated_fcs"), "Export Gated FCS",
                             class = "btn-warning btn-sm",
                             icon = icon("file-export"),
                             title = "Export gated population as FCS files")
            ),
            column(6,
              downloadButton(ns("export_gates"), "Export Gates",
                             class = "btn-info btn-sm",
                             icon = icon("download"),
                             title = "Export gate definitions as CSV")
            )
          ),
          
          br(),
          
          downloadButton(ns("export_populations"), "Export Population Stats", 
                         class = "btn-success btn-sm",
                         icon = icon("table"),
                         title = "Export population statistics"),
          
          # Status Display
          br(), br(),
          div(id = ns("gating_status"), class = "alert alert-secondary",
              style = "margin-top: 10px; font-size: 12px;",
              textOutput(ns("gating_status_text")))
        ),
        
        # Template Management  
        shinydashboard::box(
          title = "Gating Templates", status = "warning", solidHeader = TRUE,
          width = 12, collapsible = TRUE, collapsed = TRUE,
          
          fileInput(ns("load_template"), "Load Gating Template:",
                    accept = c(".csv"),
                    placeholder = "Upload gating template CSV"),
          
          textInput(ns("template_name"), "Template Name:",
                    placeholder = "Enter template name"),
          
          actionButton(ns("save_template"), "Save Template",
                       class = "btn-warning",
                       icon = icon("save")),
          
          br(), br(),
          
          # Template Library
          h5("Template Library"),
          selectInput(ns("template_library"), NULL,
                      choices = c("No templates available" = "")),
          
          actionButton(ns("apply_template"), "Apply Template",
                       class = "btn-success", 
                       icon = icon("play"))
        )
      )
    )
  )
}

# Server Function
gatingModuleServer <- function(id, app_state, raw_data_results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      current_gs = NULL,
      current_data = NULL,
      current_gh = NULL,  # NEW: Current GatingHierarchy for selected sample
      initialization_in_progress = FALSE,
      selected_gate = NULL,
      gate_coords = NULL,
      selection_type = NULL,
      drawing_mode = FALSE,
      gate_history = list(),
      current_plot_data = NULL,  # Subsampled data for display only
      full_population_data = NULL,  # Full data for accurate gating
      available_channels = NULL,
      population_stats = NULL,
      instruction_message = "Select gate type and click 'Start Gating' to begin",
      # Gated data management
      gated_data_applied = FALSE,
      applied_gates_timestamp = NULL,
      extracted_population = NULL,
      extracted_flowset = NULL,
      extraction_timestamp = NULL,
      # Reactive trigger for hierarchy updates
      hierarchy_trigger = 0
    )
    
    # NEW: Helper function to get current GatingHierarchy
    getCurrentGH <- function() {
      if (!is.null(values$current_gs) && !is.null(input$sample_select)) {
        sample_idx <- which(sampleNames(values$current_gs) == input$sample_select)
        if (length(sample_idx) > 0) {
          return(values$current_gs[[sample_idx]])
        }
      }
      return(NULL)
    }
    
    # NEW: Simplified population count function using GatingHierarchy
    getPopulationCounts <- function(gh = NULL) {
      if (is.null(gh)) gh <- getCurrentGH()
      if (is.null(gh)) return(NULL)
      
      tryCatch({
        # Get population paths
        pop_paths <- gh_get_pop_paths(gh)
        
        # Get counts for each population  
        pop_counts <- data.frame(
          Population = pop_paths,
          Count = sapply(pop_paths, function(p) gh_pop_get_count(gh, p)),
          stringsAsFactors = FALSE
        )
        
        message("GatingHierarchy population counts retrieved successfully")
        return(pop_counts)
        
      }, error = function(e) {
        message("Error getting GatingHierarchy counts: ", e$message)
        return(NULL)
      })
    }
    
    # Gate type description
    output$gate_type_description <- renderUI({
      gate_descriptions <- list(
        "polygon" = "Draw freeform polygon gates by clicking points",
        "rectangle" = "Draw rectangular gates with 2 diagonal points", 
        "ellipse" = "Draw elliptical gates around populations",
        "interval" = "Select range on one axis (1D or 2D)",
        "threshold" = "Select minimum threshold value",
        "boundary" = "Select maximum boundary value", 
        "quadrant" = "Create 4 quadrant gates with crosshairs",
        "web" = "Advanced multi-population web gating"
      )
      
      selected_type <- input$gate_type
      if (is.null(selected_type)) selected_type <- "polygon"
      
      description <- gate_descriptions[[selected_type]]
      if (is.null(description)) description <- "Select a gate type to see instructions"
      
      helpText(icon("info-circle"), description)
    })
    
    # Instruction text output
    output$instruction_text <- renderText({
      values$instruction_message
    })
    
    # Debug: Plot mode output
    output$debug_plot_mode <- renderText({
      plot_mode <- if (!is.null(input$plot_mode)) input$plot_mode else "NULL"
      data_status <- if (!is.null(values$current_gs)) "GatingSet loaded" else "No data"
      paste(plot_mode, "|", data_status)
    })
    
    # Selection status output for debugging
    output$selection_status <- renderText({
      if (values$drawing_mode) {
        if (!is.null(values$gate_coords) && !is.null(values$selection_type)) {
          n_coords <- if(is.data.frame(values$gate_coords)) nrow(values$gate_coords) else length(values$gate_coords)
          paste("Drawing mode ON | Selection type:", values$selection_type, "| Points:", n_coords)
        } else {
          "Drawing mode ON | No selection yet - click on plot or use toolbar tools"
        }
      } else {
        "Drawing mode OFF | Click 'Start Gating' to begin"
      }
    })
    
    # Data ranges output for user reference
    output$data_ranges <- renderText({
      req(values$full_population_data, input$x_channel)
      
      tryCatch({
        x_range <- range(values$full_population_data[[input$x_channel]], na.rm = TRUE)
        range_text <- paste("X (", input$x_channel, "):", round(x_range[1], 1), "to", round(x_range[2], 1))
        
        if (!is.null(input$y_channel) && input$y_channel != "" && input$y_channel %in% names(values$full_population_data)) {
          y_range <- range(values$full_population_data[[input$y_channel]], na.rm = TRUE)
          range_text <- paste0(range_text, " | Y (", input$y_channel, "):", round(y_range[1], 1), "to", round(y_range[2], 1))
        }
        
        # Add total event count
        range_text <- paste0(range_text, " | Events: ", format(nrow(values$full_population_data), big.mark = ","))
        
        return(range_text)
        
      }, error = function(e) {
        return("Data ranges not available")
      })
    })
    
    # Reset GatingSet when data source changes
    observeEvent(input$data_source, {
      if (!is.null(values$current_gs)) {
        message("Data source changed - resetting GatingSet")
        values$current_gs <- NULL
        values$current_data <- NULL
        values$available_channels <- NULL
        values$initialization_in_progress <- FALSE
        values$hierarchy_trigger <- values$hierarchy_trigger + 1
      }
    }, ignoreInit = TRUE)
    
    # Reset GatingSet when new FCS files are uploaded
    observeEvent(input$fcs_files, {
      if (!is.null(values$current_gs) && input$data_source == "new") {
        message("New FCS files uploaded - resetting GatingSet")
        values$current_gs <- NULL
        values$current_data <- NULL
        values$available_channels <- NULL
        values$initialization_in_progress <- FALSE
        values$hierarchy_trigger <- values$hierarchy_trigger + 1
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # Data source management
    current_data <- reactive({
      if (input$data_source == "existing") {
        # Use data from raw data module - need the original flow data, not processed matrices
        processed_data <- raw_data_results$processed_data()
        if (!is.null(processed_data)) {
          # Try to get the flowSet from qc_data or gated_data (these should be flowCore objects)
          if (!is.null(processed_data$qc_data) && is(processed_data$qc_data, "flowSet")) {
            return(processed_data$qc_data)
          } else if (!is.null(processed_data$gated_data) && is(processed_data$gated_data, "flowSet")) {
            return(processed_data$gated_data)
          } else if (!is.null(processed_data$raw_data) && is(processed_data$raw_data, "flowSet")) {
            return(processed_data$raw_data)
          } else {
            # More helpful error message
            available_data <- names(processed_data)[!sapply(processed_data, is.null)]
            showNotification(
              paste("No flowSet data found from Raw Data module.", 
                    "Available data types:", paste(available_data, collapse = ", "),
                    "Please run analysis in Raw Data tab first, or use 'Load new FCS files' option."), 
              type = "warning", duration = 8)
            return(NULL)
          }
        } else {
                      showNotification("No data available from Raw Data tab. Please load FCS files and run analysis first, or use 'Load new FCS files' option below.", 
                             type = "message", duration = 8)
          return(NULL)
        }
      } else {
        # Use uploaded FCS files
        req(input$fcs_files)
        
        tryCatch({
          file_paths <- input$fcs_files$datapath
          file_names <- input$fcs_files$name
          
          # Read FCS files
          fcs_list <- lapply(file_paths, function(path) {
            read.FCS(path, transformation = FALSE)
          })
          names(fcs_list) <- tools::file_path_sans_ext(file_names)
          
          # Create flowSet
          fs <- flowSet(fcs_list)
          return(fs)
          
        }, error = function(e) {
          showNotification(paste("Error loading FCS files:", e$message), type = "error")
          return(NULL)
        })
      }
    })
    
    # Initialize GatingSet when data source changes
    observeEvent(current_data(), {
      data <- current_data()
      req(data)
      
      # Guard against infinite loops - only initialize if we don't already have a valid GatingSet
      if (!is.null(values$current_gs) || values$initialization_in_progress) {
        message("GatingSet already exists or initialization in progress, skipping re-initialization")
        return()
      }
      
      # Set initialization flag
      values$initialization_in_progress <- TRUE
      message("Starting GatingSet initialization...")
      
      tryCatch({
        # Create GatingSet from current data
        if (is(data, "flowSet")) {
          gs <- GatingSet(data)
        } else if (is(data, "flowFrame")) {
          # Convert single flowFrame to flowSet then GatingSet
          fs <- flowSet(data)
          gs <- GatingSet(fs)
        } else {
          stop("Unsupported data type for gating")
        }
        
        # Apply compensation if available from raw data module
        if (input$data_source == "existing") {
          processed_data <- raw_data_results$processed_data()
          if (!is.null(processed_data) && !is.null(processed_data$compensation_matrix)) {
            gs <- compensate(gs, processed_data$compensation_matrix)
          }
        } else {
          # For uploaded files, try to extract compensation from first file
          if (is(data, "flowSet")) {
            comp_matrix <- description(data[[1]])$SPILL
            if (!is.null(comp_matrix)) {
              gs <- compensate(gs, comp_matrix)
            }
          }
        }
        
        # Apply transformations (logicle for fluorescent channels)
        channels <- colnames(gs)
        fluor_channels <- channels[!grepl("FSC|SSC|Time|Width|Height", channels, ignore.case = TRUE)]
        
        if (length(fluor_channels) > 0) {
          # Create logicle transformations
          trans_list <- lapply(fluor_channels, function(ch) {
            flowjo_biexp_trans()
          })
          names(trans_list) <- fluor_channels
          trans <- transformerList(fluor_channels, trans_list)
          gs <- transform(gs, trans)
        }
        
        # Initialize GatingSet with recompute (GatingHierarchy will handle population counting)
        tryCatch({
          recompute(gs)
          message("GatingSet initialized and recomputed successfully")
          
          # Verify basic structure
          all_pops <- gs_get_pop_paths(gs)
          message("Available populations: ", paste(all_pops, collapse = ", "))
          
        }, error = function(e) {
          message("Warning: Could not recompute GatingSet during initialization: ", e$message)
        })
        
        # Store in reactive values
        values$current_gs <- gs
        values$current_data <- data
        values$available_channels <- channels
        
        # Update UI elements
        updateSelectInput(session, "sample_select",
                          choices = sampleNames(gs))
        
        # Update channel choices - separate fluorescent and scatter channels
        scatter_channels <- channels[grepl("FSC|SSC", channels, ignore.case = TRUE)]
        fluor_channels <- channels[!grepl("FSC|SSC|Time|Width|Height", channels, ignore.case = TRUE)]
        
        all_channels <- c(scatter_channels, fluor_channels)
        
        # Set default selections for better user experience
        default_x <- if (length(scatter_channels) >= 1) scatter_channels[1] else all_channels[1]
        default_y <- if (length(scatter_channels) >= 2) scatter_channels[2] else if (length(all_channels) >= 2) all_channels[2] else ""
        
        updateSelectInput(session, "x_channel", 
                          choices = all_channels,
                          selected = default_x)
        updateSelectInput(session, "y_channel", 
                          choices = all_channels,
                          selected = default_y)
        
        message("Channel selection updated - X: ", default_x, ", Y: ", default_y)
        
        # Update color channel choices - prioritize fluorescent channels for coloring
        color_channel_choices <- c("None (Density-based)" = "density", all_channels)
        updateSelectInput(session, "color_channel", 
                          choices = color_channel_choices,
                          selected = "density")  # Default to density to avoid scale errors
        
        # Initialize hierarchy trigger for GatingHierarchy-based updates
        values$hierarchy_trigger <- 1
        
        showNotification("Data loaded successfully for gating! Population counts will be handled by GatingHierarchy.", type = "message")
        
        # Clear initialization flag on success
        values$initialization_in_progress <- FALSE
        message("GatingSet initialization completed - GatingHierarchy ready for population counting")
        
      }, error = function(e) {
        showNotification(paste("Error initializing GatingSet:", e$message), type = "error")
        values$current_gs <- NULL
        # Clear initialization flag on error
        values$initialization_in_progress <- FALSE
        message("GatingSet initialization failed, flag cleared")
      })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # Data status indicator
    output$data_status <- reactive({
      if (!is.null(values$current_gs)) {
        return("ready")
      } else {
        return("no_data")
      }
    })
    outputOptions(output, "data_status", suspendWhenHidden = FALSE)
    
    # Plot dimension controls
    observeEvent(input$lock_aspect, {
      if (input$lock_aspect) {
        # When locking aspect ratio, make height match width
        updateNumericInput(session, "plot_height", value = input$plot_width)
      }
    })
    
    # Sync height to width when aspect is locked
    observeEvent(input$plot_width, {
      if (!is.null(input$lock_aspect) && input$lock_aspect) {
        updateNumericInput(session, "plot_height", value = input$plot_width)
      }
    })
    
    # Sync width to height when aspect is locked  
    observeEvent(input$plot_height, {
      if (!is.null(input$lock_aspect) && input$lock_aspect) {
        updateNumericInput(session, "plot_width", value = input$plot_height)
      }
    })
    
    # Reset dimensions button
    observeEvent(input$reset_dimensions, {
      updateNumericInput(session, "plot_width", value = 600)
      updateNumericInput(session, "plot_height", value = 500)
      updateCheckboxInput(session, "lock_aspect", value = FALSE)
      showNotification("Plot dimensions reset to default", type = "message")
    })
    
    # Dynamic plot container - chooses static or interactive based on mode
    output$dynamic_plot_container <- renderUI({
      # Use default values if inputs not available yet, with validation
      plot_width <- if (!is.null(input$plot_width)) {
        max(300, min(1200, input$plot_width))  # Clamp between 300-1200
      } else {
        600
      }
      plot_height <- if (!is.null(input$plot_height)) {
        max(300, min(1200, input$plot_height))  # Clamp between 300-1200
      } else {
        600
      }
      
      # Choose plot type based on selected mode
      if (!is.null(input$plot_mode) && input$plot_mode == "static") {
        plotOutput(ns("static_plot"), 
                   width = paste0(plot_width, "px"),
                   height = paste0(plot_height, "px"),
                   brush = brushOpts(id = ns("plot_brush"), resetOnNew = FALSE),
                   click = clickOpts(id = ns("plot_click")))
      } else {
        plotlyOutput(ns("main_plot"), 
                     width = paste0(plot_width, "px"),
                     height = paste0(plot_height, "px"))
      }
    })
    
    # Update parent population choices using GatingHierarchy
    observe({
      req(values$current_gs, input$sample_select)
      # React to hierarchy changes
      values$hierarchy_trigger
      
      tryCatch({
        # Get current GatingHierarchy for selected sample
        gh <- getCurrentGH()
        if (is.null(gh)) {
          updateSelectInput(session, "parent_population", 
                            choices = list("root" = "root"),
                            selected = "root")
          return()
        }
        
        # Get population counts using simplified GatingHierarchy approach
        pop_counts <- getPopulationCounts(gh)
        
        if (is.null(pop_counts) || nrow(pop_counts) == 0) {
          updateSelectInput(session, "parent_population", 
                            choices = list("root" = "root"),
                            selected = "root")
          return()
        }
        
        # Handle NA values in counts
        safe_counts <- ifelse(is.na(pop_counts$Count), 0, pop_counts$Count)
        
        # Create labels with counts
        labels <- paste0(pop_counts$Population, " (", format(safe_counts, big.mark = ","), " events)")
        
        # Keep current selection if it's still valid
        current_selection <- input$parent_population
        if (is.null(current_selection) || !(current_selection %in% pop_counts$Population)) {
          current_selection <- "root"
        }
        
        updateSelectInput(session, "parent_population",
                          choices = setNames(pop_counts$Population, labels),
                          selected = current_selection)
                          
        message("Updated parent population choices using GatingHierarchy")
        
      }, error = function(e) {
        message("Error in GatingHierarchy population update: ", e$message)
        # Set safe default on error
        updateSelectInput(session, "parent_population", 
                          choices = list("root" = "root"),
                          selected = "root")
      })
    })
    

    
    # Static gating plot (for full dataset display)
    output$static_plot <- renderPlot({
      # React to gate coordinate changes for real-time visualization
      gate_coords_trigger <- values$gate_coords
      gate_type_trigger <- input$gate_type
      drawing_mode_trigger <- values$drawing_mode
      
      # Check if we have basic data first
      if (is.null(values$current_gs)) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "No data loaded. Please upload FCS files and run analysis.",
                        size = 4, color = "orange") +
               theme_void())
      }
      
      # Check if sample is selected
      if (is.null(input$sample_select) || input$sample_select == "") {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = "Please select a sample from the dropdown.",
                        size = 4, color = "blue") +
               theme_void())
      }
      
      # Check if X channel is selected
      if (is.null(input$x_channel) || input$x_channel == "") {
        available_channels <- if (!is.null(values$available_channels)) {
          paste(head(values$available_channels, 5), collapse = ", ")
        } else {
          "None available"
        }
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = paste("Please select X-axis channel.\nAvailable channels:", available_channels),
                        size = 4, color = "blue") +
               theme_void())
      }
      
              tryCatch({
        # Add debugging information
        message("=== STATIC PLOT DEBUG ===")
        message("- GatingSet exists: ", !is.null(values$current_gs))
        message("- Sample selected: ", input$sample_select)
        message("- X channel: ", input$x_channel)
        message("- Y channel: ", input$y_channel)
        message("- Parent population: ", input$parent_population)
        
        # Get current GatingHierarchy
        gh <- getCurrentGH()
        if (is.null(gh)) {
          message("- ERROR: No GatingHierarchy available")
          return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = "No GatingHierarchy available.\nCheck console for debugging info.",
                          size = 4, color = "red") +
                 theme_void())
        }
        
        message("- GatingHierarchy obtained successfully")
        
        # Get population data using GatingHierarchy - USE ALL DATA
        pop_data <- gh_pop_get_data(gh, input$parent_population)
        plot_data_full <- as.data.frame(exprs(pop_data))
        
        # Check if we have valid data
        if (nrow(plot_data_full) == 0) {
          return(ggplot() + 
                 annotate("text", x = 0.5, y = 0.5, 
                          label = paste("No events in population:", input$parent_population),
                          size = 6, color = "red") +
                 theme_void())
        }
        
        # Store full data for gating (NO subsampling)
        values$full_population_data <- plot_data_full
        
        # Create input values object for the static plot function
        input_vals <- list(
          plot_type = input$plot_type,
          color_channel = input$color_channel,
          color_scale = input$color_scale,
          alpha_level = input$alpha_level,
          point_size = input$point_size,
          density_adjust = if(!is.null(input$density_adjust)) input$density_adjust else 0.5
        )
        
        # Create static plot with ALL events and current gate visualization
        static_plot <- createStaticGatingPlot(
          plot_data_full = plot_data_full,
          x_channel = input$x_channel,
          y_channel = input$y_channel,
          sample_name = input$sample_select,
          parent_population = input$parent_population,
          input_vals = input_vals,
          existing_gates = NULL,  # Add existing gates here if needed
          current_gate_coords = values$gate_coords,
          current_gate_type = if(values$drawing_mode) input$gate_type else NULL
        )
        
        # Add drawing mode indicator when active but no points selected
        if (values$drawing_mode && (is.null(values$gate_coords) || nrow(values$gate_coords) == 0)) {
          static_plot <- static_plot + 
            labs(subtitle = paste("Ready to draw", input$gate_type, "gate - Click on plot to start"))
        }
        
        message("Static plot rendered with ", nrow(plot_data_full), " events")
        return(static_plot)
        
      }, error = function(e) {
        return(ggplot() + 
               annotate("text", x = 0.5, y = 0.5, 
                        label = paste("Error:", e$message),
                        size = 5, color = "red") +
               theme_void())
      })
    })
    
    # Interactive gating plot (plotly with subsampling)
    output$main_plot <- renderPlotly({
      req(values$current_gs, input$sample_select, input$x_channel)
      
      tryCatch({
        # Get sample data
        sample_idx <- which(sampleNames(values$current_gs) == input$sample_select)
        
        # Extract population data using GatingHierarchy
        tryCatch({
          # Get current GatingHierarchy
          gh <- getCurrentGH()
          if (is.null(gh)) {
            p <- ggplot() + 
              annotate("text", x = 0.5, y = 0.5, 
                       label = "No GatingHierarchy available",
                       size = 6, color = "red") +
              theme_void()
            return(ggplotly(p))
          }
          
          # Get population data using GatingHierarchy
          pop_data <- gh_pop_get_data(gh, input$parent_population)
          
          # Convert to data frame
          plot_data <- as.data.frame(exprs(pop_data))
          
          # Check if we have valid data
          if (nrow(plot_data) == 0) {
            # Return empty plot with message
            p <- ggplot() + 
              annotate("text", x = 0.5, y = 0.5, 
                       label = paste("No events in population:", input$parent_population),
                       size = 6, color = "red") +
              theme_void() +
              labs(title = paste("Sample:", input$sample_select, "| Parent:", input$parent_population))
            return(ggplotly(p))
          }
          
          # Store full data for gating (never subsample this)
          values$full_population_data <- plot_data
          
          # Subsample for performance in plotting only
          plot_data_for_display <- plot_data
          if (nrow(plot_data_for_display) > 10000) {
            set.seed(42)
            plot_data_for_display <- plot_data_for_display[sample(nrow(plot_data_for_display), 10000), ]
          }
          
          message("Population data extracted using GatingHierarchy: ", nrow(plot_data), " events")
          
        }, error = function(e) {
          # Return error plot
          p <- ggplot() + 
            annotate("text", x = 0.5, y = 0.5, 
                     label = paste("Error loading population data:", e$message),
                     size = 5, color = "red") +
            theme_void()
          return(ggplotly(p))
        })
        
        # Store both full data for gating and display data for plotting
        values$current_plot_data <- plot_data_for_display
        
        # Ensure coordinate consistency by setting explicit axis limits based on FULL data
        x_limits <- range(values$full_population_data[[input$x_channel]], na.rm = TRUE)
        y_limits <- if (!is.null(input$y_channel) && input$y_channel != "") {
          range(values$full_population_data[[input$y_channel]], na.rm = TRUE)
        } else {
          NULL
        }
        
        # Create base plot using display data but with full data limits
        if (!is.null(input$y_channel) && input$y_channel != "") {
          # 2D plot with enhanced color mapping
          
          # Determine color aesthetic
          if (!is.null(input$color_channel) && input$color_channel != "density" && 
              input$color_channel %in% names(plot_data_for_display)) {
            # Color by specific channel
            p <- ggplot(plot_data_for_display, aes(x = .data[[input$x_channel]], 
                                                   y = .data[[input$y_channel]], 
                                                   color = .data[[input$color_channel]]))
            color_legend_title <- input$color_channel
          } else {
            # Color by density or no specific channel
            p <- ggplot(plot_data_for_display, aes(x = .data[[input$x_channel]], 
                                                   y = .data[[input$y_channel]]))
            color_legend_title <- "Density"
          }
          
          p <- p + theme_minimal() +
            labs(title = paste("Sample:", input$sample_select, "| Parent:", input$parent_population, 
                              "| Display:", format(nrow(plot_data_for_display), big.mark = ","), "of", 
                              format(nrow(values$full_population_data), big.mark = ","), "events")) +
            # CRITICAL: Set limits to match full data range for coordinate consistency
            xlim(x_limits) +
            ylim(y_limits)
          
          # Add plot type specific geometry with flow cytometry colors
          if (input$plot_type == "scatter") {
            # Basic scatter plot with flow cytometry blue
            p <- p + geom_point(alpha = input$alpha_level, size = input$point_size, color = "#0066CC")
            
          } else if (input$plot_type == "scatter_colored") {
            if (!is.null(input$color_channel) && input$color_channel != "density" && 
                input$color_channel %in% names(plot_data_for_display)) {
              # Color by specific channel with flow cytometry colors
              color_data <- plot_data_for_display[[input$color_channel]]
              p <- p + geom_point(alpha = input$alpha_level, size = input$point_size) +
                getColorScale(input$color_scale, type = "color", name = color_legend_title, data_values = color_data)
            } else {
              # Point density coloring using ggpointdensity - hotspots where cells overlap
              p <- ggplot(plot_data_for_display, aes(x = .data[[input$x_channel]], 
                                                     y = .data[[input$y_channel]],
                                                     color = after_stat(ndensity))) +
                theme_minimal() +
                labs(title = paste("Sample:", input$sample_select, "| Parent:", input$parent_population, 
                                  "| Display:", format(nrow(plot_data_for_display), big.mark = ","), "of", 
                                  format(nrow(values$full_population_data), big.mark = ","), "events")) +
                xlim(x_limits) + ylim(y_limits) +
                geom_pointdensity(alpha = input$alpha_level, size = input$point_size, 
                                adjust = if(!is.null(input$density_adjust)) input$density_adjust else 0.5) +
                scale_color_gradientn(colors = getFlowCytometryColors(input$color_scale), 
                                    name = "Point Density")
            }
            
          } else if (input$plot_type == "contour") {
            if (!is.null(input$color_channel) && input$color_channel != "density" && 
                input$color_channel %in% names(plot_data_for_display)) {
              # Colored points with contour overlay using flow cytometry colors
              color_data <- plot_data_for_display[[input$color_channel]]
              p <- p + geom_point(alpha = input$alpha_level, size = input$point_size) +
                stat_density_2d(color = "white", size = 1.2, alpha = 0.8) +
                getColorScale(input$color_scale, type = "color", name = color_legend_title, data_values = color_data)
            } else {
              # Point density with contour overlay - hotspots with flow structure lines
              p <- ggplot(plot_data_for_display, aes(x = .data[[input$x_channel]], 
                                                     y = .data[[input$y_channel]],
                                                     color = after_stat(ndensity))) +
                theme_minimal() +
                labs(title = paste("Sample:", input$sample_select, "| Parent:", input$parent_population, 
                                  "| Display:", format(nrow(plot_data_for_display), big.mark = ","), "of", 
                                  format(nrow(values$full_population_data), big.mark = ","), "events")) +
                xlim(x_limits) + ylim(y_limits) +
                geom_pointdensity(alpha = input$alpha_level * 0.8, size = input$point_size, 
                                adjust = if(!is.null(input$density_adjust)) input$density_adjust else 0.5) +
                stat_density_2d(color = "#FF6600", size = 1, alpha = 0.8) +  # Flow cytometry orange contour lines
                scale_color_gradientn(colors = getFlowCytometryColors(input$color_scale), 
                                    name = "Point Density")
            }
            
          } else if (input$plot_type == "hex") {
            p <- p + geom_hex(alpha = input$alpha_level) +
              getColorScale(input$color_scale, type = "fill", name = "Count", data_values = NULL, force_discrete = FALSE)
          }
          
        } else {
          # Enhanced 1D histogram with flow cytometry colors
          if (!is.null(input$color_channel) && input$color_channel != "density" && 
              input$color_channel %in% names(plot_data_for_display)) {
            # Color histogram by channel values using flow cytometry colors
            color_data <- plot_data_for_display[[input$color_channel]]
            p <- ggplot(plot_data_for_display, aes(x = .data[[input$x_channel]], 
                                                   fill = .data[[input$color_channel]])) +
              geom_histogram(bins = 50, alpha = input$alpha_level, position = "identity") +
              getColorScale(input$color_scale, type = "fill", name = input$color_channel, data_values = color_data)
          } else {
            # Standard single-color histogram with flow cytometry colors
            flow_colors <- getFlowCytometryColors(input$color_scale)
            color_value <- flow_colors[3]  # Use mid-range color from flow cytometry palette
            
            p <- ggplot(plot_data_for_display, aes(x = .data[[input$x_channel]])) +
              geom_histogram(bins = 50, alpha = input$alpha_level, fill = color_value)
          }
          
          p <- p + theme_minimal() +
            labs(title = paste("Sample:", input$sample_select, "| Parent:", input$parent_population,
                              "| Display:", format(nrow(plot_data_for_display), big.mark = ","), "of", 
                              format(nrow(values$full_population_data), big.mark = ","), "events"),
                 y = "Count") +
            # CRITICAL: Set x limits to match full data range
            xlim(x_limits)
        }
        
        # Add existing gates if any
        if (!is.null(values$current_gs)) {
          p <- addExistingGates(p, values$current_gs, sample_idx, input$parent_population, 
                                input$x_channel, input$y_channel)
        }
        
        # Get plot dimensions with validation
        plot_width <- if (!is.null(input$plot_width)) {
          max(300, min(1200, input$plot_width))  # Clamp between 300-1200
        } else {
          600
        }
        plot_height <- if (!is.null(input$plot_height)) {
          max(300, min(1200, input$plot_height))  # Clamp between 300-1200
        } else {
          500
        }
        lock_aspect <- if (!is.null(input$lock_aspect)) input$lock_aspect else FALSE
        
        # Convert to plotly with proper configuration for interactive gating
        p_plotly <- ggplotly(p, source = "gating_plot") %>%
          config(
            displayModeBar = TRUE,
            modeBarButtonsToAdd = list('select2d', 'lasso2d'),
            displaylogo = FALSE,
            showTips = TRUE
          ) %>%
          layout(
            dragmode = "select",  # Enable selection by default
            selectdirection = "any",
            hovermode = "closest",
            width = plot_width,
            height = plot_height,
            # Handle aspect ratio for square plots
            xaxis = if (lock_aspect) {
              list(
                scaleanchor = "y",
                scaleratio = 1,
                constraintoward = "center"
              )
            } else {
              list()
            },
            # Add margin to prevent label cutoff
            margin = list(l = 60, r = 40, t = 80, b = 60)
          )
        
        # Events are automatically available when using source ID in ggplotly()
        # No explicit registration needed - events captured by event_data() functions
        
        return(p_plotly)
        
      }, error = function(e) {
        # Return empty plot on error
        p <- ggplot() + 
          annotate("text", x = 0.5, y = 0.5, label = paste("Error:", e$message)) +
          theme_void()
        return(ggplotly(p))
      })
    })
    
    # Start gating process
    observeEvent(input$start_gating, {
      req(input$population_name, input$gate_type)
      
      if (input$population_name == "") {
        showNotification("Please enter a population name first", type = "warning")
        return()
      }
      
      # Clear any previous gate coordinates
      values$gate_coords <- NULL
      values$selection_type <- NULL
      
      values$drawing_mode <- TRUE
      
      # Update instructions based on gate type and plot mode
      if (input$plot_mode == "static") {
        instructions <- switch(input$gate_type,
          "polygon" = "POLYGON: Click individual points on plot. Need 3+ points.",
          "rectangle" = "RECTANGLE: Use brush selection or click two diagonal corners.", 
          "ellipse" = "ELLIPSE: Click center and edge points. Need 3+ points.",
          "interval" = "INTERVAL: Click two points to define range on selected axis.",
          "threshold" = "THRESHOLD: Click to set minimum threshold value.",
          "boundary" = "BOUNDARY: Click to set maximum boundary value.",
          "quadrant" = "QUADRANT: Click to set crosshair position for 4 quadrants.",
          "web" = "WEB: Click multiple points for complex gate construction."
        )
      } else {
        instructions <- switch(input$gate_type,
          "polygon" = "POLYGON: Click individual points or use box/lasso select from toolbar. Need 3+ points.",
          "rectangle" = "RECTANGLE: Click two diagonal corners or use box select tool from toolbar.", 
          "ellipse" = "ELLIPSE: Click center and edge points or use box/lasso selection. Need 3+ points.",
          "interval" = "INTERVAL: Click two points to define range on selected axis.",
          "threshold" = "THRESHOLD: Click to set minimum threshold value.",
          "boundary" = "BOUNDARY: Click to set maximum boundary value.",
          "quadrant" = "QUADRANT: Click to set crosshair position for 4 quadrants.",
          "web" = "WEB: Click multiple points for complex gate construction."
        )
      }
      
      values$instruction_message <- instructions
      showNotification("Gating mode activated. Follow the plot instructions.", type = "message")
    })
    
    # Handle static plot brush selection for gate drawing
    observeEvent(input$plot_brush, {
      req(values$drawing_mode, input$plot_mode == "static")
      
      tryCatch({
        brush_data <- input$plot_brush
        if (!is.null(brush_data)) {
          # Convert brush coordinates to our format
          brush_coords <- data.frame(
            x = c(brush_data$xmin, brush_data$xmax, brush_data$xmax, brush_data$xmin),
            y = c(brush_data$ymin, brush_data$ymin, brush_data$ymax, brush_data$ymax)
          )
          
          values$gate_coords <- brush_coords
          values$selection_type <- "brush"
          
          # Show coordinate ranges for user feedback
          x_range <- c(brush_data$xmin, brush_data$xmax)
          y_range <- c(brush_data$ymin, brush_data$ymax)
          coord_info <- paste("X range:", round(x_range[1], 2), "to", round(x_range[2], 2),
                              "| Y range:", round(y_range[1], 2), "to", round(y_range[2], 2))
          
          showNotification(paste("Brush selection captured.", coord_info, "Click 'Save Gate' to apply."), 
                           type = "message", duration = 5)
          
          # Update instruction message to show brush captured
          values$instruction_message <- "Brush selection captured! Click 'Save Gate' to apply or continue adding points."
        }
      }, error = function(e) {
        # Silently handle brush errors
        message("Brush selection error: ", e$message)
      })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # Handle static plot click events for gate drawing
    observeEvent(input$plot_click, {
      req(values$drawing_mode, input$plot_mode == "static")
      
      tryCatch({
        click_data <- input$plot_click
        if (!is.null(click_data) && !is.null(click_data$x) && !is.null(click_data$y)) {
          # Store click coordinates for manual gate construction
          if (is.null(values$gate_coords)) {
            values$gate_coords <- data.frame(
              x = click_data$x,
              y = click_data$y
            )
          } else {
            values$gate_coords <- rbind(values$gate_coords, 
                                        data.frame(x = click_data$x, y = click_data$y))
          }
          values$selection_type <- "click"
          
          n_points <- nrow(values$gate_coords)
          required_points <- switch(input$gate_type,
            "rectangle" = 2,
            "polygon" = 3,
            "ellipse" = 3,
            "interval" = 2,
            "threshold" = 1,
            "boundary" = 1,
            "quadrant" = 1,
            2
          )
          
          # Show coordinate information for user feedback
          coord_info <- paste("(", round(click_data$x, 2), ",", round(click_data$y, 2), ")")
          
          if (n_points >= required_points) {
            showNotification(paste("Point", n_points, "captured", coord_info, "- Ready to save gate!"), 
                             type = "message")
            values$instruction_message <- paste(input$gate_type, "gate ready! Click 'Save Gate' to apply.")
          } else {
            showNotification(paste("Point", n_points, "captured", coord_info, "- Need", required_points - n_points, "more points."), 
                             type = "message")
            values$instruction_message <- paste("Continue clicking to add points for", input$gate_type, "gate. Need", required_points - n_points, "more.")
          }
        }
      }, error = function(e) {
        # Silently handle click errors
        message("Click selection error: ", e$message)
      })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # Handle plotly selection for gate drawing (interactive mode only)
    observeEvent(event_data("plotly_selected", source = "gating_plot"), {
      req(values$drawing_mode, input$plot_mode == "interactive")
      
      tryCatch({
        selection <- event_data("plotly_selected", source = "gating_plot")
        if (!is.null(selection) && is.data.frame(selection) && nrow(selection) > 0) {
          values$gate_coords <- selection
          values$selection_type <- "selected"
          
          # Show coordinate ranges for user feedback
          if ("x" %in% names(selection)) {
            x_range <- range(selection$x, na.rm = TRUE)
            coord_info <- paste("X range:", round(x_range[1], 2), "to", round(x_range[2], 2))
            
            if ("y" %in% names(selection)) {
              y_range <- range(selection$y, na.rm = TRUE)
              coord_info <- paste0(coord_info, " | Y range:", round(y_range[1], 2), "to", round(y_range[2], 2))
            }
            
            # Enhanced feedback for hierarchical gating
            hierarchical_info <- ""
            if (!is.null(input$parent_population) && input$parent_population != "root") {
              # Get parent population data ranges for comparison
              tryCatch({
                sample_idx <- which(sampleNames(values$current_gs) == input$sample_select)
                parent_data <- gs_pop_get_data(values$current_gs[sample_idx], input$parent_population)[[1]]
                parent_df <- as.data.frame(exprs(parent_data))
                
                parent_x_range <- range(parent_df[[input$x_channel]], na.rm = TRUE)
                parent_y_range <- if (!is.null(input$y_channel) && input$y_channel != "") {
                  range(parent_df[[input$y_channel]], na.rm = TRUE)
                } else {
                  c(0, 1)
                }
                
                # Check if selection is within parent range
                x_within_parent <- x_range[1] >= parent_x_range[1] && x_range[2] <= parent_x_range[2]
                y_within_parent <- if (!is.null(input$y_channel) && input$y_channel != "") {
                  y_range[1] >= parent_y_range[1] && y_range[2] <= parent_y_range[2]
                } else {
                  TRUE
                }
                
                if (x_within_parent && y_within_parent) {
                  hierarchical_info <- paste0(" ✓ Within ", input$parent_population, " range")
                } else {
                  hierarchical_info <- paste0(" ⚠️  May be outside ", input$parent_population, " range")
                }
                
              }, error = function(e) {
                hierarchical_info <- ""
              })
            }
            
            showNotification(paste("Captured", nrow(selection), "points via box/lasso selection.", 
                                   coord_info, hierarchical_info, "Click 'Save Gate' to apply."), 
                             type = "message")
            values$instruction_message <- "Selection captured! Click 'Save Gate' to apply or continue selecting."
          } else {
            showNotification(paste("Captured", nrow(selection), "points via box/lasso selection. Click 'Save Gate' to apply."), 
                             type = "message")
          }
        }
      }, error = function(e) {
        # Silently handle event data errors to prevent console spam
      })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # Handle individual point clicks for gate drawing (interactive mode only)
    observeEvent(event_data("plotly_click", source = "gating_plot"), {
      req(values$drawing_mode, input$plot_mode == "interactive")
      
      tryCatch({
        click_data <- event_data("plotly_click", source = "gating_plot")
        if (!is.null(click_data) && !is.null(click_data$x) && !is.null(click_data$y)) {
          # Store click coordinates for manual gate construction
          if (is.null(values$gate_coords)) {
            values$gate_coords <- data.frame(
              x = click_data$x,
              y = click_data$y
            )
          } else {
            values$gate_coords <- rbind(values$gate_coords, 
                                        data.frame(x = click_data$x, y = click_data$y))
          }
          values$selection_type <- "click"
          
          n_points <- nrow(values$gate_coords)
          required_points <- switch(input$gate_type,
            "rectangle" = 2,
            "polygon" = 3,
            "ellipse" = 3,
            "interval" = 2,
            "threshold" = 1,
            "boundary" = 1,
            "quadrant" = 1,
            2
          )
          
          # Show coordinate information for user feedback
          coord_info <- paste("(", round(click_data$x, 2), ",", round(click_data$y, 2), ")")
          
          # Enhanced feedback for hierarchical gating
          hierarchical_info <- ""
          if (!is.null(input$parent_population) && input$parent_population != "root") {
            tryCatch({
              sample_idx <- which(sampleNames(values$current_gs) == input$sample_select)
              parent_data <- gs_pop_get_data(values$current_gs[sample_idx], input$parent_population)[[1]]
              parent_df <- as.data.frame(exprs(parent_data))
              
              parent_x_range <- range(parent_df[[input$x_channel]], na.rm = TRUE)
              parent_y_range <- if (!is.null(input$y_channel) && input$y_channel != "") {
                range(parent_df[[input$y_channel]], na.rm = TRUE)
              } else {
                c(0, 1)
              }
              
              # Check if click point is within parent range
              x_within_parent <- click_data$x >= parent_x_range[1] && click_data$x <= parent_x_range[2]
              y_within_parent <- if (!is.null(input$y_channel) && input$y_channel != "") {
                click_data$y >= parent_y_range[1] && click_data$y <= parent_y_range[2]
              } else {
                TRUE
              }
              
              if (x_within_parent && y_within_parent) {
                hierarchical_info <- paste0(" ✓ Within ", input$parent_population)
              } else {
                hierarchical_info <- paste0(" ⚠️  Outside ", input$parent_population, " range")
              }
              
            }, error = function(e) {
              hierarchical_info <- ""
            })
          }
          
          if (n_points >= required_points) {
            showNotification(paste("Point", n_points, "captured", coord_info, hierarchical_info, "- Ready to save gate!"), 
                             type = "message")
          } else {
            showNotification(paste("Point", n_points, "captured", coord_info, hierarchical_info, "- Need", required_points - n_points, "more points."), 
                             type = "message")
          }
        }
      }, error = function(e) {
        # Silently handle event data errors to prevent console spam
      })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
    
    # Enhanced save gate function to handle multiple gates (like quadrants)
    observeEvent(input$save_gate, {
      req(values$drawing_mode, values$gate_coords, input$population_name)
      
      tryCatch({
        message("=== SAVE GATE DEBUG START ===")
        message("- Gate name: ", input$population_name)
        message("- Gate type: ", input$gate_type)
        message("- Parent population: ", input$parent_population)
        message("- X channel: ", input$x_channel)
        message("- Y channel: ", ifelse(is.null(input$y_channel), "NULL", input$y_channel))
        message("- Number of coordinate points: ", nrow(values$gate_coords))
        
        # Create gate(s) based on type and coordinates
        gate_result <- createGateFromCoords(values$gate_coords, input$gate_type, 
                                           input$x_channel, input$y_channel, 
                                           input$population_name)
        
        if (!is.null(gate_result)) {
          # Handle single gate vs multiple gates (quadrants)
          if (is.list(gate_result) && input$gate_type == "quadrant") {
            # Quadrant gates - add all four gates
            success_count <- 0
            for (gate_name in names(gate_result)) {
              tryCatch({
                message("- Adding quadrant gate: ", gate_name, " to parent: ", input$parent_population)
                gs_pop_add(values$current_gs, gate_result[[gate_name]], 
                           parent = input$parent_population,
                           name = gate_name)
                success_count <- success_count + 1
                message("  - Successfully added gate: ", gate_name)
              }, error = function(e) {
                message("  - ERROR adding gate ", gate_name, ": ", e$message)
                showNotification(paste("Warning: Could not add gate", gate_name, ":", e$message), 
                                 type = "warning")
              })
            }
            
            if (success_count > 0) {
              recompute(values$current_gs)
              message("- Recomputed GatingSet after adding ", success_count, " quadrant gates")
              
              # Check gate application results using GatingHierarchy
              gh_quad <- getCurrentGH()
              new_pop_counts <- if (!is.null(gh_quad)) getPopulationCounts(gh_quad) else NULL
              
              if (!is.null(new_pop_counts) && nrow(new_pop_counts) > 0) {
                message("- Population counts after recompute:")
                for (i in seq_len(nrow(new_pop_counts))) {
                  message("  - ", new_pop_counts$Population[i], ": ", new_pop_counts$Count[i], " events")
                }
              } else {
                message("- WARNING: Could not retrieve population counts after quadrant gate creation")
              }
              
              # Trigger hierarchy update
              values$hierarchy_trigger <- values$hierarchy_trigger + 1
              showNotification(paste("Successfully created", success_count, "quadrant gates!"), 
                               type = "message")
            }
            
          } else {
            # Single gate
            message("- Adding single gate: ", input$population_name, " to parent: ", input$parent_population)
            
            # Get pre-gate population counts using GatingHierarchy
            gh <- getCurrentGH()
            pre_pop_counts <- if (!is.null(gh)) getPopulationCounts(gh) else NULL
            
            message("- Population counts BEFORE adding gate:")
            if (!is.null(pre_pop_counts) && nrow(pre_pop_counts) > 0) {
              for (i in seq_len(nrow(pre_pop_counts))) {
                message("  - ", pre_pop_counts$Population[i], ": ", pre_pop_counts$Count[i], " events")
              }
            } else {
              message("  - ERROR: Could not retrieve pre-gate population counts using GatingHierarchy")
            }
            
            # Add the gate with proper error handling
            tryCatch({
              gs_pop_add(values$current_gs, gate_result, 
                         parent = input$parent_population,
                         name = input$population_name)
              message("  - Gate added to GatingSet successfully")
            }, error = function(e) {
              stop(paste("Failed to add gate to GatingSet:", e$message))
            })
            
            # Recompute to apply the gate with error handling
            tryCatch({
              recompute(values$current_gs)
              message("  - GatingSet recomputed successfully")
            }, error = function(e) {
              stop(paste("Failed to recompute GatingSet:", e$message))
            })
            
            # Pause briefly to allow recomputation to complete
            Sys.sleep(0.1)
            
            # Get post-gate population counts using GatingHierarchy
            # Force recompute first to ensure counts are updated
            recompute(values$current_gs)
            
            # Small delay to ensure recomputation completes
            Sys.sleep(0.1)
            
            # Get updated GatingHierarchy and counts
            gh_updated <- getCurrentGH()
            post_pop_counts <- if (!is.null(gh_updated)) getPopulationCounts(gh_updated) else NULL
            
            if (is.null(post_pop_counts)) {
              showNotification("Warning: Could not retrieve population counts after gating", type = "warning")
              return()
            }
            
            message("- Population counts AFTER adding and recomputing gate:")
            for (i in seq_len(nrow(post_pop_counts))) {
              message("  - ", post_pop_counts$Population[i], ": ", post_pop_counts$Count[i], " events")
            }
            
            # Find the new gate with improved logic to handle various path formats
            # Create expected population paths (multiple formats)
            expected_paths <- if (input$parent_population == "root" || input$parent_population == "/" || input$parent_population == "root/") {
              c(
                input$population_name,                    # "Test"
                paste0("/", input$population_name),       # "/Test"
                paste0("root/", input$population_name),   # "root/Test"
                paste0("/root/", input$population_name)   # "/root/Test"
              )
            } else {
              c(
                paste(input$parent_population, input$population_name, sep = "/"),
                paste0("/", input$parent_population, "/", input$population_name),
                input$population_name
              )
            }
            
            message("- Looking for new population with any of these paths: ", paste(expected_paths, collapse = ", "))
            
            # Try multiple search strategies with enhanced path matching
            new_gate_row <- NULL
            
            # Strategy 1: Exact match against any expected path
            for (path in expected_paths) {
              new_gate_row <- which(post_pop_counts$Population == path)
              if (length(new_gate_row) > 0) {
                message("- Found exact match for path: ", path)
                break
              }
            }
            
            # Strategy 2: Pattern matching - ends with population name
            if (length(new_gate_row) == 0) {
              new_gate_row <- which(grepl(paste0(input$population_name, "$"), post_pop_counts$Population))
              if (length(new_gate_row) > 0) {
                message("- Found pattern match ending with: ", input$population_name)
              }
            }
            
            # Strategy 3: Contains population name
            if (length(new_gate_row) == 0) {
              new_gate_row <- which(grepl(input$population_name, post_pop_counts$Population))
              if (length(new_gate_row) > 0) {
                message("- Found partial match containing: ", input$population_name)
              }
            }
            
            if (length(new_gate_row) > 0) {
              new_gate_count <- post_pop_counts$Count[new_gate_row[1]]  # Take first match
              actual_pop_name <- post_pop_counts$Population[new_gate_row[1]]
              message("- NEW GATE FOUND: ", actual_pop_name, " captured ", new_gate_count, " events")
              
              if (is.na(new_gate_count) || new_gate_count == 0) {
                warning_msg <- paste("Gate", input$population_name, "captured 0 events!",
                                     "Check if gate coordinates are within the parent population range.")
                message("- WARNING: ", warning_msg)
                showNotification(warning_msg, type = "warning")
              } else {
                success_msg <- paste("Gate", input$population_name, "captured", format(new_gate_count, big.mark = ","), "events")
                message("- SUCCESS: ", success_msg)
                showNotification(success_msg, type = "message")
              }
            } else {
              error_msg <- paste("ERROR: Could not find gate", input$population_name, "in population counts!")
              message("- ", error_msg)
              message("- Expected paths: ", paste(expected_paths, collapse = ", "))
              message("- Available populations:", paste(post_pop_counts$Population, collapse = ", "))
              showNotification(error_msg, type = "error")
            }
            
            # Force complete refresh of the gating set with comprehensive updates
            tryCatch({
              # Force a complete recomputation
              recompute(values$current_gs)
              message("- Forced complete recomputation after gate creation")
              
              # Verify final population counts using GatingHierarchy
              final_gh <- getCurrentGH()
              final_counts <- if (!is.null(final_gh)) getPopulationCounts(final_gh) else NULL
              
              if (!is.null(final_counts) && nrow(final_counts) > 0) {
                message("- FINAL POPULATION COUNTS VERIFICATION:")
                for (i in seq_len(nrow(final_counts))) {
                  message("  - ", final_counts$Population[i], ": ", format(final_counts$Count[i], big.mark = ","), " events")
                }
              } else {
                message("- WARNING: Could not verify final population counts")
              }
              
              # Trigger multiple hierarchy updates 
              values$hierarchy_trigger <- values$hierarchy_trigger + 1
              
              # Schedule hierarchy display update with delay to ensure completion
              shiny::invalidateLater(200, session = session)
              
              showNotification(paste("Gate", input$population_name, "saved successfully!"), 
                               type = "message")
              
            }, error = function(e) {
              message("Error in post-gate refresh: ", e$message)
              showNotification(paste("Gate saved but may have display issues:", e$message), 
                               type = "warning")
            })
          }
          
          # Reset gating mode
          values$drawing_mode <- FALSE
          values$gate_coords <- NULL
          values$selection_type <- NULL
          values$instruction_message <- "Select gate type and click 'Start Gating' to begin"
          
          # Clear population name for next gate
          updateTextInput(session, "population_name", value = "")
          
          # Force update of all reactive elements
          values$hierarchy_trigger <- values$hierarchy_trigger + 1
          
        } else {
          message("- ERROR: Gate creation returned NULL")
          showNotification("Could not create gate - check coordinates and gate type", 
                           type = "warning")
        }
        
        message("=== SAVE GATE DEBUG END ===")
        
      }, error = function(e) {
        error_msg <- paste("Error saving gate:", e$message)
        message("- CRITICAL ERROR: ", error_msg)
        showNotification(error_msg, type = "error")
      })
    })
    
    # Cancel gating
    observeEvent(input$cancel_gate, {
      values$drawing_mode <- FALSE
      values$gate_coords <- NULL
      values$selection_type <- NULL
      values$instruction_message <- "Select gate type and click 'Start Gating' to begin"
      showNotification("Gating cancelled", type = "message")
    })
    
    # Manual refresh hierarchy using GatingHierarchy
    observeEvent(input$refresh_hierarchy, {
      req(values$current_gs, input$sample_select)
      
      tryCatch({
        message("=== MANUAL HIERARCHY REFRESH START ===")
        
        # Force complete recomputation
        recompute(values$current_gs)
        message("- Forced recomputation completed")
        
        # Get current population counts for debugging
        gh <- getCurrentGH()
        if (!is.null(gh)) {
          refresh_counts <- getPopulationCounts(gh)
          
          if (!is.null(refresh_counts) && nrow(refresh_counts) > 0) {
            message("- Current population counts after manual refresh:")
            for (i in seq_len(nrow(refresh_counts))) {
              message("  - ", refresh_counts$Population[i], ": ", format(refresh_counts$Count[i], big.mark = ","), " events")
            }
          } else {
            message("- WARNING: Could not retrieve population counts during manual refresh")
          }
        } else {
          message("- WARNING: No GatingHierarchy available for sample: ", input$sample_select)
        }
        
        # Trigger all reactive updates
        values$hierarchy_trigger <- values$hierarchy_trigger + 1
        
        # Update hierarchy display
        updateHierarchyDisplay()
        
        # Force update of all UI elements
        shiny::invalidateLater(100, session)
        
        showNotification("Hierarchy display refreshed successfully!", type = "message")
        
        message("=== MANUAL HIERARCHY REFRESH END ===")
        
      }, error = function(e) {
        error_msg <- paste("Error refreshing hierarchy:", e$message)
        message("- ERROR: ", error_msg)
        showNotification(error_msg, type = "error")
      })
    })
    
    # Enhanced helper function to create gates from coordinates using advanced helpers
    createGateFromCoords <- function(coords, gate_type, x_channel, y_channel, pop_name) {
      tryCatch({
        # Validate inputs
        if (is.null(coords) || nrow(coords) == 0) {
          stop("No coordinates provided for gate creation")
        }
        
        if (is.null(values$full_population_data)) {
          stop("No population data available for gating")
        }
        
        # Validate that coordinates are within data range for better gating
        data_x_range <- range(values$full_population_data[[x_channel]], na.rm = TRUE)
        data_y_range <- if (!is.null(y_channel) && y_channel != "") {
          range(values$full_population_data[[y_channel]], na.rm = TRUE)
        } else {
          c(0, 1)
        }
        
        # Debug information
        coord_x_range <- range(coords$x, na.rm = TRUE)
        coord_y_range <- if ("y" %in% names(coords)) range(coords$y, na.rm = TRUE) else c(0, 1)
        
        message("Gate creation debug:")
        message("- Gate type: ", gate_type)
        message("- Parent population: ", input$parent_population)
        message("- Population data size: ", nrow(values$full_population_data), " events")
        message("- Coordinate X range: ", paste(round(coord_x_range, 2), collapse = " to "))
        message("- Data X range: ", paste(round(data_x_range, 2), collapse = " to "))
        if (!is.null(y_channel) && y_channel != "") {
          message("- Coordinate Y range: ", paste(round(coord_y_range, 2), collapse = " to "))
          message("- Data Y range: ", paste(round(data_y_range, 2), collapse = " to "))
        }
        
        # CRITICAL FIX: For hierarchical gating, validate coordinates against actual population data
        if (input$parent_population != "root") {
          # Get the actual data range from the parent population
          sample_idx <- which(sampleNames(values$current_gs) == input$sample_select)
          parent_data <- gs_pop_get_data(values$current_gs[sample_idx], input$parent_population)[[1]]
          parent_df <- as.data.frame(exprs(parent_data))
          
          actual_x_range <- range(parent_df[[x_channel]], na.rm = TRUE)
          actual_y_range <- if (!is.null(y_channel) && y_channel != "") {
            range(parent_df[[y_channel]], na.rm = TRUE)
          } else {
            c(0, 1)
          }
          
          message("- HIERARCHICAL GATING CHECK:")
          message("  - Parent population: ", input$parent_population, " (", nrow(parent_df), " events)")
          message("  - Actual parent X range: ", paste(round(actual_x_range, 2), collapse = " to "))
          if (!is.null(y_channel) && y_channel != "") {
            message("  - Actual parent Y range: ", paste(round(actual_y_range, 2), collapse = " to "))
          }
          
          # Check if coordinates are within parent population range
          x_within_parent <- coord_x_range[1] >= actual_x_range[1] && coord_x_range[2] <= actual_x_range[2]
          y_within_parent <- if (!is.null(y_channel) && y_channel != "") {
            coord_y_range[1] >= actual_y_range[1] && coord_y_range[2] <= actual_y_range[2]
          } else {
            TRUE
          }
          
          if (!x_within_parent || !y_within_parent) {
            warning_msg <- paste("Warning: Gate coordinates may be outside parent population range.",
                                 "This may result in 0 events captured.")
            message("  - COORDINATE WARNING: ", warning_msg)
            showNotification(warning_msg, type = "warning")
          } else {
            message("  - COORDINATE VALIDATION: Coordinates are within parent population range")
          }
        }
        
        # Check if coordinates are reasonable (within or close to data range)
        x_within_range <- coord_x_range[1] >= (data_x_range[1] - abs(data_x_range[1]) * 0.1) && 
                          coord_x_range[2] <= (data_x_range[2] + abs(data_x_range[2]) * 0.1)
        
        if (!x_within_range) {
          warning("X coordinates may be outside expected data range. Gate may capture 0 events.")
          showNotification(paste("Warning: X coordinates", paste(round(coord_x_range, 2), collapse = "-"), 
                                 "may be outside data range", paste(round(data_x_range, 2), collapse = "-")), 
                           type = "warning")
        }
        
        # Use advanced helper functions based on gate type
        gate_result <- switch(gate_type,
          "rectangle" = {
            if (nrow(coords) >= 2) {
              createRectangleGate(coords, x_channel, y_channel, pop_name)
            } else {
              stop("Rectangle gate requires at least 2 points")
            }
          },
          "polygon" = {
            if (nrow(coords) >= 3) {
              createPolygonGate(coords, x_channel, y_channel, pop_name)
            } else {
              stop("Polygon gate requires at least 3 points")
            }
          },
          "ellipse" = {
            if (nrow(coords) >= 3) {
              createEllipseGate(coords, x_channel, y_channel, pop_name)
            } else {
              stop("Ellipse gate requires at least 3 points")
            }
          },
          "interval" = {
            if (nrow(coords) >= 2) {
              createIntervalGate(coords, x_channel, pop_name)
            } else {
              stop("Interval gate requires at least 2 points")
            }
          },
          "threshold" = {
            if (nrow(coords) >= 1) {
              createThresholdGate(coords, x_channel, pop_name, direction = "above")
            } else {
              stop("Threshold gate requires at least 1 point")
            }
          },
          "boundary" = {
            if (nrow(coords) >= 1) {
              createBoundaryGate(coords, x_channel, pop_name)
            } else {
              stop("Boundary gate requires at least 1 point")
            }
          },
          "quadrant" = {
            if (nrow(coords) >= 1) {
              createQuadrantGates(coords, x_channel, y_channel, pop_name)
            } else {
              stop("Quadrant gate requires at least 1 point")
            }
          },
          "web" = {
            showNotification("Web gating functionality coming soon!", type = "message")
            return(NULL)
          },
          {
            stop(paste("Unknown gate type:", gate_type))
          }
        )
        
        # Validate that gate was created successfully
        if (is.null(gate_result)) {
          stop("Gate creation returned NULL - check coordinates and parameters")
        }
        
        return(gate_result)
        
      }, error = function(e) {
        error_msg <- paste("Error creating", gate_type, "gate:", e$message)
        showNotification(error_msg, type = "error")
        message("Gate creation error details: ", error_msg)
        return(NULL)
      })
    }
    

    
    # Update hierarchy display using GatingHierarchy
    updateHierarchyDisplay <- function() {
      if (!is.null(values$current_gs) && !is.null(input$sample_select)) {
        tryCatch({
          # Get current GatingHierarchy
          gh <- getCurrentGH()
          if (is.null(gh)) {
            updateSelectInput(session, "parent_population", choices = list("root" = "root"))
            updateSelectInput(session, "extract_population", choices = NULL)
            return()
          }
          
          # Get population counts using simplified approach
          pop_counts <- getPopulationCounts(gh)
          
          if (is.null(pop_counts) || nrow(pop_counts) == 0) {
            updateSelectInput(session, "parent_population", choices = list("root" = "root"))
            updateSelectInput(session, "extract_population", choices = NULL)
            return()
          }
          
          # Handle NA values in counts
          pop_counts$Count[is.na(pop_counts$Count)] <- 0
          safe_counts <- pop_counts$Count
          
          # Update parent population choices with counts
          parent_labels <- paste0(pop_counts$Population, " (", format(safe_counts, big.mark = ","), " events)")
          updateSelectInput(session, "parent_population",
                            choices = setNames(pop_counts$Population, parent_labels))
          
          # Update extract population choices (exclude root)
          extract_choices <- pop_counts$Population[pop_counts$Population != "root"]
          if (length(extract_choices) > 0) {
            extract_indices <- match(extract_choices, pop_counts$Population)
            extract_counts <- safe_counts[extract_indices]
            extract_labels <- paste0(extract_choices, " (", format(extract_counts, big.mark = ","), " events)")
            updateSelectInput(session, "extract_population",
                              choices = setNames(extract_choices, extract_labels),
                              selected = if(length(extract_choices) > 0) extract_choices[1] else NULL)
          } else {
            updateSelectInput(session, "extract_population", choices = NULL)
          }
          
          # Trigger reactive updates
          values$hierarchy_trigger <- values$hierarchy_trigger + 0.1
          
          message("Hierarchy display updated using GatingHierarchy")
          
        }, error = function(e) {
          message("Error updating hierarchy display with GatingHierarchy: ", e$message)
          showNotification(paste("Error updating hierarchy display:", e$message), type = "warning")
          # Set safe defaults on error
          updateSelectInput(session, "parent_population", choices = list("root" = "root"))
          updateSelectInput(session, "extract_population", choices = NULL)
        })
      }
    }
    
    # Hierarchy tree output using GatingHierarchy
    output$hierarchy_tree <- renderUI({
      req(values$current_gs, input$sample_select)
      # React to hierarchy changes
      values$hierarchy_trigger
      
      tryCatch({
        # Get current GatingHierarchy
        gh <- getCurrentGH()
        if (is.null(gh)) {
          return(HTML("<p style='color: orange;'>No GatingHierarchy available for selected sample</p>"))
        }
        
        # Get population counts using simplified approach
        pop_counts <- getPopulationCounts(gh)
        
        if (is.null(pop_counts) || nrow(pop_counts) == 0) {
          return(HTML("<p style='color: orange;'>No population hierarchy available</p>"))
        }
        
        # Debug log for population display
        message("=== HIERARCHY TREE DEBUG ===")
        message("Sample: ", input$sample_select)
        message("Population count rows: ", nrow(pop_counts))
        for (i in seq_len(min(5, nrow(pop_counts)))) {  # Show first 5 for brevity
          message("  - ", pop_counts$Population[i], ": ", pop_counts$Count[i])
        }
        message("=== END HIERARCHY TREE DEBUG ===")
        
        # Create hierarchical list with better formatting
        tree_html <- "<ul class='hierarchy-tree' style='list-style-type: none; padding-left: 0;'>"
        for (i in seq_len(nrow(pop_counts))) {
          # Safe count extraction
          count_val <- if (!is.na(pop_counts$Count[i])) {
            format(pop_counts$Count[i], big.mark = ",")
          } else {
            "0"
          }
          
          # Check if this is the root population
          is_root <- pop_counts$Population[i] == "root"
          
          if (is_root) {
            tree_html <- paste0(tree_html, 
                                "<li style='margin: 5px 0; font-weight: bold;'>", 
                                "<span style='color: #337ab7;'>root</span>", 
                                " <span style='color: #666;'>(", count_val, " events)</span></li>")
          } else {
            # Calculate indentation based on population path depth
            pop_name <- pop_counts$Population[i]
            level <- lengths(regmatches(pop_name, gregexpr("/", pop_name)))
            indent <- level * 20
            
            # Clean up population name for display
            display_name <- basename(pop_name)
            if (display_name == "") display_name <- pop_name
            
            tree_html <- paste0(tree_html,
                                "<li style='margin: 3px 0; margin-left: ", indent, "px;'>",
                                "<span style='color: #28a745;'>📊 ", display_name, "</span>", 
                                " <span style='color: #666;'>(", count_val, " events)</span></li>")
          }
        }
        tree_html <- paste0(tree_html, "</ul>")
        
        return(HTML(tree_html))
        
      }, error = function(e) {
        return(HTML("<p style='color: red;'>Error displaying hierarchy: ", e$message, "</p>"))
      })
    })
    
    # Population statistics using GatingHierarchy
    output$gate_statistics <- renderText({
      req(values$current_gs, input$sample_select)
      # React to hierarchy changes
      values$hierarchy_trigger
      
      tryCatch({
        # Get current GatingHierarchy
        gh <- getCurrentGH()
        if (is.null(gh)) {
          return("No GatingHierarchy available for selected sample")
        }
        
        # Get population counts using simplified approach
        pop_counts <- getPopulationCounts(gh)
        
        if (is.null(pop_counts) || nrow(pop_counts) == 0) {
          return("No population data available")
        }
        
        # Get root population count (should be reliable now)
        root_count <- pop_counts$Count[pop_counts$Population == "root"]
        
        if (length(root_count) == 0 || is.na(root_count) || root_count <= 0) {
          return(paste("Error: Invalid root population count"))
        }
        
        n_populations <- nrow(pop_counts) - 1  # excluding root
        
        stats_text <- paste0("Population Summary for Sample: ", input$sample_select, "\n\n")
        stats_text <- paste0(stats_text, "Total Events: ", format(root_count, big.mark = ","), "\n")
        stats_text <- paste0(stats_text, "Total Populations: ", n_populations, "\n\n")
        
        # Add detailed population stats
        for (i in seq_len(nrow(pop_counts))) {
          pop_name <- pop_counts$Population[i]
          pop_count <- pop_counts$Count[i]
          
          # Handle potential NA or invalid counts
          if (is.na(pop_count)) {
            pop_count <- 0
          }
          
          if (pop_name == "root") {
            stats_text <- paste0(stats_text, "🔹 root: ", format(pop_count, big.mark = ","), " events (100.0%)\n")
          } else {
            # Calculate percentage of root
            percentage <- round((pop_count / root_count) * 100, 2)
            display_name <- basename(pop_name)
            if (display_name == "") display_name <- pop_name
            stats_text <- paste0(stats_text, "📊 ", display_name, ": ", format(pop_count, big.mark = ","), " events (", percentage, "%)\n")
          }
        }
        
        return(stats_text)
        
      }, error = function(e) {
        return(paste("Error calculating statistics:", e$message))
      })
    })
    
    # Population table using GatingHierarchy
    output$population_table <- DT::renderDataTable({
      req(values$current_gs, input$sample_select)
      # React to hierarchy changes
      values$hierarchy_trigger
      
      tryCatch({
        # Get current GatingHierarchy
        gh <- getCurrentGH()
        if (is.null(gh)) {
          return(DT::datatable(data.frame(
            Message = "No GatingHierarchy available for selected sample",
            Status = "Please select a valid sample"
          ), options = list(dom = 't'), rownames = FALSE))
        }
        
        # Get population counts using simplified approach
        pop_counts <- getPopulationCounts(gh)
        
        if (is.null(pop_counts) || nrow(pop_counts) == 0) {
          return(DT::datatable(data.frame(
            Message = "No population data available",
            Status = "Please create gates first"
          ), options = list(dom = 't'), rownames = FALSE))
        }
        
        # Handle NA values in counts
        pop_counts$Count[is.na(pop_counts$Count)] <- 0
        
        # Get root count for percentage calculations
        root_count <- pop_counts$Count[pop_counts$Population == "root"]
        
        if (length(root_count) > 0 && root_count > 0) {
          pop_counts$Percentage <- round((pop_counts$Count / root_count) * 100, 2)
        } else {
          pop_counts$Percentage <- 0
        }
        
        # Add population level indicator (simpler with GatingHierarchy paths)
        pop_counts$Level <- sapply(pop_counts$Population, function(x) {
          if (is.na(x) || x == "root") return(0)
          tryCatch({
            return(lengths(regmatches(x, gregexpr("/", x))))
          }, error = function(e) return(0))
        })
        
        # Add cleaner display names
        pop_counts$DisplayName <- sapply(pop_counts$Population, function(x) {
          if (x == "root") return("root")
          display_name <- basename(x)
          if (display_name == "") return(x)
          return(display_name)
        })
        
        # Reorder columns for better display
        pop_counts <- pop_counts[, c("DisplayName", "Level", "Count", "Percentage")]
        colnames(pop_counts) <- c("Population", "Level", "Count", "Percentage")
        
        # Create base datatable
        dt <- DT::datatable(pop_counts, 
                            options = list(
                              pageLength = 15, 
                              scrollX = TRUE,
                              order = list(list(1, 'asc'))  # Sort by level
                            ),
                            rownames = FALSE)
        
        # Add color bar styling only if we have valid percentage range
        if (length(pop_counts$Percentage) > 0 && 
            !all(is.na(pop_counts$Percentage)) && 
            !all(pop_counts$Percentage == 0)) {
          
          # Calculate safe range for color bar
          pct_range <- range(pop_counts$Percentage, na.rm = TRUE)
          if (is.finite(pct_range[1]) && is.finite(pct_range[2]) && 
              pct_range[1] != pct_range[2]) {
            
            dt <- dt %>%
              DT::formatStyle("Percentage",
                              background = DT::styleColorBar(pct_range, "#f0f0f0"),
                              backgroundSize = "90% 90%",
                              backgroundRepeat = "no-repeat",
                              backgroundPosition = "center")
          }
        }
        
        return(dt)
        
      }, error = function(e) {
        DT::datatable(data.frame(Error = paste("No data available:", e$message)))
      })
    })
    
    # Apply gates to update data flow
    observeEvent(input$apply_gates, {
      req(values$current_gs)
      
      tryCatch({
        # Recompute all gates to ensure they're applied
        recompute(values$current_gs)
        
        # Store applied gating results in reactive values
        values$gated_data_applied <- TRUE
        values$applied_gates_timestamp <- Sys.time()
        
        # Trigger hierarchy update
        values$hierarchy_trigger <- values$hierarchy_trigger + 1
        
        # Update status
        total_pops <- length(gs_get_pop_paths(values$current_gs)) - 1  # excluding root
        showNotification(paste("Successfully applied", total_pops, "gate(s) to data!"), 
                         type = "message")
        
        updateHierarchyDisplay()
        
      }, error = function(e) {
        showNotification(paste("Error applying gates:", e$message), type = "error")
      })
    })
    
    # Extract gated data for specific population
    observeEvent(input$extract_gated_data, {
      req(values$current_gs, input$extract_population)
      
      tryCatch({
        # Validate population exists and has data using GatingHierarchy
        gh <- getCurrentGH()
        pop_counts <- if (!is.null(gh)) getPopulationCounts(gh) else NULL
        
        if (is.null(pop_counts)) {
          showNotification("Could not retrieve population counts for validation", type = "error")
          return()
        }
        
        pop_row <- which(pop_counts$Population == input$extract_population)
        
        if (length(pop_row) == 0) {
          showNotification(paste("Population '", input$extract_population, "' not found"), type = "error")
          return()
        }
        
        pop_cells <- pop_counts$Count[pop_row]
        if (is.na(pop_cells) || pop_cells <= 0) {
          showNotification(paste("Population '", input$extract_population, 
                                 "' has no events to extract"), type = "warning")
          return()
        }
        
        # Extract specific population data
        gated_flowset <- gs_pop_get_data(values$current_gs, input$extract_population)
        
        # Validate extracted data
        if (is.null(gated_flowset) || length(gated_flowset) == 0) {
          showNotification(paste("Failed to extract data for population:", input$extract_population), 
                           type = "error")
          return()
        }
        
        # Store extracted data
        values$extracted_population <- input$extract_population
        values$extracted_flowset <- gated_flowset
        values$extraction_timestamp <- Sys.time()
        
        # Get cell counts for feedback with safety checks
        total_cells <- sum(pop_counts$Count, na.rm = TRUE)
        if (total_cells > 0) {
          percentage <- round((pop_cells / total_cells) * 100, 2)
        } else {
          percentage <- 0
        }
        
        showNotification(paste("Extracted population '", input$extract_population, 
                               "' with", format(pop_cells, big.mark = ","), "cells (",
                               percentage, "% of total)"),
                         type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error extracting population:", e$message), type = "error")
      })
    })
    
    # Gating status display
    output$gating_status_text <- renderText({
      req(values$current_gs)
      # React to hierarchy changes
      values$hierarchy_trigger
      
      # Count gates and populations
      pop_paths <- gs_get_pop_paths(values$current_gs)
      n_gates <- length(pop_paths) - 1  # excluding root
      
      status_parts <- c()
      
      # Basic status
      status_parts <- c(status_parts, paste("Gates created:", n_gates))
      
      # Applied status
      if (!is.null(values$gated_data_applied) && values$gated_data_applied) {
        time_applied <- format(values$applied_gates_timestamp, "%H:%M:%S")
        status_parts <- c(status_parts, paste("Applied at:", time_applied))
      } else if (n_gates > 0) {
        status_parts <- c(status_parts, "Gates not yet applied")
      }
      
      # Extraction status
      if (!is.null(values$extracted_population)) {
        time_extracted <- format(values$extraction_timestamp, "%H:%M:%S")
        status_parts <- c(status_parts, 
                          paste("Extracted:", values$extracted_population, "at", time_extracted))
      }
      
      if (length(status_parts) == 0) {
        return("No gates created yet")
      } else {
        return(paste(status_parts, collapse=" | "))
      }
    })
    

    
    # Export gated FCS files
    output$export_gated_fcs <- downloadHandler(
      filename = function() {
        if (!is.null(values$extracted_population)) {
          paste0("gated_", gsub("[^A-Za-z0-9]", "_", values$extracted_population), "_", Sys.Date(), ".zip")
        } else {
          paste0("gated_populations_", Sys.Date(), ".zip")
        }
      },
      content = function(file) {
        req(values$current_gs)
        
        tryCatch({
          # Create temporary directory
          temp_dir <- tempdir()
          zip_dir <- file.path(temp_dir, "gated_fcs")
          if (dir.exists(zip_dir)) unlink(zip_dir, recursive = TRUE)
          dir.create(zip_dir, recursive = TRUE)
          
          # Determine which populations to export
          if (!is.null(values$extracted_population) && !is.null(values$extracted_flowset)) {
            # Export specific extracted population
            pop_name <- values$extracted_population
            flowset_to_export <- values$extracted_flowset
            
            # Export each sample in the flowset
            sample_names <- sampleNames(flowset_to_export)
            for (i in seq_along(sample_names)) {
              sample_name <- sample_names[i]
              clean_name <- gsub("[^A-Za-z0-9_-]", "_", sample_name)
              clean_pop <- gsub("[^A-Za-z0-9_-]", "_", pop_name)
              
              filename <- file.path(zip_dir, 
                                    paste0(clean_name, "_", clean_pop, ".fcs"))
              
              # Write FCS file
              write.FCS(flowset_to_export[[i]], filename = filename)
            }
            
            showNotification(paste("Exported", length(sample_names), "FCS files for population:", pop_name), 
                             type = "message")
            
          } else {
            # Export all populations
            pop_paths <- gs_get_pop_paths(values$current_gs)
            pop_paths <- pop_paths[pop_paths != "root"]  # exclude root
            
            exported_count <- 0
            for (pop in pop_paths) {
              tryCatch({
                pop_flowset <- gs_pop_get_data(values$current_gs, pop)
                sample_names <- sampleNames(pop_flowset)
                
                for (i in seq_along(sample_names)) {
                  sample_name <- sample_names[i]
                  clean_name <- gsub("[^A-Za-z0-9_-]", "_", sample_name)
                  clean_pop <- gsub("[^A-Za-z0-9_-]", "_", pop)
                  
                  filename <- file.path(zip_dir, 
                                        paste0(clean_name, "_", clean_pop, ".fcs"))
                  
                  write.FCS(pop_flowset[[i]], filename = filename)
                  exported_count <- exported_count + 1
                }
              }, error = function(e) {
                # Skip problematic populations
              })
            }
            
            showNotification(paste("Exported", exported_count, "FCS files for all populations"), 
                             type = "message")
          }
          
          # Create ZIP file
          fcs_files <- list.files(zip_dir, pattern = "\\.fcs$", full.names = TRUE)
          if (length(fcs_files) > 0) {
            zip(file, files = fcs_files, flags = "-r9X", extras = "-j")
          } else {
            # Create empty file with error message
            writeLines("No FCS files could be exported", file)
          }
          
          # Clean up
          unlink(zip_dir, recursive = TRUE)
          
        }, error = function(e) {
          showNotification(paste("Error exporting FCS files:", e$message), type = "error")
          # Create error file
          writeLines(paste("Export error:", e$message), file)
        })
      }
    )
    
    # Export gates
    output$export_gates <- downloadHandler(
      filename = function() {
        paste0("gates_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Export gate definitions
        if (!is.null(values$current_gs)) {
          tryCatch({
            pop_paths <- gs_get_pop_paths(values$current_gs, path = "auto")
            
            # Create comprehensive gating template
            gt <- data.frame(
              Population = pop_paths,
              Parent = c("root", rep("root", length(pop_paths) - 1)),  # simplified parent structure
              Channels = paste(input$x_channel, input$y_channel, sep = ","),
              GatingMethod = "manual",
              Created = Sys.time(),
              AppliedChannels = paste(input$x_channel, input$y_channel, sep = ","),
              stringsAsFactors = FALSE
            )
            
            # Add population counts if available using GatingHierarchy
            if (!is.null(values$current_gs) && !is.null(input$sample_select)) {
              gh <- getCurrentGH()
              pop_counts <- if (!is.null(gh)) getPopulationCounts(gh) else NULL
              if (!is.null(pop_counts)) {
                gt$CellCount <- pop_counts$Count[match(gt$Population, pop_counts$Population)]
              }
            }
            
            write.csv(gt, file, row.names = FALSE)
            showNotification("Gate definitions exported successfully", type = "message")
            
          }, error = function(e) {
            # Create simple export on error
            simple_gt <- data.frame(
              Population = gs_get_pop_paths(values$current_gs, path = "auto"),
              Method = "manual_gating",
              Export_Time = Sys.time()
            )
            write.csv(simple_gt, file, row.names = FALSE)
          })
        }
      }
    )
    
    # Export population statistics
    output$export_populations <- downloadHandler(
      filename = function() {
        paste0("population_stats_", Sys.Date(), ".csv")
      },
      content = function(file) {
        if (!is.null(values$current_gs) && !is.null(input$sample_select)) {
          tryCatch({
            # Get comprehensive population statistics using GatingHierarchy
            gh <- getCurrentGH()
            pop_counts <- if (!is.null(gh)) getPopulationCounts(gh) else NULL
            
            if (is.null(pop_counts)) {
              writeLines("No population data available for export", file)
              return()
            }
            
            # Add percentage calculations
            root_count <- pop_counts$Count[pop_counts$Population == "root"]
            
            if (length(root_count) > 0 && root_count > 0) {
              pop_counts$Percentage <- round((pop_counts$Count / root_count) * 100, 2)
            } else {
              pop_counts$Percentage <- 0
            }
            
            # Add export metadata
            pop_counts$ExportTime <- Sys.time()
            pop_counts$GatingApplied <- values$gated_data_applied
            pop_counts$Sample <- input$sample_select
            
            write.csv(pop_counts, file, row.names = FALSE)
            showNotification("Population statistics exported successfully", type = "message")
            
          }, error = function(e) {
            # Simple fallback export
            simple_data <- data.frame(
              Error = paste("Export failed:", e$message),
              Time = Sys.time()
            )
            write.csv(simple_data, file, row.names = FALSE)
          })
        }
      }
    )
    
    # Return reactive values for use by other modules
    return(reactive({
      list(
        gating_set = values$current_gs,
        current_data = values$current_data,
        available_channels = values$available_channels,
        # Gated data outputs
        gated_data_applied = values$gated_data_applied,
        extracted_population = values$extracted_population,
        extracted_flowset = values$extracted_flowset,
        applied_gates_timestamp = values$applied_gates_timestamp,
        extraction_timestamp = values$extraction_timestamp,
        # Helper function to get specific population data
        get_population_data = function(population_name = NULL) {
          if (!is.null(values$current_gs)) {
            if (is.null(population_name)) {
              # Return all population data
              return(gs_pop_get_data(values$current_gs))
            } else {
              # Return specific population data
              return(gs_pop_get_data(values$current_gs, population_name))
            }
          }
          return(NULL)
        },
        # Helper function to get population statistics using GatingHierarchy
        get_population_stats = function() {
          if (!is.null(values$current_gs) && !is.null(input$sample_select)) {
            tryCatch({
              gh <- getCurrentGH()
              pop_counts <- if (!is.null(gh)) getPopulationCounts(gh) else NULL
              
              if (!is.null(pop_counts)) {
                root_count <- pop_counts$Count[pop_counts$Population == "root"]
                if (length(root_count) > 0 && root_count > 0) {
                  pop_counts$Percentage <- round((pop_counts$Count / root_count) * 100, 2)
                }
              }
              return(pop_counts)
            }, error = function(e) {
              return(NULL)
            })
          }
          return(NULL)
        }
      )
    }))
  })
} 