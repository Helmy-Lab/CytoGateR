# Visualization Module for Flow Cytometry Analysis Tool

#' UI for the Visualization Module
#' @param id Module ID
#' @return UI elements for visualization
visualizationModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(4,
             # Color by control
             selectInput(ns("colorBy"), "Color Points By", choices = c("None"), selected = "None"),
             
             # Visualization options
             h4("Visualization Options"),
             numericInput(ns("plotWidth"), "Plot Width", value = 800, min = 400, max = 2000, step = 100),
             numericInput(ns("plotHeight"), "Plot Height", value = 600, min = 300, max = 1500, step = 100),
             
             # Point customization
             sliderInput(ns("pointSize"), "Point Size", min = 1, max = 10, value = 3, step = 0.5),
             sliderInput(ns("pointAlpha"), "Point Transparency", min = 0.1, max = 1.0, value = 0.7, step = 0.1),
             
             # Legend options
             checkboxInput(ns("showLegend"), "Show Legend", value = TRUE),
             conditionalPanel(
               condition = paste0("input['", ns("showLegend"), "'] === true"),
               selectInput(ns("legendPosition"), "Legend Position", 
                           choices = c("Right" = "right", "Left" = "left", 
                                       "Top" = "top", "Bottom" = "bottom"),
                           selected = "right")
             )
      ),
      column(8,
             # Plot preview
             shinycssloaders::withSpinner(plotOutput(ns("plotPreview"), height = "300px")),
             br(),
             actionButton(ns("updatePlot"), "Update Plot", class = "btn-info")
      )
    )
  )
}

#' Server function for the Visualization Module
#' 
#' @param id Module ID
#' @param plot_data Reactive expression containing data to visualize
#' @param clustering_results Reactive expression with clustering results
#' @param app_state Reactive values with app state
#' @param markers Reactive expression with marker names
#' @return List with visualization-related values
visualizationModuleServer <- function(id, plot_data, clustering_results = reactive(NULL), 
                                      app_state, markers = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    
    # Update color by dropdown when data or clustering changes
    observe({
      req(plot_data())
      
      # Get marker columns
      data <- plot_data()
      all_cols <- colnames(data)
      
      # Determine available columns for coloring
      color_choices <- c("None")
      
      # Add Cluster if clustering results exist
      if (!is.null(clustering_results())) {
        color_choices <- c(color_choices, "Cluster")
      }
      
      # Add marker columns if available
      if (!is.null(markers())) {
        marker_cols <- intersect(markers(), all_cols)
        if (length(marker_cols) > 0) {
          color_choices <- c(color_choices, marker_cols)
        }
      } else {
        # If no explicit markers provided, add all numeric columns
        numeric_cols <- all_cols[sapply(data, is.numeric)]
        if (length(numeric_cols) > 0) {
          color_choices <- c(color_choices, numeric_cols)
        }
      }
      
      # Update the dropdown
      updateSelectInput(session, "colorBy", 
                        choices = color_choices,
                        selected = if ("Cluster" %in% color_choices) "Cluster" else color_choices[1])
    })
    
    # Create preview plot
    createPlot <- reactive({
      req(plot_data())
      data <- plot_data()
      
      # Check for dimension columns (expected for tsne, umap, pca)
      has_dim1 <- "dim1" %in% colnames(data)
      has_dim2 <- "dim2" %in% colnames(data)
      has_tsne <- "tsne1" %in% colnames(data) && "tsne2" %in% colnames(data)
      has_umap <- "umap1" %in% colnames(data) && "umap2" %in% colnames(data)
      has_pca  <- "pca1" %in% colnames(data) && "pca2" %in% colnames(data)
      has_mds  <- "mds1" %/in% colnames(data) && "mds2" %in% colnames(data)
      
      # Determine x and y variables
      if (has_dim1 && has_dim2) {
        x_var <- "dim1"
        y_var <- "dim2"
        dim_method <- "Dimension Reduction"
      } else if (has_tsne) {
        x_var <- "tsne1"
        y_var <- "tsne2"
        dim_method <- "t-SNE"
      } else if (has_umap) {
        x_var <- "umap1"
        y_var <- "umap2"
        dim_method <- "UMAP"
      } else if (has_pca) {
        x_var <- "pca1"
        y_var <- "pca2"
        dim_method <- "PCA"
      } else if (has_mds) {
        x_var <- "mds1"
        y_var <- "mds2"
        dim_method <- "MDS"
      } else if (length(colnames(data)) >= 2) {
        # Fallback to first two columns
        x_var <- colnames(data)[1]
        y_var <- colnames(data)[2]
        dim_method <- "Scatter"
      } else {
        # Not enough variables
        return(NULL)
      }
      
      # Add cluster information if available
      if (!is.null(clustering_results()) && input$colorBy == "Cluster") {
        data$Cluster <- as.factor(clustering_results()$cluster_ids)
      }
      
      # Plot settings
      settings <- app_state$plot_settings
      point_size <- input$pointSize
      point_alpha <- input$pointAlpha
      show_legend <- input$showLegend
      legend_position <- input$legendPosition
      
      # Create plot based on coloring variable
      if (input$colorBy != "None" && input$colorBy %in% colnames(data)) {
        if (input$colorBy == "Cluster" || is.factor(data[[input$colorBy]])) {
          # Categorical coloring
          p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], color = .data[[input$colorBy]])) +
            geom_point(alpha = point_alpha, size = point_size)
          
          # Apply categorical color palette
          if (settings$color_palette == "viridis") {
            p <- p + scale_color_viridis_d()
          } else if (settings$color_palette == "plasma") {
            p <- p + scale_color_viridis_d(option = "plasma")
          } else if (settings$color_palette == "blues") {
            p <- p + scale_color_brewer(palette = "Blues")
          } else if (settings$color_palette == "reds") {
            p <- p + scale_color_brewer(palette = "Reds")
          } else if (settings$color_palette == "brewer_paired") {
            p <- p + scale_color_brewer(palette = "Paired")
          } else if (settings$color_palette == "brewer_brbg") {
            p <- p + scale_color_brewer(palette = "BrBG")
          }
        } else {
          # Continuous coloring
          p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]], color = .data[[input$colorBy]])) +
            geom_point(alpha = point_alpha, size = point_size)
          
          # Apply continuous color palette
          if (settings$color_palette == "viridis") {
            p <- p + scale_color_viridis_c()
          } else if (settings$color_palette == "plasma") {
            p <- p + scale_color_viridis_c(option = "plasma")
          } else if (settings$color_palette == "blues") {
            p <- p + scale_color_gradient(low = "lightblue", high = "darkblue")
          } else if (settings$color_palette == "reds") {
            p <- p + scale_color_gradient(low = "lightsalmon", high = "darkred")
          } else if (settings$color_palette == "brewer_paired") {
            # For continuous data with categorical palette, use viridis as fallback
            p <- p + scale_color_viridis_c()
          } else if (settings$color_palette == "brewer_brbg") {
            p <- p + scale_color_distiller(palette = "BrBG")
          }
        }
      } else {
        # No coloring
        p <- ggplot(data, aes(x = .data[[x_var]], y = .data[[y_var]])) +
          geom_point(alpha = point_alpha, size = point_size, color = "#3366CC")
      }
      
      # Add labels and theme
      p <- p + 
        labs(
          title = paste(dim_method, "Plot"),
          x = gsub("[0-9]$", "", x_var),  # Remove trailing numbers
          y = gsub("[0-9]$", "", y_var)   # Remove trailing numbers
        ) +
        get_standard_theme(settings$font_size)
      
      # Adjust legend display
      if (!show_legend) {
        p <- p + theme(legend.position = "none")
      } else {
        p <- p + theme(legend.position = legend_position)
      }
      
      return(p)
    })
    
    # Render the preview plot
    output$plotPreview <- renderPlot({
      req(createPlot())
      createPlot()
    })
    
    # Convert to plotly if needed
    createPlotly <- reactive({
      req(createPlot())
      
      # Get dimensions
      width <- input$plotWidth
      height <- input$plotHeight
      
      # Convert to plotly
      p_plotly <- ggplotly(createPlot(), width = width, height = height) %>%
        layout(
          hoverlabel = list(
            bgcolor = "white",
            font = list(family = "Arial", size = app_state$plot_settings$font_size)
          )
        )
      
      return(p_plotly)
    })
    
    # Return values that might be needed by other modules
    return(list(
      base_plot = createPlot,
      plotly_plot = createPlotly,
      plot_width = reactive(input$plotWidth),
      plot_height = reactive(input$plotHeight),
      color_by = reactive(input$colorBy)
    ))
  })
}