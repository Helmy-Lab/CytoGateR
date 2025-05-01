# Settings Module for Flow Cytometry Analysis Tool

#' UI for the Settings Module
#' @param id Module ID
#' @return UI elements for settings module
settingsModuleUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    column(4,
           wellPanel(
             h4("Global Plot Settings", style = "text-align: center; font-weight: bold;"),
             
             sliderInput(ns("global_plot_width"), "Plot Width", 
                         min = 300, max = 1200, value = 800, step = 50),
             
             sliderInput(ns("global_plot_height"), "Plot Height", 
                         min = 300, max = 1200, value = 800, step = 50),
             
             sliderInput(ns("global_font_size"), "Base Font Size", 
                         min = 8, max = 36, value = 12, step = 1),
             
             sliderInput(ns("global_point_size"), "Point Size", 
                         min = 2, max = 12, value = 6, step = 1),
             
             radioButtons(ns("global_color_palette"), "Color Palette",
                          choices = c("Viridis" = "viridis", 
                                      "Plasma" = "plasma", 
                                      "Blues" = "blues",
                                      "Reds" = "reds"),
                          selected = "viridis"),
             
             actionButton(ns("apply_plot_settings"), "Apply Settings to All Plots", 
                          class = "btn-primary")
           )
    ),
    column(8,
           wellPanel(
             h4("Plot Preview", style = "text-align: center; font-weight: bold;"),
             # Plot preview will be added here
             plotOutput(ns("settings_preview"), height = "400px")
           )
    )
  )
}

#' Server function for the Settings Module
#' 
#' @param id Module ID
#' @param app_state Reactive values list containing shared app state
#' @return List of reactive values
settingsModuleServer <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    
    # Create a preview plot based on current settings
    output$settings_preview <- renderPlot({
      # Create sample data
      set.seed(123)
      n_points <- 500
      n_clusters <- 4
      
      sample_data <- data.frame(
        x = c(rnorm(n_points/4, -2, 0.5), 
              rnorm(n_points/4, 2, 0.5),
              rnorm(n_points/4, 0, 0.5),
              rnorm(n_points/4, 0, 2)),
        y = c(rnorm(n_points/4, -2, 0.5), 
              rnorm(n_points/4, 2, 0.5),
              rnorm(n_points/4, 0, 2),
              rnorm(n_points/4, 0, 0.5)),
        Cluster = as.factor(rep(1:n_clusters, each = n_points/4)),
        Marker = c(rnorm(n_points/4, 3, 1), 
                   rnorm(n_points/4, 5, 1),
                   rnorm(n_points/4, 2, 1),
                   rnorm(n_points/4, 4, 1))
      )
      
      # Get current settings
      font_size <- input$global_font_size
      point_size <- input$global_point_size
      palette <- input$global_color_palette
      
      # Create a demonstration plot
      p <- ggplot(sample_data, aes(x = x, y = y, color = Cluster)) +
        geom_point(size = point_size/2, alpha = 0.7) +
        labs(
          title = "Preview: Clustering Example",
          subtitle = paste0("Font Size: ", font_size, ", Point Size: ", point_size),
          x = "Dimension 1",
          y = "Dimension 2"
        ) +
        get_standard_theme(font_size = font_size)
      
      # Apply color palette
      if (palette == "viridis") {
        p <- p + scale_color_viridis_d()
      } else if (palette == "plasma") {
        p <- p + scale_color_viridis_d(option = "plasma")
      } else if (palette == "blues") {
        p <- p + scale_color_brewer(palette = "Blues")
      } else if (palette == "reds") {
        p <- p + scale_color_brewer(palette = "Reds")
      }
      
      return(p)
    })
    
    # Update global settings when apply button is clicked
    observeEvent(input$apply_plot_settings, {
      app_state$plot_settings <- list(
        width = input$global_plot_width,
        height = input$global_plot_height,
        font_size = input$global_font_size,
        point_size = input$global_point_size,
        color_palette = input$global_color_palette
      )
      
      showNotification("Plot settings applied to all visualizations", type = "message")
    })
    
    # Return values that might be needed by other modules
    return(reactive({
      list(
        plot_settings = app_state$plot_settings
      )
    }))
  })
}