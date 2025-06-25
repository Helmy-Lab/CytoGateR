# Main application file
# Flow Cytometry Analysis Tool

# Source global.R first
source("global.R")

# Define UI
ui <- navbarPage(
  # Add theme and include custom CSS
  theme = shinytheme("flatly"),
  includeCSS("www/custom.css"),
  includeScript("www/custom.js"), # Add the custom JavaScript
  
  # Add a custom CSS style tag to set initial font sizes
  tags$head(
    tags$style(HTML("
      /* Initial font size variables */
      :root {
        --plotly-font-size: 16px;
        --plotly-title-size: 19px;
        --plotly-axis-size: 17px;
      }
      
      /* Plotly optimizations */
      .js-plotly-plot .plotly text {
        font-family: 'Arial', sans-serif;
      }
    "))
  ),
  
  # Application title
  title = "CytoVerse",
  
  # Tab for the Settings Module
  tabPanel("Settings", 
           settingsModuleUI("settings")
  ),

  # Tab for the Compensation Module
  tabPanel("Spillover Compensation", 
           compensationModuleUI("compensation")
  ),
  
  # Tab for the Gating Module
  tabPanel("Interactive Gating", 
           gatingModuleUI("gating")
  ),
    
  # Tab for the Raw Data Module
  tabPanel("Raw Data", 
           rawDataModuleUI("raw_data")
  ),

  # Tab for the Processed Data Module
  tabPanel("Processed Data", 
           processedDataModuleUI("processed_data")
  ),
  
  # Tab for the Batch Analysis Module
  tabPanel("Batch Analysis", 
           batchAnalysisModuleUI("batch_analysis")
  )
)

# Define server
server <- function(input, output, session) {
  # App state - shared between modules
  app_state <- reactiveValues(
    plot_settings = list(
      width = 800,
      height = 600,
      font_size = 16,
      point_size = 6,
      color_palette = "viridis"
    )
  )
  
  # Call module servers
  raw_data_results <- rawDataModuleServer("raw_data", app_state)
  compensation_results <- compensationModuleServer("compensation", app_state)
  gating_results <- gatingModuleServer("gating", app_state, raw_data_results)
  batch_results <- batchAnalysisModuleServer("batch_analysis", app_state)
  settings_results <- settingsModuleServer("settings", app_state)
  
  # Efficient observer to ensure plots render properly after settings changes
  observe({
    # Force reactivity when plot settings change
    settings <- app_state$plot_settings
    
    # Update CSS variables once (more efficient than individual DOM updates)
    session$sendCustomMessage(type = "updatePlotlyFonts", message = list(
      fontSize = settings$font_size
    ))
    
    # Define critical plots that need faster updates
    priorityPlots <- c(
      "raw_data-clusterPlot",
      "raw_data-tsnePlot",
      "raw_data-umapPlot"
    )
    
    # Only update essential plots immediately with batched parameters
    for (id in priorityPlots) {
      if (!is.null(id)) {
        session$sendCustomMessage(type = "plotly-replot", message = list(
          id = id,
          fontSize = settings$font_size
        ))
      }
    }
    
    # Trigger a general refresh for other plots
    session$sendCustomMessage(type = "refreshPlots", message = list())
  })
}

# Run the app
shinyApp(ui = ui, server = server)