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
  
  # Add a custom CSS style tag to override font-size controls
  tags$head(
    tags$style(HTML("
      /* Ensure plotly respects its internal font settings */
      .js-plotly-plot .plotly text {
        font-family: 'Arial', sans-serif !important;
      }
      
      /* Fix spacing for plot containers */
      .plot-container-wrapper {
        margin-bottom: 120px !important;
      }
    "))
  ),
  
  # Application title
  title = "Flow Cytometry Analysis Tool",
  
  # Tab for the Settings Module
  tabPanel("Settings", 
           settingsModuleUI("settings")
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
  # Initialize reactive values for global application state
  app_state <- reactiveValues(
    # Global plot settings controlled by the settings module
    plot_settings = list(
      width = 800,
      height = 600,
      font_size = 18,
      point_size = 10,
      color_palette = "viridis"
    )
  )
  
  # Call module servers
  settings_results <- settingsModuleServer("settings", app_state = app_state)
  
  # Pass app_state to all modules that need access to global settings
  raw_data_results <- rawDataModuleServer("raw_data", app_state = app_state)
  
  processed_data_results <- processedDataModuleServer("processed_data", app_state = app_state)
  
  batch_analysis_results <- batchAnalysisModuleServer("batch_analysis", app_state = app_state)
  
  # Create an observer to trigger plot refreshes when settings change
  observe({
    # This will re-execute whenever any plot setting changes
    font_size <- app_state$plot_settings$font_size
    
    # Update CSS variable for plotly fonts if possible
    session$sendCustomMessage(type = "updatePlotlyFonts", message = list(
      fontSize = font_size
    ))
    
    # Delay execution slightly to allow for other reactives to update
    invalidateLater(300)
    
    # Explicitly target main plots by ID with font size
    session$sendCustomMessage(type = "plotly-replot", message = list(
      id = "raw_data-tsnePlot", 
      fontSize = font_size
    ))
    session$sendCustomMessage(type = "plotly-replot", message = list(
      id = "raw_data-umapPlot",
      fontSize = font_size
    ))
    session$sendCustomMessage(type = "plotly-replot", message = list(
      id = "raw_data-clusterPlot",
      fontSize = font_size
    ))
    session$sendCustomMessage(type = "plotly-replot", message = list(
      id = "batch_analysis-sampleDimensionalityPlot",
      fontSize = font_size
    ))
    session$sendCustomMessage(type = "plotly-replot", message = list(
      id = "batch_analysis-sampleClusterPlot",
      fontSize = font_size
    ))
  })
  
  # Add JavaScript to ensure font settings are applied to plotly charts
  session$onSessionEnded(function() {
    # Clean up any resources when session ends
  })
}

# Run the app
shinyApp(ui = ui, server = server)