# Main application file
# Flow Cytometry Analysis Tool

# Source global.R first
source("global.R")

# Define UI
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$script("$(document).on('shiny:connected', function() { 
      Shiny.setInputValue('showBatchClustering', true);
      Shiny.setInputValue('showBatchPreprocessing', false);
      Shiny.setInputValue('showBatchAdvancedCluster', false);
    });"),
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  
  titlePanel("Flow Cytometry Analysis Tool"),
  
  # Application tabs
  tabsetPanel(
    id = "main_tabs",
    # Settings Module
    tabPanel("Application Settings", settingsModuleUI("settings")),
    
    # Raw Data Analysis Module
    tabPanel("Raw Data Analysis", rawDataModuleUI("raw_data")),
    
    # Processed Data Analysis Module
    tabPanel("Processed Data Analysis", processedDataModuleUI("processed_data")),
    
    # Batch Analysis Module
    tabPanel("Batch Analysis", batchAnalysisModuleUI("batch_analysis"))
  )
)

# Define server
server <- function(input, output, session) {
  # Initialize reactive values for global application state
  app_state <- reactiveValues(
    # Global plot settings controlled by the settings module
    plot_settings = list(
      width = 800,
      height = 500,
      font_size = 12,
      point_size = 6,
      color_palette = "viridis"
    )
  )
  
  # Call module servers
  settings_results <- settingsModuleServer("settings", app_state = app_state)
  
  # Pass app_state to all modules that need access to global settings
  raw_data_results <- rawDataModuleServer("raw_data", app_state = app_state)
  
  processed_data_results <- processedDataModuleServer("processed_data", app_state = app_state)
  
  batch_analysis_results <- batchAnalysisModuleServer("batch_analysis", app_state = app_state)
}

# Run the app
shinyApp(ui = ui, server = server)