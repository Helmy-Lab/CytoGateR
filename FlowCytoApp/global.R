# Global file for Flow Cytometry Analysis Tool
# Loads libraries and common functions

# Load required libraries
library(shiny)
library(shinydashboard)  # For dashboard components
library(shinyjs)  # For JavaScript functionality
library(flowCore)
library(ggplot2)
library(Rtsne)
library(uwot)
library(data.table)
library(plotly)
library(shinycssloaders)
library(dplyr)
library(tidyr)
library(DT)
library(readr)
library(openxlsx)
library(shinythemes)
library(shinyWidgets)
library(cluster)
library(tools)  # For file path utilities
# libraries for clustering
library(dbscan)
library(FlowSOM)
library(Rphenograph)
library(igraph)
# Flow-specific libraries
library(flowAI)
library(flowDensity)
library(flowStats)  # For spillover compensation
suppressPackageStartupMessages(library(openCyto))
suppressPackageStartupMessages(library(flowClust))
suppressPackageStartupMessages(library(flowWorkspace))  # For GatingSet functionality
suppressPackageStartupMessages(library(ggcyto))  # For ggplot2-based flow cytometry plotting
library(flowMatch)
library(MASS)  # For density estimation in gating
library(gridExtra)  # For arranging multiple plots

# Set global options
options(shiny.maxRequestSize = 250*1024^2)

# Source utility functions
files_to_source <- list.files("R/utils", pattern = "\\.R$", full.names = TRUE)
for(file in files_to_source) {
  source(file)
}

# Source modules
module_files <- list.files("R/modules", pattern = "\\.R$", full.names = TRUE)
for(file in module_files) {
  source(file)
}

# Common helper function to get the active color palette
get_color_palette <- function(palette_name) {
  switch(palette_name,
         "viridis" = scale_color_viridis_d(),
         "plasma" = scale_color_viridis_d(option = "plasma"),
         "magma" = scale_color_viridis_d(option = "magma"),
         "inferno" = scale_color_viridis_d(option = "inferno"),
         "blues" = scale_color_brewer(palette = "Blues"),
         "reds" = scale_color_brewer(palette = "Reds"),
         "brewer_paired" = scale_color_brewer(palette = "Paired"),
         "brewer_brbg" = scale_color_brewer(palette = "BrBG"),
         scale_color_viridis_d()  # Default fallback
  )
}

# Function to get fill palette
get_fill_palette <- function(palette_name) {
  switch(palette_name,
         "viridis" = scale_fill_viridis_c(),
         "plasma" = scale_fill_viridis_c(option = "plasma"),
         "magma" = scale_fill_viridis_c(option = "magma"),
         "inferno" = scale_fill_viridis_c(option = "inferno"),
         "blues" = scale_fill_distiller(palette = "Blues", direction = 1),
         "reds" = scale_fill_distiller(palette = "Reds", direction = 1),
         "brewer_paired" = scale_fill_brewer(palette = "Paired"),
         "brewer_brbg" = scale_fill_brewer(palette = "BrBG"),
         scale_fill_viridis_c()  # Default fallback
  )
}

# Performance-optimized ggplot theme
get_standard_theme <- function(font_size = 12) {
  # Start with minimal theme as it's simpler
  theme_minimal(base_size = font_size) +
    theme(
      # Essential styling only - reduces calculation burden
      plot.title = element_text(face = "bold", hjust = 0.5, size = font_size * 1.2),
      plot.subtitle = element_text(hjust = 0.5, size = font_size),
      axis.title = element_text(face = "bold", size = font_size * 1.1),
      axis.text = element_text(size = font_size),
      
      # Simplified legend styling
      legend.title = element_text(face = "bold", size = font_size * 1.1),
      legend.text = element_text(size = font_size),
      legend.position = "bottom",
      legend.box = "horizontal",
      
      # Simplified grid - less elements to render
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      
      # Important margin for larger plots - balanced for performance
      plot.margin = margin(15, 15, 100, 15)
    )
}

# Optimized tooltip for plotly
create_standard_tooltip <- function(plot, height = 600, width = 800, font_size = 12) {
  # Batch all layout changes in a single call for better performance
  plot %>% layout(
    # Group related properties together 
    font = list(
      family = "Arial",
      size = font_size,
      color = "black"
    ),
    
    # Hover settings
    hoverlabel = list(
      bgcolor = "white",
      font = list(family = "Arial", size = font_size)
    ),
    
    # Size settings
    height = height,
    width = width,
    
    # Margin - significantly increased bottom margin for more space
    margin = list(b = 250, l = 70, t = 70, r = 40),
  
    # Critical for aspect ratio but allow setting to auto
    autosize = TRUE
  )
}