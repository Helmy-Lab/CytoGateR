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
library(ggpointdensity)  # For point density coloring

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

# Flow cytometry-optimized color palettes
getFlowCytometryColors <- function(palette_name) {
  switch(palette_name,
    "flow_classic" = {
      # Classic flow cytometry: Dark blue -> Cyan -> Green -> Yellow -> Red
      c("#000080", "#0066CC", "#00CCFF", "#00FF66", "#FFFF00", "#FF6600", "#FF0000")
    },
    "flowjo_style" = {
      # FlowJo-inspired: Dark blue -> Light blue -> White -> Pink -> Red
      c("#000066", "#0066CC", "#66CCFF", "#FFFFFF", "#FF99CC", "#FF3366", "#CC0000")
    },
    "density_optimized" = {
      # Optimized for density plots: Very dark blue -> Bright cyan -> Yellow
      c("#000033", "#000066", "#003399", "#0066CC", "#00CCFF", "#66FFCC", "#CCFF66", "#FFFF00")
    },
    "turbo" = {
      # High contrast turbo-style colormap
      c("#30123B", "#4A1486", "#5E35B1", "#7E57C2", "#9C7ECC", "#B0A4D3", 
        "#C5CBD7", "#D8F1DB", "#E4F6DB", "#F0FFC0", "#FCFFA4", "#FDE68A", 
        "#FECC5C", "#FD9A3A", "#FA7D2D", "#F15F30", "#E5443A", "#D92120")
    },
    {
      # Default fallback
      c("#000080", "#0066CC", "#00CCFF", "#00FF66", "#FFFF00", "#FF6600", "#FF0000")
    }
  )
}

# Helper function to get color scales with flow cytometry optimization
getColorScale <- function(scale_name, type = "color", name = "Value", data_values = NULL, force_discrete = FALSE) {
  # Determine if data is discrete or continuous
  is_continuous <- TRUE
  
  # Force discrete if requested (for density contours)
  if (force_discrete) {
    is_continuous <- FALSE
  } else if (!is.null(data_values) && length(data_values) > 0) {
    # Check if data is numeric and has reasonable range for continuous scale
    if (is.numeric(data_values)) {
      # Remove NA values for analysis
      clean_data <- data_values[!is.na(data_values) & is.finite(data_values)]
      if (length(clean_data) > 0) {
        unique_vals <- length(unique(clean_data))
        # If fewer than 10 unique values, treat as discrete
        is_continuous <- unique_vals >= 10
      } else {
        # If no valid data, default to continuous
        is_continuous <- TRUE
      }
    } else {
      # Non-numeric data is always discrete
      is_continuous <- FALSE
    }
  }
  
  if (type == "color") {
    if (is_continuous) {
      switch(scale_name,
        # Flow cytometry specific palettes
        "flow_classic" = scale_color_gradientn(colors = getFlowCytometryColors("flow_classic"), name = name),
        "flowjo_style" = scale_color_gradientn(colors = getFlowCytometryColors("flowjo_style"), name = name),
        "density_optimized" = scale_color_gradientn(colors = getFlowCytometryColors("density_optimized"), name = name),
        "turbo" = scale_color_gradientn(colors = getFlowCytometryColors("turbo"), name = name),
        # Standard palettes
        "viridis" = scale_color_viridis_c(name = name),
        "plasma" = scale_color_viridis_c(option = "plasma", name = name),
        "heat" = scale_color_gradientn(colors = heat.colors(7), name = name),
        "spectral" = scale_color_distiller(palette = "Spectral", name = name),
        # Default to flow_classic for best cytometry visualization
        scale_color_gradientn(colors = getFlowCytometryColors("flow_classic"), name = name)
      )
    } else {
      # For discrete scales, calculate number of unique values safely
      n_unique <- if (!is.null(data_values) && length(data_values) > 0) {
        length(unique(data_values[!is.na(data_values)]))
      } else {
        6  # Default number of colors
      }
      
      switch(scale_name,
        # Flow cytometry discrete palettes - use subset of continuous colors
        "flow_classic" = scale_color_manual(values = getFlowCytometryColors("flow_classic")[1:min(n_unique, 7)], name = name),
        "flowjo_style" = scale_color_manual(values = getFlowCytometryColors("flowjo_style")[1:min(n_unique, 7)], name = name),
        "density_optimized" = scale_color_manual(values = getFlowCytometryColors("density_optimized")[1:min(n_unique, 8)], name = name),
        "turbo" = scale_color_manual(values = getFlowCytometryColors("turbo")[seq(1, 18, length.out = min(n_unique, 10))], name = name),
        # Standard discrete palettes
        "viridis" = scale_color_viridis_d(name = name),
        "plasma" = scale_color_viridis_d(option = "plasma", name = name),
        "heat" = scale_color_manual(values = heat.colors(n_unique), name = name),
        "spectral" = if(n_unique <= 11) scale_color_brewer(palette = "Spectral", name = name) else scale_color_viridis_d(name = name),
        # Default
        scale_color_manual(values = getFlowCytometryColors("flow_classic")[1:min(n_unique, 7)], name = name)
      )
    }
  } else {  # fill
    if (is_continuous) {
      switch(scale_name,
        # Flow cytometry specific palettes
        "flow_classic" = scale_fill_gradientn(colors = getFlowCytometryColors("flow_classic"), name = name),
        "flowjo_style" = scale_fill_gradientn(colors = getFlowCytometryColors("flowjo_style"), name = name),
        "density_optimized" = scale_fill_gradientn(colors = getFlowCytometryColors("density_optimized"), name = name),
        "turbo" = scale_fill_gradientn(colors = getFlowCytometryColors("turbo"), name = name),
        # Standard palettes
        "viridis" = scale_fill_viridis_c(name = name),
        "plasma" = scale_fill_viridis_c(option = "plasma", name = name),
        "heat" = scale_fill_gradientn(colors = heat.colors(7), name = name),
        "spectral" = scale_fill_distiller(palette = "Spectral", name = name),
        # Default
        scale_fill_gradientn(colors = getFlowCytometryColors("flow_classic"), name = name)
      )
    } else {
      # For discrete scales, calculate number of unique values safely
      n_unique <- if (!is.null(data_values) && length(data_values) > 0) {
        length(unique(data_values[!is.na(data_values)]))
      } else {
        6  # Default number of colors
      }
      
      switch(scale_name,
        # Flow cytometry discrete palettes
        "flow_classic" = scale_fill_manual(values = getFlowCytometryColors("flow_classic")[1:min(n_unique, 7)], name = name),
        "flowjo_style" = scale_fill_manual(values = getFlowCytometryColors("flowjo_style")[1:min(n_unique, 7)], name = name),
        "density_optimized" = scale_fill_manual(values = getFlowCytometryColors("density_optimized")[1:min(n_unique, 8)], name = name),
        "turbo" = scale_fill_manual(values = getFlowCytometryColors("turbo")[seq(1, 18, length.out = min(n_unique, 10))], name = name),
        # Standard discrete palettes
        "viridis" = scale_fill_viridis_d(name = name),
        "plasma" = scale_fill_viridis_d(option = "plasma", name = name),
        "heat" = scale_fill_manual(values = heat.colors(n_unique), name = name),
        "spectral" = if(n_unique <= 11) scale_fill_brewer(palette = "Spectral", name = name) else scale_fill_viridis_d(name = name),
        # Default
        scale_fill_manual(values = getFlowCytometryColors("flow_classic")[1:min(n_unique, 7)], name = name)
      )
    }
  }
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