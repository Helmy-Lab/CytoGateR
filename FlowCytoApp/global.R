# Global file for Flow Cytometry Analysis Tool
# Loads libraries and common functions

# Load required libraries
library(shiny)
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
# libraries for clustering
library(dbscan)
library(FlowSOM)
library(Rphenograph)
library(igraph)
# Flow-specific libraries
library(flowAI)
library(flowDensity)
suppressPackageStartupMessages(library(openCyto))
suppressPackageStartupMessages(library(flowClust))
library(flowMatch)

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

# Function to create a standard ggplot theme
get_standard_theme <- function(font_size = 12) {
  theme_minimal(base_size = font_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = rel(1.2)),
      plot.subtitle = element_text(hjust = 0.5, size = rel(1.0)),
      axis.title = element_text(face = "bold", size = rel(1.1)),
      axis.text = element_text(size = rel(1.0)),
      legend.title = element_text(face = "bold", size = rel(1.1)),
      legend.text = element_text(size = rel(1.0)),
      legend.position = "right",
      legend.background = element_rect(fill = "white", color = "gray90"),
      legend.key = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      panel.border = element_rect(color = "grey80", fill = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(20, 20, 80, 20) # Increased bottom margin from 30 to 80 to prevent button overlap
    )
}

# Function to create standard tooltips for plotly
create_standard_tooltip <- function(plot, height = 600, width = 800, font_size = 12) {
  plot %>% layout(
    hoverlabel = list(
      bgcolor = "white",
      font = list(family = "Arial", size = font_size)
    ),
    margin = list(b = 100, l = 80, t = 100, r = 50), # Increased bottom margin from 80 to 100
    height = height,
    width = width,
    xaxis = list(
      title = list(
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
    yaxis = list(
      title = list(
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
    title = list(
      font = list(
        family = "Arial",
        size = font_size * 1.2,
        color = "black"
      )
    )
  )
}