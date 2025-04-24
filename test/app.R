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
# install.packages("devtools")
# devtools::install_github("JinmiaoChenLab/Rphenograph")
# install.packages("FlowSOM")

# Load additional preprocessing libraries
library(flowAI)
library(flowDensity)
suppressPackageStartupMessages(library(openCyto))
suppressPackageStartupMessages(library(flowClust))
library(flowMatch)

options(shiny.maxRequestSize = 250*1024^2)

# ----------------- MODULAR PREPROCESSING PIPELINE -----------------

# Data loading function
loadFlowData <- function(file_path, file_name) {
  ext <- tools::file_ext(file_name)
  
  # Load data based on file extension
  data <- switch(ext,
                 "fcs" = read.FCS(file_path, transformation = FALSE),
                 "csv" = fread(file_path),
                 "tsv" = fread(file_path, sep = "\t"),
                 stop("Unsupported file format")
  )
  
  return(list(
    data = data,
    file_name = file_name,
    ext = ext
  ))
}

# Quality control function
performQC <- function(flow_data, qc_settings = list()) {
  # Default settings
  default_settings <- list(
    remove_margins = TRUE,
    min_cells = 100,
    max_anomalies = 0.1
  )
  
  # Merge with user-provided settings
  settings <- modifyList(default_settings, qc_settings)
  
  # Apply QC only for FCS files
  if (inherits(flow_data, "flowFrame")) {
    # Get initial cell count
    initial_count <- nrow(flow_data)
    
    # Apply flowAI QC
    tryCatch({
      qc_result <- flow_auto_qc(
        flow_data,
        alpha = 0.01
      )
      
      # Check if enough cells remain
      if (nrow(qc_result) < settings$min_cells) {
        warning("QC removed too many cells. Using original data.")
        qc_result <- flow_data
      }
      
      # Calculate percent removed
      removed_pct <- 1 - (nrow(qc_result) / initial_count)
      
      # If too many anomalies were found, use original data
      if (removed_pct > settings$max_anomalies) {
        warning(sprintf("QC found too many anomalies (%.1f%%). Using original data.", removed_pct * 100))
        qc_result <- flow_data
      }
      
      return(list(
        data = qc_result,
        metrics = list(
          initial_count = initial_count,
          final_count = nrow(qc_result),
          removed_pct = removed_pct
        )
      ))
    }, error = function(e) {
      warning("QC process failed: ", e$message, ". Using original data.")
      return(list(
        data = flow_data,
        metrics = list(
          initial_count = initial_count,
          final_count = initial_count,
          removed_pct = 0,
          error = e$message
        )
      ))
    })
  } else {
    # For non-FCS data, return as is
    return(list(
      data = flow_data, 
      metrics = list(
        initial_count = nrow(flow_data),
        final_count = nrow(flow_data),
        removed_pct = 0,
        message = "QC not applicable for non-FCS files"
      )
    ))
  }
}

# Gating function to remove debris and dead cells
performGating <- function(flow_data, gates = list()) {
  # Default gates
  default_gates <- list(
    debris_gate = c("FSC-A", "SSC-A"),
    live_dead_gate = NULL
  )
  
  # Merge with user-provided gates
  gates <- modifyList(default_gates, gates)
  
  # Apply gating only for FCS files
  if (inherits(flow_data, "flowFrame")) {
    # Track cell counts
    initial_count <- nrow(flow_data)
    gated_data <- flow_data
    
    # Apply debris gate if parameters exist
    if (!is.null(gates$debris_gate) && 
        all(gates$debris_gate %in% colnames(flow_data@exprs))) {
      
      # Create a debris gate using flowDensity
      tryCatch({
        # Get FSC and SSC parameters
        fsc_param <- gates$debris_gate[1]
        ssc_param <- gates$debris_gate[2]
        
        # Create a simple rectangular gate to remove very low FSC/SSC events
        debris_gate <- rectangleGate(
          filterId = "Debris",
          fsc_param = c(100, Inf),
          ssc_param = c(50, Inf)
        )
        names(debris_gate@min) <- c(fsc_param, ssc_param)
        names(debris_gate@max) <- c(fsc_param, ssc_param)
        
        # Create proper gate structure for debris
        gate_ranges <- list(
          c(100, Inf),  # FSC range
          c(50, Inf)    # SSC range
        )
        names(gate_ranges) <- c(fsc_param, ssc_param)
        
        debris_gate <- rectangleGate(
          gate_ranges,
          filterId = "Debris"
        )
        
        # Apply gate
        gated_data <- Subset(gated_data, debris_gate)
      }, error = function(e) {
        warning("Debris gating failed: ", e$message)
      })
    }
    
    # Apply live/dead gate if parameter exists
    if (!is.null(gates$live_dead_gate) && 
        gates$live_dead_gate %in% colnames(flow_data@exprs)) {
      
      tryCatch({
        # Create a threshold gate for live cells (lower viability dye)
        # Create gate with proper syntax 
        gate_ranges <- list(c(-Inf, 1000))  # Threshold value, adjust as needed
        names(gate_ranges) <- gates$live_dead_gate
        
        live_gate <- rectangleGate(
          gate_ranges,
          filterId = "Live"
        )
        
        # Apply gate
        gated_data <- Subset(gated_data, live_gate)
      }, error = function(e) {
        warning("Live/dead gating failed: ", e$message)
      })
    }
    
    # Calculate gating results
    final_count <- nrow(gated_data)
    removed_pct <- 1 - (final_count / initial_count)
    
    return(list(
      data = gated_data,
      metrics = list(
        initial_count = initial_count,
        final_count = final_count,
        removed_pct = removed_pct
      )
    ))
  } else {
    # For non-FCS data, return as is
    return(list(
      data = flow_data, 
      metrics = list(
        initial_count = nrow(flow_data),
        final_count = nrow(flow_data),
        removed_pct = 0,
        message = "Gating not applicable for non-FCS files"
      )
    ))
  }
}

# Transform data function
transformData <- function(flow_data, markers, transform = TRUE, cofactor = 5) {
  # Extract expression data
  if (inherits(flow_data, "flowFrame")) {
    exprs_data <- exprs(flow_data)[, markers, drop = FALSE]
  } else {
    exprs_data <- as.matrix(flow_data[, markers, drop = FALSE])
  }
  
  # Apply transformation if requested
  if (transform) {
    transformed_data <- apply(exprs_data, 2, asinhTransform, cofactor = cofactor)
  } else {
    transformed_data <- exprs_data
  }
  
  return(transformed_data)
}

# Down-sampling function
sampleCells <- function(data, n_events, seed = 123) {
  set.seed(seed)
  n_rows <- nrow(data)
  
  if (n_rows <= n_events) {
    return(list(
      data = data,
      indices = 1:n_rows
    ))
  }
  
  # Sample indices
  sampled_indices <- sample(n_rows, n_events)
  sampled_data <- data[sampled_indices, ]
  
  return(list(
    data = sampled_data,
    indices = sampled_indices
  ))
}

# Main preprocessing pipeline function
preprocessFlowData <- function(input_data, preprocessing_params = list()) {
  # Default parameters
  default_params <- list(
    markers = NULL,  # Must be provided
    transform = TRUE,
    cofactor = 5,
    n_events = 5000,
    perform_qc = TRUE,
    perform_gating = TRUE,
    scale_data = TRUE,
    qc_settings = list(),
    gates = list(),
    seed = 123
  )
  
  # Merge with user-provided parameters
  params <- modifyList(default_params, preprocessing_params)
  
  # Check for required parameters
  if (is.null(params$markers)) {
    stop("Markers must be provided for preprocessing")
  }
  
  # Initialize results tracking
  results <- list(
    raw_data = input_data,
    metrics = list()
  )
  
  # Step 1: Quality Control
  if (params$perform_qc) {
    qc_result <- performQC(input_data, params$qc_settings)
    results$qc_data <- qc_result$data
    results$metrics$qc <- qc_result$metrics
  } else {
    results$qc_data <- input_data
  }
  
  # Step 2: Gating
  if (params$perform_gating) {
    gating_result <- performGating(results$qc_data, params$gates)
    results$gated_data <- gating_result$data
    results$metrics$gating <- gating_result$metrics
  } else {
    results$gated_data <- results$qc_data
  }
  
  # Step 3: Transform data
  transformed_data <- transformData(
    results$gated_data, 
    params$markers, 
    params$transform, 
    params$cofactor
  )
  results$transformed_data <- transformed_data
  
  # Step 4: Sample cells
  sampled_result <- sampleCells(transformed_data, params$n_events, params$seed)
  results$sampled_data <- sampled_result$data
  results$sampled_indices <- sampled_result$indices
  
  # Step 5: Scale data
  if (params$scale_data) {
    results$scaled_data <- scale(results$sampled_data)
  } else {
    results$scaled_data <- results$sampled_data
  }
  
  return(results)
}

# Function to apply preprocessing to both control and treated samples for comparison
preprocessComparisonData <- function(control_data, treated_data, preprocessing_params = list()) {
  # Preprocess each dataset
  control_results <- preprocessFlowData(control_data, preprocessing_params)
  treated_results <- preprocessFlowData(treated_data, preprocessing_params)
  
  # Combine the results
  comparison_results <- list(
    control = control_results,
    treated = treated_results,
    metrics = list(
      control = control_results$metrics,
      treated = treated_results$metrics
    )
  )
  
  return(comparison_results)
}

asinhTransform <- function(x, cofactor = 5) {
  asinh(x / cofactor)
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$script("$(document).on('shiny:connected', function() { 
      Shiny.setInputValue('showBatchClustering', true);
      Shiny.setInputValue('showBatchPreprocessing', false);
      Shiny.setInputValue('showBatchAdvancedCluster', false);
    });"),
    tags$style(HTML("
      /* Improve readability of all plot text */
      .plotly text, .ggplot text, .plot text {
        font-family: 'Arial', sans-serif !important;
      }
      /* Add some spacing between UI elements */
      .well { margin-bottom: 15px; }
      /* Ensure consistent widths for buttons */
      .action-button { width: 100%; }
    "))
  ),
  
  titlePanel("Flow Cytometry Analysis Tool"),

  # --- Application Settings ---
  tabPanel("Application Settings",
    fluidRow(
      column(4,
        wellPanel(
          h4("Global Plot Settings", style = "text-align: center; font-weight: bold;"),
          
          sliderInput("global_plot_width", "Plot Width", 
                      min = 300, max = 1200, value = 800, step = 50),
          
          sliderInput("global_plot_height", "Plot Height", 
                      min = 300, max = 1200, value = 800, step = 50),
          
          sliderInput("global_font_size", "Base Font Size", 
                      min = 8, max = 36, value = 12, step = 1),
          
          sliderInput("global_point_size", "Point Size", 
                      min = 2, max = 12, value = 6, step = 1),
          
          radioButtons("global_color_palette", "Color Palette",
                       choices = c("Viridis" = "viridis", 
                                  "Plasma" = "plasma", 
                                  "Blues" = "blues",
                                  "Reds" = "reds"),
                       selected = "viridis"),
          
          actionButton("apply_plot_settings", "Apply Settings to All Plots", 
                       class = "btn-primary")
        )
      ),
      column(8,
        wellPanel(
          h4("Plot Preview", style = "text-align: center; font-weight: bold;")
        )
      )
    )
  ),

  # --- Raw Data Analysis Tabs ---
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Raw Data Analysis", 
             sidebarLayout(
               sidebarPanel(
                 fileInput("fcsFile", "Upload FCS/CSV/TSV File", accept = c(".fcs", ".csv", ".tsv")),
                 uiOutput("markerSelectUI"),
                 
                 # Transform options
                 checkboxInput("transform", "Apply arcsinh transformation", value = TRUE),
                 numericInput("cofactor", "Transformation cofactor", value = 5, min = 1, max = 10),
                 numericInput("nEvents", "Number of events to analyze", value = 5000, min = 100, step = 1000),
                 
                 # Dimensionality reduction method selection
                 checkboxGroupInput("methods", "Select Dimensionality Reduction Methods",
                                  choices = c("t-SNE", "UMAP"),
                                  selected = c("t-SNE", "UMAP")),
                 
                 # Dimensionality reduction parameters
                 conditionalPanel(
                   condition = "input.methods.includes('t-SNE')",
                   numericInput("perplexity", "t-SNE Perplexity", value = 30, min = 5, max = 50),
                   checkboxInput("use_barnes_hut", "Use Barnes-Hut Approximation (faster)", value = TRUE),
                   conditionalPanel(
                     condition = "input.use_barnes_hut",
                     sliderInput("tsne_theta", "Barnes-Hut theta (higher theta = speed, lower theta = accuracy)", 
                                 min = 0.0, max = 1.0, value = 0.5, step = 0.1)
                   ),
                  conditionalPanel(
                     condition = "!input.use_barnes_hut",
                     tags$div(class = "alert alert-warning",
                              "Warning: Exact t-SNE is very slow for datasets > 1000 cells.")
                   ),
                   numericInput("tsne_max_iter", "Maximum Iterations", value = 1000, min = 100, max = 10000, step = 100)
                 ),
                 conditionalPanel(
                   condition = "input.methods.includes('UMAP')",
                   numericInput("n_neighbors", "UMAP n_neighbors", value = 15, min = 2, max = 100)
                 ),
                 
                 # QC and gating options
                 checkboxInput("performQC", "Perform Quality Control", value = TRUE),
                 numericInput("maxAnomalies", "Max Anomalies (%)", value = 10, min = 0, max = 50),
                 
                 checkboxInput("performGating", "Perform Debris/Dead Cell Gating", value = TRUE),
                 conditionalPanel(
                   condition = "input.performGating === true",
                   textInput("debrisGate", "FSC/SSC Parameters (comma-separated)", 
                            value = "FSC-A,SSC-A"),
                   selectInput("liveDeadGate", "Live/Dead Parameter", 
                              choices = c("None", "Live Dead BV570 Violet-610-A"),
                              selected = "None")
                 ),
                 
                 # Clustering options
                 h4("Clustering Options"),
                 checkboxInput("showClusteringOptions", "Enable Clustering", value = TRUE),
                 conditionalPanel(
                   condition = "input.showClusteringOptions === true",
                   selectInput("clusterMethod", "Clustering Method",
                               choices = c("K-means", "DBSCAN", "FlowSOM", "Phenograph"),
                               selected = "FlowSOM"),
                   
                   conditionalPanel(
                     condition = "input.clusterMethod === 'K-means'",
                     numericInput("numClusters", "Number of Clusters", value = 8, min = 2, max = 20)
                   ),
                   conditionalPanel(
                     condition = "input.clusterMethod === 'DBSCAN'",
                     numericInput("dbscanEps", "DBSCAN epsilon", value = 0.5, min = 0.01, step = 0.05),
                     numericInput("dbscanMinPts", "DBSCAN minPts", value = 5, min = 1, step = 1)
                   ),
                   conditionalPanel(
                     condition = "input.clusterMethod === 'FlowSOM'",
                     numericInput("som_xdim", "SOM Grid X dimension", value = 6, min = 2, max = 20),
                     numericInput("som_ydim", "SOM Grid Y dimension", value = 6, min = 2, max = 20),
                     numericInput("som_clusters", "Number of metaclusters", value = 12, min = 2, max = 30)
                   ),
                   conditionalPanel(
                     condition = "input.clusterMethod === 'Phenograph'",
                     numericInput("phenoK", "Phenograph k nearest neighbors", value = 30, min = 5, max = 100)
                   ),
                   
                   # Population identification
                   h5("Population Identification"),
                   actionButton("identifyPopulations", "Identify Cell Populations", class = "btn-success"),
                   checkboxInput("showPopulationLabels", "Show Population Labels", value = TRUE),
                   
                   # Advanced identification options
                   checkboxInput("showAdvancedIdentOptions", "Show Advanced Population ID Settings", value = FALSE),
                   conditionalPanel(
                     condition = "input.showAdvancedIdentOptions === true",
                     sliderInput("highExpressionThreshold", "High Expression Threshold", 
                                min = 0.1, max = 1.5, value = 0.5, step = 0.1),
                     sliderInput("lowExpressionThreshold", "Low Expression Threshold", 
                                min = -1.5, max = -0.1, value = -0.5, step = 0.1),
                     sliderInput("minConfidenceThreshold", "Minimum Confidence Threshold (%)", 
                                min = 10, max = 90, value = 30, step = 5)
                   )
                 ),
                 
                 # Run button
                   hr(),
                 actionButton("run", "Run Analysis", class = "btn-primary"),
                 
                 # Run clustering button (separate action)
                 conditionalPanel(
                   condition = "input.showClusteringOptions === true",
                   br(),
                   actionButton("runClustering", "Run Clustering", class = "btn-success")
                 ),
                 
                 # Visualization options
                 hr(),
                 h4("Visualization Options"),
                 numericInput("plotWidth", "Plot Width", value = 800, min = 400, max = 2000, step = 100),
                 numericInput("plotHeight", "Plot Height", value = 600, min = 300, max = 1500, step = 100),
                 
                 # Download options
                 hr(),
                 h4("Download Results"),
                 conditionalPanel(
                   condition = "input.showClusteringOptions === true",
                   downloadButton("downloadClusterTable", "Download Cluster Data"),
                   br(), br()
                 ),
                 downloadButton("downloadProcessedData", "Download Processed Data")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("t-SNE", withSpinner(plotlyOutput("tsnePlot", height = "600px"))),
                   tabPanel("UMAP", withSpinner(plotlyOutput("umapPlot", height = "600px"))),
                   tabPanel("Data Info", verbatimTextOutput("fcsInfo")),
                   # New tabs for cluster analysis
                   tabPanel("Cluster Analysis", 
                            conditionalPanel(
                              condition = "input.showClusteringOptions == true",
                              tabsetPanel(
                                tabPanel("Cluster Visualization", plotlyOutput("clusterPlot")),
                                tabPanel("Cluster Profiles", withSpinner(plotOutput("clusterHeatmap"))),
                                tabPanel("Cluster Statistics", DT::dataTableOutput("clusterStats")),
                                tabPanel("Identified Populations", DT::dataTableOutput("populationTable"))
                              )
                            )
                   )
                 )
               )
             )
    ),
    
    # --- Cleaned Data Analysis Tabs ---
    tabPanel("Cleaned Data Analysis",
             sidebarLayout(
               sidebarPanel(
                 fileInput("cleanedFile", "Upload CSV/TSV/Excel File", accept = c(".csv", ".tsv", ".xlsx")),
                 uiOutput("analysis_type_ui"),
                 uiOutput("marker_ui"),
                 uiOutput("treatment_ui"),
                 selectInput("dimred_method", "Dimensionality Reduction Method", choices = c("t-SNE", "UMAP")),
                 numericInput("perplexity_cleaned", "t-SNE: Perplexity", value = 5, min = 2, max = 50),
                 numericInput("neighbors", "UMAP: n_neighbors", value = 5, min = 2, max = 100),
                 numericInput("min_dist", "UMAP: min_dist", value = 0.1, min = 0, max = 1, step = 0.05),
                 numericInput("n_clusters", "Number of Clusters (k-means)", value = 3, min = 1),
                 sliderInput("plot_width", "Plot Width (px)", min = 300, max = 1200, value = 600, step = 50),
                 sliderInput("plot_height", "Plot Height (px)", min = 300, max = 1200, value = 600, step = 50),
                 actionButton("run_cleaned", "Run Analysis", class = "btn-primary")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Data Preview", DTOutput("preview")),
                   tabPanel("Structure Detection", verbatimTextOutput("structure")),
                   tabPanel("Results Plot", plotlyOutput("plot")),
                   tabPanel("t-SNE / UMAP", plotlyOutput("dimred_plot", width = "auto", height = "auto")),
                   tabPanel("Summary Table", DTOutput("summary_table"))
                 )
               )
             )
    ),
    
    # --- Batch Analysis Tab (replacement for Sample Comparison) ---
    tabPanel("Batch Analysis",
             sidebarLayout(
               sidebarPanel(
                 h4("Sample Management"),
                 # Sample File Upload and Group Assignment
                 fileInput("batchFile", "Upload FCS/CSV/TSV File", accept = c(".fcs", ".csv", ".tsv"), multiple = TRUE),
                 uiOutput("batchSampleList"),  # Dynamic UI for sample list and group assignment
                 actionButton("addSample", "Add Selected Files", class = "btn-info"),
                 hr(),
                 
                 # Sample Grouping Options
                 selectInput("groupingVariable", "Group Samples By:",
                             choices = c("Manual Assignment", "Filename Pattern"),
                             selected = "Manual Assignment"),
                 conditionalPanel(
                   condition = "input.groupingVariable === 'Filename Pattern'",
                   textInput("patternControl", "Control Pattern", value = "control|ctrl"),
                   textInput("patternTreated", "Treated Pattern", value = "treated|sample")
                 ),
                 hr(),
                 
                 # Common Analysis Parameters
                 h4("Analysis Parameters"),
                 uiOutput("batchMarkerSelectUI"),
                 
                 # Preprocessing options (same for all samples)
                 checkboxInput("batchTransform", "Apply arcsinh transformation", value = TRUE),
                 numericInput("batchCofactor", "Transformation cofactor", value = 5, min = 1, max = 10),
                 numericInput("batchEvents", "Events per sample", value = 5000, min = 100, step = 1000),
                 
                 # Show advanced preprocessing
                 checkboxInput("showBatchPreprocessing", "Show Advanced Preprocessing", value = FALSE),
                 conditionalPanel(
                   condition = "input.showBatchPreprocessing === true",
                   # QC options
                   h5("Quality Control"),
                   checkboxInput("batchPerformQC", "Perform Quality Control", value = TRUE),
                   conditionalPanel(
                     condition = "input.showBatchPreprocessing === true && input.batchPerformQC === true",
                     numericInput("batchMaxAnomalies", "Max Anomalies (%)", value = 10, min = 0, max = 50)
                   ),
                   
                   # Gating options
                   h5("Gating"),
                   checkboxInput("batchPerformGating", "Perform Debris/Dead Cell Gating", value = TRUE),
                   conditionalPanel(
                     condition = "input.showBatchPreprocessing === true && input.batchPerformGating === true",
                     textInput("batchDebrisGate", "FSC/SSC Parameters (comma-separated)", 
                               value = "FSC-A,SSC-A"),
                     selectInput("batchLiveDeadGate", "Live/Dead Parameter", 
                                 choices = c("None", "Live Dead BV570 Violet-610-A"),
                                 selected = "None")
                   )
                 ),
                 
                 # Dimensionality Reduction
                 h5("Dimensionality Reduction"),
                 selectInput("batchDimRedMethod", "Method", 
                             choices = c("t-SNE", "UMAP"), selected = "t-SNE"),
                 conditionalPanel(
                   condition = "input.batchDimRedMethod === 't-SNE'",
                   sliderInput("batchPerplexity", "t-SNE perplexity", min = 5, max = 50, value = 30),
                   checkboxInput("batch_use_barnes_hut", "Use Barnes-Hut Approximation (faster)", value = TRUE),
                   conditionalPanel(
                     condition = "input.batch_use_barnes_hut",
                     sliderInput("batchTsneTheta", "Barnes-Hut theta (higher theta = speed, lower theta = accuracy)", 
                                 min = 0.0, max = 1.0, value = 0.5, step = 0.1)
                   ),
                   conditionalPanel(
                     condition = "!input.batch_use_barnes_hut",
                     tags$div(class = "alert alert-warning",
                              "Warning: Exact t-SNE is very slow for datasets > 1000 cells.")
                   ),
                   numericInput("batch_tsne_max_iter", "Maximum Iterations", value = 1000, min = 100, max = 10000, step = 100)
                 ),
                 conditionalPanel(
                   condition = "input.batchDimRedMethod === 'UMAP'",
                   sliderInput("batchNeighbors", "UMAP n_neighbors", min = 2, max = 100, value = 15)
                 ),
                 
                 # Add clustering controls
                 hr(),
                 h4("Clustering Options"),
                 checkboxInput("showBatchClustering", "Enable Clustering", value = TRUE),
                 
                 conditionalPanel(
                   condition = "input.showBatchClustering === true",
                   # clustering methods
                   selectInput("batchClusterMethod", "Clustering Method",
                              choices = c("K-means", "FlowSOM", "DBSCAN", "Phenograph"),
                              selected = "FlowSOM"),
                   
                   conditionalPanel(
                     condition = "input.showBatchClustering === true && input.batchClusterMethod === 'K-means'",
                     numericInput("batchNumClusters", "Number of Clusters", value = 8, min = 2, max = 30)
                   ),
                   conditionalPanel(
                     condition = "input.showBatchClustering === true && input.batchClusterMethod === 'FlowSOM'",
                     numericInput("batchSomXdim", "SOM Grid X dimension", value = 6, min = 2, max = 20),
                     numericInput("batchSomYdim", "SOM Grid Y dimension", value = 6, min = 2, max = 20),
                     numericInput("batchSomRlen", "Training iterations", value = 10, min = 5, max = 50, step = 5),
                     numericInput("batchSomClusters", "Number of clusters", value = 12, min = 2, max = 30)
                   ),
                   conditionalPanel(
                     condition = "input.showBatchClustering === true && input.batchClusterMethod === 'DBSCAN'",
                     numericInput("batchDbscanEps", "Epsilon (neighborhood size)", value = 0.5, min = 0.1, max = 5, step = 0.1),
                     numericInput("batchDbscanMinPts", "MinPts (min samples in neighborhood)", value = 5, min = 3, max = 50)
                   ),
                   conditionalPanel(
                     condition = "input.showBatchClustering === true && input.batchClusterMethod === 'Phenograph'",
                     numericInput("batchPhenoK", "k (nearest neighbors)", value = 30, min = 5, max = 100)
                   ),
                   
                   # Population identification
                   checkboxInput("batchIdentifyPops", "Identify Cell Populations", value = TRUE),
                   checkboxInput("batchShowPopLabels", "Show Population Labels", value = TRUE),
                   
                   # Population identification thresholds
                   conditionalPanel(
                     condition = "input.batchIdentifyPops === true",
                     sliderInput("batchHighExpressionThreshold", "High Expression Threshold",
                               min = 0, max = 2, value = 0.5, step = 0.1),
                     sliderInput("batchLowExpressionThreshold", "Low Expression Threshold",
                               min = -2, max = 0, value = -0.5, step = 0.1),
                     sliderInput("batchMinConfidenceThreshold", "Minimum Confidence Threshold (%)",
                               min = 0, max = 100, value = 30, step = 5)
                   )
                 ),
                 
                 # Run button for analysis
                 hr(),
                 actionButton("runBatchAnalysis", "Run Batch Analysis", class = "btn-primary", 
                              style = "width: 100%; font-weight: bold;")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Sample Management",
                            h4("Sample Groups"),
                            DT::dataTableOutput("batchSampleTable"),
                            hr(),
                            fluidRow(
                              column(12, h4("Sample Management Actions")),
                              column(4, actionButton("clearSamples", "Clear All Samples", class = "btn-warning", style = "width: 100%")),
                              column(4, downloadButton("downloadSampleConfig", "Save Sample Config", style = "width: 100%")),
                              column(4, div(style = "width: 100%", fileInput("uploadSampleConfig", "Load Config", accept = c(".csv"))))
                            )
                   ),
                   
                   tabPanel("Sample Visualization",
                            fluidRow(
                              column(3, selectInput("viewSample", "Select Sample:", choices = NULL)),
                              column(9, h4(textOutput("sampleViewTitle"), align = "center"))
                            ),
                            hr(),
                            fluidRow(
                              column(12, 
                                     h4("Dimensionality Reduction Plot", align = "center"),
                                     withSpinner(plotlyOutput("sampleDimensionalityPlot", height = "600px")))
                            ),
                            conditionalPanel(
                              condition = "input.showBatchClustering === true",
                              hr(),
                              fluidRow(
                                column(12, h4("Clustering Visualization", align = "center")),
                                column(6, withSpinner(plotlyOutput("sampleClusterPlot", height = "500px"))),
                                column(6, withSpinner(plotOutput("sampleHeatmap", height = "500px")))
                              ),
                              hr(),
                              fluidRow(
                                column(12, h4("Cluster Statistics", align = "center")),
                                column(12, withSpinner(DT::dataTableOutput("sampleClusterStats")))
                              ),
                              hr(),
                              fluidRow(
                                column(12, h4("Marker Expression by Cluster", align = "center")),
                                column(4, selectInput("sampleMarkerSelect", "Select Marker:", choices = NULL)),
                                column(8, withSpinner(plotlyOutput("markerExpressionByCluster", height = "500px")))
                              )
                            )
                   ),
                   
                   tabPanel("Control vs Treated", 
                            fluidRow(
                              column(12, h4("Sample Comparison", align = "center")),
                              column(6, selectInput("compareViewControl", "Control Sample:", choices = NULL)),
                              column(6, selectInput("compareViewTreated", "Treated Sample:", choices = NULL))
                            ),
                            hr(),
                            fluidRow(
                              column(12, h4("Dimensionality Reduction Comparison", align = "center")),
                              column(6, withSpinner(plotlyOutput("controlSamplePlot", height = "600px"))),
                              column(6, withSpinner(plotlyOutput("treatedSamplePlot", height = "600px")))
                            )
                   ),
                   
                   tabPanel("Cluster Comparison",
                          conditionalPanel(
                            condition = "input.showBatchClustering === true",
                            fluidRow(
                              column(6, selectInput("clusterCompareControl", "Control Sample:", choices = NULL)),
                              column(6, selectInput("clusterCompareTreated", "Treated Sample:", choices = NULL))
                            ),
                            hr(),
                            fluidRow(
                              column(12, h4("Cluster Mapping", align = "center"),
                                    p("Visualize how clusters from Control and Treated samples relate to each other based on marker expression similarity"),
                                    withSpinner(plotOutput("clusterMappingHeatmap", height = "600px")))
                            ),
                            hr(),
                            fluidRow(
                              column(12, h4("Signature Markers by Cluster", align = "center")),
                              column(12, withSpinner(plotOutput("signatureMarkerHeatmap", height = "600px")))
                            )
                          ),
                          conditionalPanel(
                            condition = "!input.showBatchClustering",
                            h3("Clustering must be enabled to use this feature", align = "center", 
                               style = "margin-top: 100px; color: #888;")
                          )
                   )
                 )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # --- GLOBAL PLOT SETTINGS ---
  
  # Reactive values to store plot settings
  plot_settings <- reactiveValues(
    width = 800,
    height = 500,
    font_size = 12,
    point_size = 6,
    color_palette = "viridis"
  )
  
  # Update plot settings when the apply button is clicked
  observeEvent(input$apply_plot_settings, {
    plot_settings$width <- input$global_plot_width
    plot_settings$height <- input$global_plot_height
    plot_settings$font_size <- input$global_font_size
    plot_settings$point_size <- input$global_point_size
    plot_settings$color_palette <- input$global_color_palette
    
    showNotification("Plot settings applied to all visualizations", type = "message")
  })
  
  # Make Raw Data Analysis visualization options affect plots directly
  observe({
    # Only update when explicitly changed in Raw Data tab to avoid conflicts with global settings
    if (!is.null(input$plotWidth) && !is.null(input$plotHeight)) {
      plot_settings$width <- input$plotWidth
      plot_settings$height <- input$plotHeight
    }
  })
  
  # Function to get the active color palette
  get_color_palette <- function() {
    palette_name <- plot_settings$color_palette
    
    switch(palette_name,
           "viridis" = scale_color_viridis_d(),
           "plasma" = scale_color_viridis_d(option = "plasma"),
           "blues" = scale_color_brewer(palette = "Blues"),
           "reds" = scale_color_brewer(palette = "Reds"),
           scale_color_viridis_d()  # Default fallback
    )
  }
  
  # Function to get fill palette
  get_fill_palette <- function() {
    palette_name <- plot_settings$color_palette
    
    switch(palette_name,
           "viridis" = scale_fill_viridis_c(),
           "plasma" = scale_fill_viridis_c(option = "plasma"),
           "blues" = scale_fill_distiller(palette = "Blues", direction = 1),
           "reds" = scale_fill_distiller(palette = "Reds", direction = 1),
           scale_fill_viridis_c()  # Default fallback
    )
  }
  
  # Function to create a standard ggplot theme
  get_standard_theme <- function() {
    theme_minimal(base_size = plot_settings$font_size) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title = element_text(face = "bold"),
        legend.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "grey80", fill = NA)
      )
  }
  
  # Function to create standard tooltips for plotly
  create_standard_tooltip <- function(plot) {
    plot %>% layout(
      hoverlabel = list(
        bgcolor = "white",
        font = list(family = "Arial", size = plot_settings$font_size)
      ),
      height = plot_settings$height,
      width = plot_settings$width
    )
  }
  
  # --- RAW DATA ANALYSIS (from first app) ---
  
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
  
  output$fcsInfo <- renderPrint({
    req(input$fcsFile)
    fcs <- rawFCS()
    cat("File name: ", input$fcsFile$name, "\n")
    cat("Dimensions: ", paste(dim(fcs), collapse = " x "), "\n")
    if (inherits(fcs, "flowFrame")) {
      cat("Parameters:\n")
      print(head(parameters(fcs)[, c("name", "desc")], 20))
    } else {
      cat("Column names:\n")
      print(colnames(fcs))
    }
  })
  
  output$markerSelectUI <- renderUI({
    req(input$fcsFile)
    data <- rawFCS()
    if (inherits(data, "flowFrame")) {
      exprs_data <- exprs(data)
      params <- parameters(data)
      choices <- setNames(colnames(exprs_data), paste0(colnames(exprs_data), " - ", params$desc))
    } else {
      exprs_data <- data
      choices <- colnames(exprs_data)
    }
    
    selectInput("selectedMarkers", "Select Markers/Channels", choices = choices, selected = choices[1:min(5, length(choices))], multiple = TRUE)
  })
  
  # Add optimization metrics UI output
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
    
    # Add recommendation if both methods are available
    if (!is.null(metrics$tsne) && !is.null(metrics$umap)) {
      # Determine which method seems better (simple heuristic)
      tsne_score <- 1 - min(metrics$tsne$kl_divergence, 1)  # Convert to 0-1 scale where higher is better
      umap_score <- max(min(metrics$umap$trustworthiness, 1), 0)  # Ensure in 0-1 range
      
      recommended <- if (tsne_score > umap_score) "t-SNE" else "UMAP"
      
      metrics_ui <- tagAppendChildren(
        metrics_ui,
        div(
          h5("Recommendation:"),
          p(paste("Based on calculated metrics, ", recommended, " may provide better results for this dataset."))
        )
      )
    }
    
    # Add preprocessing metrics
    if (!is.null(results$metrics)) {
      metrics_ui <- tagAppendChildren(
        metrics_ui,
        hr(),
        h4("Preprocessing Metrics"),
        div(
          h5("Quality Control:"),
          p(paste("Initial cells:", results$metrics$qc$initial_count)),
          p(paste("After QC:", results$metrics$qc$final_count)),
          p(paste("Removed:", round(results$metrics$qc$removed_pct * 100, 1), "%"))
        ),
        div(
          h5("Gating:"),
          p(paste("After gating:", results$metrics$gating$final_count)),
          p(paste("Removed in gating:", round(results$metrics$gating$removed_pct * 100, 1), "%"))
        ),
        div(
          h5("Sampling:"),
          p(paste("Final analyzed cells:", nrow(results$sampled_data)))
        )
      )
    }
    
    return(metrics_ui)
  })
  
  observe({
    req(input$selectedMarkers)
    markers <- input$selectedMarkers
    updateSelectInput(session, "colorBy", choices = c("None", "Cluster", markers))
  })
  
  processedData <- reactiveVal(NULL)
  
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
        perform_gating = isTRUE(input$performGating),
        scale_data = TRUE,
        seed = 123
      )
      
      # Add QC settings if QC is enabled
      if (isTRUE(input$performQC)) {
        preprocessing_params$qc_settings <- list(
          max_anomalies = input$maxAnomalies / 100  # Convert from percentage to proportion
        )
      }
      
      # Add gating parameters if gating is enabled
      if (isTRUE(input$performGating)) {
        # Parse debris gate parameters from comma-separated string
        debris_gate_params <- unlist(strsplit(input$debrisGate, ",\\s*"))
        
        preprocessing_params$gates <- list(
          debris_gate = if (length(debris_gate_params) >= 2) debris_gate_params[1:2] else NULL,
          live_dead_gate = if (!is.null(input$liveDeadGate) && input$liveDeadGate != "None") input$liveDeadGate else NULL
        )
      }
      
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
      }
      
      if ("UMAP" %in% input$methods) {
        incProgress(0.7, detail = "Running UMAP...")
        umap_result <- umap(preprocessing_results$scaled_data, n_neighbors = input$n_neighbors)
        results$umap <- data.frame(umap1 = umap_result[,1], umap2 = umap_result[,2])
      }
      
      # Create plot data
      plot_data <- as.data.frame(preprocessing_results$sampled_data)
      colnames(plot_data) <- input$selectedMarkers
      
      # Add dimensionality reduction coordinates
      if (!is.null(results$tsne)) {
        plot_data$tsne1 <- results$tsne$tsne1
        plot_data$tsne2 <- results$tsne$tsne2
      }
      if (!is.null(results$umap)) {
        plot_data$umap1 <- results$umap$umap1
        plot_data$umap2 <- results$umap$umap2
      }
      
      results$plot_data <- plot_data
      processedData(results)
      
      # Reset clustering and gates when new data is loaded
      clustering_results(NULL)
      
      # Display notification about preprocessing results
      if (!is.null(results$metrics)) {
        qc_removed <- round(results$metrics$qc$removed_pct * 100, 1)
        gating_removed <- round(results$metrics$gating$removed_pct * 100, 1)
        
        msg <- paste0("Preprocessing complete: ", 
                      qc_removed, "% removed in QC, ",
                      gating_removed, "% removed in gating. ",
                      "Analyzing ", nrow(results$sampled_data), " cells.")
        
        showNotification(msg, type = "message", duration = 5)
      }
    })
  })
  
  # --- CLUSTERING FUNCTIONALITY ---
  
  clustering_results <- reactiveVal(NULL)
  
  observeEvent(input$runClustering, {
    req(processedData(), input$selectedMarkers)
    
    withProgress(message = 'Clustering cells...', value = 0, {
      plot_data <- processedData()$plot_data
      # Extract just the marker columns for clustering
      marker_data <- plot_data[, input$selectedMarkers, drop = FALSE]
      
      # Run the selected clustering algorithm
      if (input$clusterMethod == "K-means") {
        set.seed(123)  # For reproducibility
        incProgress(0.3, detail = "Running K-means clustering...")
        km <- kmeans(marker_data, centers = input$numClusters, nstart = 10)
        cluster_ids <- km$cluster
        
        # Store cluster centers for intensity profiles
        centers <- km$centers
        colnames(centers) <- input$selectedMarkers
        
        # Create results object
        results <- list(
          cluster_ids = cluster_ids,
          centers = centers,
          method = "K-means"
        )
        
        clustering_results(results)
      }
      else if (input$clusterMethod == "DBSCAN") {
        incProgress(0.3, detail = "Running DBSCAN clustering...")
        # Scale data for DBSCAN
        scaled_markers <- scale(marker_data)
        
        # Run DBSCAN
        dbscan_result <- dbscan::dbscan(scaled_markers, eps = input$dbscan_eps, minPts = input$dbscan_minPts)
        cluster_ids <- dbscan_result$cluster
        
        # Calculate cluster centers (mean of each cluster)
        unique_clusters <- sort(unique(cluster_ids))
        centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(marker_data))
        
        for (i in seq_along(unique_clusters)) {
          cluster_idx <- which(cluster_ids == unique_clusters[i])
          if (length(cluster_idx) > 0) {
            centers[i,] <- colMeans(marker_data[cluster_idx, , drop = FALSE])
          }
        }
        
        colnames(centers) <- input$selectedMarkers
        rownames(centers) <- paste("Cluster", unique_clusters)
        
        # Create results object
        results <- list(
          cluster_ids = cluster_ids,
          centers = centers,
          method = "DBSCAN"
        )
        
        clustering_results(results)
      }
      else if (input$clusterMethod == "FlowSOM") {
        incProgress(0.3, detail = "Running FlowSOM clustering...")
        
        tryCatch({
          # Validate dimensions to ensure they are numeric and >= 2
          xdim <- max(2, as.numeric(input$som_xdim))
          ydim <- max(2, as.numeric(input$som_ydim))
          
          # Validate number of metaclusters
          n_metaclusters <- max(2, min(30, as.numeric(input$som_clusters)))
          
          # Create FlowSOM object
          # First, create a flowFrame from the matrix with proper marker names
          marker_matrix <- as.matrix(marker_data)
          colnames(marker_matrix) <- input$selectedMarkers
          fcs_data <- flowCore::flowFrame(marker_matrix)
          
          # Set proper column names in the flowFrame parameters
          params <- flowCore::parameters(fcs_data)
          params$name <- input$selectedMarkers
          params$desc <- input$selectedMarkers
          flowCore::parameters(fcs_data) <- params
          
          # Use ReadInput to properly prepare the data for FlowSOM
          fsom_input <- FlowSOM::ReadInput(fcs_data, transform = FALSE, scale = FALSE)
          
          # Now build the SOM with validated parameters
          showNotification(
            sprintf("Building SOM grid with dimensions %d x %d...", xdim, ydim),
            type = "message",
            duration = 3
          )
          
          # Build the SOM with explicit validations
          fsom <- FlowSOM::BuildSOM(
            fsom_input, 
            colsToUse = NULL,  # Use all columns
            xdim = xdim,       # Validated x dimension
            ydim = ydim,       # Validated y dimension
            rlen = 12,         # Default training length
            silent = FALSE     # Show progress
          )
          
          # Get metaclusters
          showNotification(
            sprintf("Creating %d metaclusters...", n_metaclusters),
            type = "message", 
            duration = 3
          )
          
          metacl <- FlowSOM::MetaClustering(
            fsom$map$codes, 
            method = "metaClustering_consensus",
            max = n_metaclusters
          )
          
          # Get cluster IDs for each cell
          # First map to SOM clusters
          cell_som_clusters <- fsom$map$mapping[,1]
          # Then map to metaclusters
          cell_metaclusters <- metacl[cell_som_clusters]
          
          # Calculate metacluster centers (mean of each cluster)
          unique_clusters <- sort(unique(cell_metaclusters))
          centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(marker_data))
          
          for (i in seq_along(unique_clusters)) {
            cluster_idx <- which(cell_metaclusters == unique_clusters[i])
            if (length(cluster_idx) > 0) {
              centers[i,] <- colMeans(marker_data[cluster_idx, , drop = FALSE])
            }
          }
          
          colnames(centers) <- input$selectedMarkers
          rownames(centers) <- paste("Cluster", unique_clusters)
          
          # Create results object
          results <- list(
            cluster_ids = cell_metaclusters,
            centers = centers,
            som_object = fsom,
            som_clusters = cell_som_clusters,
            metaclusters = metacl,
            method = "FlowSOM"
          )
          
          clustering_results(results)
          showNotification("FlowSOM clustering completed successfully", type = "message", duration = 3)
        }, error = function(e) {
          # Show detailed error message
          showNotification(
            paste("FlowSOM error:", e$message, 
                  "Try adjusting SOM parameters or selecting different markers."),
            type = "error",
            duration = 10
          )
        })
      }
      else if (input$clusterMethod == "Phenograph") {
        incProgress(0.3, detail = "Running Phenograph clustering...")
        
        tryCatch({
          # Run Phenograph
          pheno_result <- Rphenograph(as.matrix(marker_data), k = input$phenoK)
          
          # Get cluster IDs
          cluster_ids <- as.numeric(membership(pheno_result[[2]]))
          
          # Calculate cluster centers (mean of each cluster)
          unique_clusters <- sort(unique(cluster_ids))
          centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(marker_data))
          
          for (i in seq_along(unique_clusters)) {
            cluster_idx <- which(cluster_ids == unique_clusters[i])
            if (length(cluster_idx) > 0) {
              centers[i,] <- colMeans(marker_data[cluster_idx, , drop = FALSE])
            }
          }
          
          colnames(centers) <- input$selectedMarkers
          rownames(centers) <- paste("Cluster", unique_clusters)
          
          # Create results object
          results <- list(
            cluster_ids = cluster_ids,
            centers = centers,
            pheno_object = pheno_result,
            method = "Phenograph"
          )
          
          clustering_results(results)
        }, error = function(e) {
          # Show error message to user
          showNotification(
            paste("Phenograph error:", e$message, 
                  "Try adjusting k parameter or selecting different markers."),
            type = "error",
            duration = 10
          )
        })
      }
      # Add other clustering methods here
    })
  })
  
  # Update plot outputs to show clusters
  observe({
    req(clustering_results())
    updateSelectInput(session, "colorBy", choices = c("None", "Cluster", input$selectedMarkers), selected = "Cluster")
  })
  
  output$tsnePlot <- renderPlotly({
    req(processedData(), "t-SNE" %in% input$methods)
    plot_data <- processedData()$plot_data
    req("tsne1" %in% colnames(plot_data))
    
    # Create a copy of plot_data to avoid modifying the reactive value
    plot_data_copy <- plot_data
    
    # Add row names if missing
    if (is.null(rownames(plot_data_copy))) {
      rownames(plot_data_copy) <- paste0("Cell_", 1:nrow(plot_data_copy))
    }
    
    # Ensure colorBy input is valid
    if (is.null(input$colorBy)) {
      colorBy <- "None"
    } else {
      colorBy <- input$colorBy
    }
    
    # If clustering results exist, add them to the plot data
    if (!is.null(clustering_results()) && colorBy == "Cluster") {
      plot_data_copy$Cluster <- as.factor(clustering_results()$cluster_ids)
      
      # First create a ggplot object
      p <- ggplot(plot_data_copy, aes(x = tsne1, y = tsne2, color = Cluster)) +
        geom_point(alpha = 0.7, size = plot_settings$point_size/2) +
        get_color_palette() +
        labs(
          title = "t-SNE Projection",
          x = "t-SNE 1", 
          y = "t-SNE 2", 
          color = "Cluster"
        ) +
        get_standard_theme()
      
    } else if (!is.null(colorBy) && colorBy != "None" && colorBy != "Cluster" && 
               colorBy %in% colnames(plot_data_copy)) {
      p <- ggplot(plot_data_copy, aes(x = tsne1, y = tsne2)) +
        geom_point(aes(color = .data[[colorBy]]), 
                 alpha = 0.7, size = plot_settings$point_size/2) +
        scale_color_viridis_c() +
        labs(
          title = "t-SNE Projection",
          x = "t-SNE 1", 
          y = "t-SNE 2", 
          color = colorBy
        ) +
        get_standard_theme()
    } else {
      # Default: No coloring
      p <- ggplot(plot_data_copy, aes(x = tsne1, y = tsne2)) +
        geom_point(alpha = 0.7, size = plot_settings$point_size/2, color = "#3366CC") +
        labs(
          title = "t-SNE Projection", 
          x = "t-SNE 1", 
          y = "t-SNE 2"
        ) +
        get_standard_theme()
    }
    
    # Create basic tooltips
    tooltip_info <- paste0("Index: ", rownames(plot_data_copy),
                          "<br>t-SNE 1: ", round(plot_data_copy$tsne1, 2),
                          "<br>t-SNE 2: ", round(plot_data_copy$tsne2, 2))
    
    # Convert to plotly with proper dimensions
    p <- ggplotly(p, width = plot_settings$width, height = plot_settings$height) 
    
    # Apply additional styling
    p %>% layout(
      hoverlabel = list(
        bgcolor = "white",
        font = list(family = "Arial", size = plot_settings$font_size)
      )
    )
  })
  
  output$umapPlot <- renderPlotly({
    req(processedData(), "UMAP" %in% input$methods)
    plot_data <- processedData()$plot_data
    req("umap1" %in% colnames(plot_data))
    
    # Create a copy of plot_data to avoid modifying the reactive value
    plot_data_copy <- plot_data
    
    # If clustering results exist, add them to the plot data
    if (!is.null(clustering_results()) && input$colorBy == "Cluster") {
      plot_data_copy$Cluster <- as.factor(clustering_results()$cluster_ids)
      
      p <- ggplot(plot_data_copy, aes(x = umap1, y = umap2, color = Cluster, 
                                      text = paste("Cluster:", Cluster))) +
        geom_point(alpha = 0.7) +
        scale_color_viridis_d() +
        labs(color = "Cluster")
    } else if (!is.null(input$colorBy) && input$colorBy != "None" && input$colorBy != "Cluster") {
      p <- ggplot(plot_data_copy, aes(x = umap1, y = umap2)) +
        geom_point(aes(color = .data[[input$colorBy]], 
                       text = paste("Value:", .data[[input$colorBy]])), alpha = 0.7) +
        scale_color_viridis_c() +
        labs(color = input$colorBy)
    } else {
      p <- ggplot(plot_data_copy, aes(x = umap1, y = umap2)) +
        geom_point(aes(text = paste("Index:", rownames(plot_data_copy))), alpha = 0.7)
    }

    p <- p + labs(title = "UMAP Projection", x = "UMAP 1", y = "UMAP 2") +
      theme_minimal(base_size = 16) +
      theme(axis.title = element_text(size = 18), axis.text = element_text(size = 14))
    
    ggplotly(p, tooltip = "text") %>% 
      layout(
        height = plot_settings$height, 
        width = plot_settings$width,
        hoverlabel = list(
          bgcolor = "white",
          font = list(family = "Arial", size = plot_settings$font_size)
        )
      )
  })
  
  # Cluster visualization in dedicated tab
  output$clusterPlot <- renderPlotly({
    req(processedData(), clustering_results())
    
    # Check what dimension reduction method is available
    plot_data <- processedData()$plot_data
    
    # Choose appropriate dimensions
    if ("tsne1" %in% colnames(plot_data)) {
      dim1 <- "tsne1"
      dim2 <- "tsne2"
      dim_labels <- c("t-SNE 1", "t-SNE 2")
    } else if ("umap1" %in% colnames(plot_data)) {
      dim1 <- "umap1"
      dim2 <- "umap2"
      dim_labels <- c("UMAP 1", "UMAP 2")
    } else {
      # Fallback to first two markers if no dimension reduction available
      dim1 <- input$selectedMarkers[1]
      dim2 <- input$selectedMarkers[2]
      dim_labels <- input$selectedMarkers[1:2]
    }
    
    # Add cluster information
    plot_data$Cluster <- as.factor(clustering_results()$cluster_ids)
    
    # Create plot
    p <- plot_ly(plot_data, 
                 x = ~.data[[dim1]], 
                 y = ~.data[[dim2]],
                 color = ~Cluster, 
                 type = "scatter", 
                 mode = "markers",
                 marker = list(size = 8),
                 text = ~paste("Cluster:", Cluster, 
                               "<br>", dim_labels[1], ":", .data[[dim1]], 
                               "<br>", dim_labels[2], ":", .data[[dim2]])) %>%
      layout(title = paste("Clusters from", clustering_results()$method),
             xaxis = list(title = dim_labels[1]),
             yaxis = list(title = dim_labels[2]),
             height = plot_settings$height,
             width = plot_settings$width)
    
    p
  })
  
  # Cluster heatmap
  output$clusterHeatmap <- renderPlot(
    {
      req(clustering_results())
      centers <- clustering_results()$centers
      df <- as.data.frame(centers) %>%
        tibble::rownames_to_column("Cluster") %>%
        reshape2::melt(id.vars = "Cluster", variable.name = "Marker", value.name = "Expression")
      
      ggplot(df, aes(x = Marker, y = Cluster, fill = Expression)) +
        geom_tile() +
        scale_fill_gradient2(low = "navy", mid = "white", high = "firebrick3") +
        labs(
          title = paste("Cluster Intensity Profiles from", clustering_results()$method),
          x = "Markers", y = "Clusters",
          fill = "Expression"
        ) +
        get_standard_theme() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text.y = element_text(face = "bold")
        )
    },
    width = function() plot_settings$width,
    height = function() plot_settings$height
  )
  
  # Cluster statistics
  output$clusterStats <- DT::renderDataTable({
    req(clustering_results(), processedData())
    
    plot_data <- processedData()$plot_data
    plot_data$Cluster <- as.factor(clustering_results()$cluster_ids)
    
    # Calculate basic statistics for each cluster
    cluster_stats <- plot_data %>%
      group_by(Cluster) %>%
      summarize(
        Count = n(),
        Percentage = n() / nrow(plot_data) * 100,
        across(all_of(input$selectedMarkers), 
               list(Mean = ~mean(., na.rm = TRUE), 
                    Median = ~median(., na.rm = TRUE),
                    SD = ~sd(., na.rm = TRUE)))
      )
    
    DT::datatable(cluster_stats, 
                  options = list(scrollX = TRUE, pageLength = 5),
                  caption = paste("Cluster statistics from", clustering_results()$method))
  })

  # --- CELL POPULATION IDENTIFICATION ---
  
  identified_populations <- reactiveVal(NULL)
  
  # Add at the start of your identify_cell_populations function
  # Manual mapping for the specific markers in your dataset
  manual_mapping <- list(
    # "FL5-A - CD3 FITC FITC-A" = "CD3",
    # "FL13-A - CD4 APCCy7 APC-A750-A" = "CD4",
    # "FL11-A - MHC II AF647 APC-A" = "MHCII",
    # "FL12-A - Foxp3 A700 APC-A700-A" = "FOXP3",
    # "FL1-A - CXCR5 BV421 PB450-A" = "CXCR5",
    # "FL2-A - CX3 BV510 KO525-A" = "CX3",
    # "FL4-A - IgG2 BV711 V5-711-A" = "IGG2",
    # "FL7-A - BCI6 PE PE-A" = "BCI6",
    # "FL8-A - IgG AF594 ECD-A" = "IGG",
    # "FL10-A - Ki67 PECy7 PC7-A" = "KI67"
    "FL5-A" = "CD3",
    "FL13-A" = "CD4",
    "FL11-A" = "MHCII",
    "FL12-A" = "FOXP3",
    "FL1-A" = "CXCR5",
    "FL2-A" = "CX3",
    "FL4-A" = "IGG2",
    "FL7-A" = "BCI6",
    "FL8-A" = "IGG",
    "FL10-A" = "KI67"
  )
  
  # Then use this mapping directly in your template matching
  # Define common cell population reference profiles based on marker expression patterns
  cell_population_templates <- list(
    "CD4+ T cells" = list(
      high = c("CD3", "CD4"),
      low = c("CD8", "CD19")
    ),
    "CD8+ T cells" = list(
      high = c("CD3", "CD8"),
      low = c("CD4", "CD19")
    ),
    "B cells" = list(
      high = c("CD19", "CD20"),
      low = c("CD3", "CD56")
    ),
    "NK cells" = list(
      high = c("CD56"),
      low = c("CD3", "CD19")
    ),
    "Regulatory T cells" = list(
      high = c("CD3", "CD4", "FOXP3"),
      low = c("CD8")
    ),
    "Helper T cells" = list(
      high = c("CD3", "CD4"),
      low = c("CD8", "FOXP3")
    ),
    "Monocytes" = list(
      high = c("CD14"),
      low = c("CD3", "CD19")
    ),
    "Dendritic cells" = list(
      high = c("CD11c", "HLA-DR"),
      low = c("CD3", "CD19", "CD14")
    )
  )
  
  # Function to identify cell populations based on marker expression patterns
  identify_cell_populations <- function(cluster_centers, marker_names, 
                                        high_threshold = 0.3,  # Lowered from 0.5
                                        low_threshold = -0.3,  # Raised from -0.5
                                        min_confidence = 0.2) { # Lowered from 0.3
    
    # Create results dataframe
    results <- data.frame(
      Cluster = 1:nrow(cluster_centers),
      Population = rep("Unknown", nrow(cluster_centers)),
      Confidence = rep(0, nrow(cluster_centers)),
      MatchDetails = rep("", nrow(cluster_centers)),
      stringsAsFactors = FALSE
    )
    
    # Define cell population templates based on this panel
    cell_population_templates <- list(
      "CD4+ T cells" = list(
        high = c("CD3", "CD4"),
        medium = c(),
        low = c("FOXP3", "BCL6")
      ),
      "CD8+ T cells (inferred)" = list(
        high = c("CD3"),
        medium = c(),
        low = c("CD4", "FOXP3", "BCL6")
      ),
      "Regulatory T cells" = list(
        high = c("CD3", "CD4", "FOXP3"),
        medium = c(),
        low = c("BCL6", "CXCR5")
      ),
      "T follicular helper cells" = list(
        high = c("CD3", "CD4", "CXCR5", "BCL6"),
        medium = c(),
        low = c("FOXP3")
      ),
      "MHCII+ APCs" = list(
        high = c("MHCII"),
        medium = c(),
        low = c("CD3", "CD4")
      ),
      "Proliferating T cells" = list(
        high = c("CD3", "KI67"),
        medium = c(),
        low = c()
      ),
      "Proliferating Tregs" = list(
        high = c("CD3", "CD4", "FOXP3", "KI67"),
        medium = c(),
        low = c("BCL6", "CXCR5")
      ),
      "Proliferating Tfh cells" = list(
        high = c("CD3", "CD4", "CXCR5", "BCL6", "KI67"),
        medium = c(),
        low = c("FOXP3")
      ),
      "Activated T cells" = list(
        high = c("CD3", "MHCII"),
        medium = c(),
        low = c()
      )
    )
    
    # Function to clean and standardize marker names
    clean_marker_name <- function(name) {
      # Convert to uppercase
      name <- toupper(name)
      
      # Remove common prefixes/suffixes and special characters
      name <- gsub("FL[0-9]+-[A-Z]+ *- *", "", name)
      name <- gsub(" +[A-Z0-9]+-[A-Z0-9]+-[A-Z0-9]+$", "", name) # Remove suffixes like APC-A750-A
      name <- gsub(" +[A-Z0-9]+-[A-Z0-9]+$", "", name)  # Remove suffixes like PE-A
      name <- gsub(" +[A-Z]+$", "", name)  # Remove single suffix like A
      name <- gsub("[-_\\s]+", "", name)   # Remove spaces, hyphens, underscores
      
      # Special cases for your panel
      name <- gsub("^MHCII$", "MHCII", name)
      name <- gsub("^KI-?67$", "KI67", name)
      name <- gsub("^FOXP3$", "FOXP3", name)
      name <- gsub("^BCL6$", "BCL6", name)
      
      return(name)
    }
    
    # Manual mapping for the specific markers in your dataset
    manual_mapping <- list(
      # Original channel names
      "FL5-A - CD3 FITC FITC-A" = "CD3",
      "FL13-A - CD4 APCCy7 APC-A750-A" = "CD4",
      "FL11-A - MHC II AF647 APC-A" = "MHCII",
      "FL12-A - Foxp3 A700 APC-A700-A" = "FOXP3",
      "FL1-A - CXCR5 BV421 PB450-A" = "CXCR5",
      "FL7-A - BCl6 PE PE-A" = "BCL6",
      "FL10-A - Ki67 PECy7 PC7-A" = "KI67",
      "FL3-A - Live Dead BV570 Violet610-A" = "LIVEDEAD",
      
      # Channel numbers alone
      "FL5-A" = "CD3",
      "FL13-A" = "CD4",
      "FL11-A" = "MHCII",
      "FL12-A" = "FOXP3",
      "FL1-A" = "CXCR5",
      "FL7-A" = "BCL6", 
      "FL10-A" = "KI67",
      "FL3-A" = "LIVEDEAD",
      
      # Direct marker names
      "CD3" = "CD3",
      "CD4" = "CD4",
      "MHC" = "MHCII",
      "FOXP3" = "FOXP3",
      "CXCR5" = "CXCR5",
      "BCL6" = "BCL6",
      "KI67" = "KI67",
      "DEAD" = "LIVEDEAD"
    )
    
    # Apply standardization to marker names
    normalized_markers <- sapply(marker_names, clean_marker_name)
    
    # Print diagnostics - will help with debugging
    print("Normalized markers: ")
    print(normalized_markers)
    print("Original markers: ")
    print(marker_names)
    
    # Direct map markers using our manual mapping
    mapped_markers <- character(length(marker_names))
    for (i in 1:length(marker_names)) {
      # Try exact match first
      if (marker_names[i] %in% names(manual_mapping)) {
        mapped_markers[i] <- manual_mapping[[marker_names[i]]]
      } 
      # Try normalized match
      else if (normalized_markers[i] %in% names(manual_mapping)) {
        mapped_markers[i] <- manual_mapping[[normalized_markers[i]]]
      }
      # Try partial matching as a fallback
      else {
        for (pattern in names(manual_mapping)) {
          if (grepl(pattern, marker_names[i], ignore.case = TRUE)) {
            mapped_markers[i] <- manual_mapping[[pattern]]
            break
          }
        }
      }
      
      # If still not mapped, use normalized name
      if (is.null(mapped_markers[i]) || mapped_markers[i] == "") {
        mapped_markers[i] <- normalized_markers[i]
      }
    }
    
    # Debug output of mapping
    print("Mapped markers: ")
    print(mapped_markers)
    
    # Calculate z-scores for all markers across clusters
    z_scores <- apply(cluster_centers, 2, function(x) {
      if (sd(x, na.rm=TRUE) == 0) return(rep(0, length(x)))
      return((x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE))
    })
    
    # Add column names back to z-scores matrix
    colnames(z_scores) <- colnames(cluster_centers)
    
    # For each cluster, compare expression profile to templates
    for (i in 1:nrow(cluster_centers)) {
      best_match <- "Unknown"
      best_score <- 0
      best_details <- ""
      match_scores <- list()
      
      # Compare with each reference profile
      for (pop_name in names(cell_population_templates)) {
        template <- cell_population_templates[[pop_name]]
        score <- 0
        total_markers <- 0
        matching_details <- c()
        
        # Weight factors for different marker categories
        high_weight <- 1.0
        medium_weight <- 0.7
        low_weight <- 0.5
        
        # Track matched markers for logging
        matched_high <- c()
        matched_medium <- c()
        matched_low <- c()
        
        # Check for high-expression markers
        for (marker in template$high) {
          # Find index of this marker in our mapped list
          matching_indices <- which(mapped_markers == marker)
          
          if (length(matching_indices) > 0) {
            total_markers <- total_markers + high_weight
            col_idx <- matching_indices[1]  # Take first match if multiple
            marker_z <- z_scores[i, col_idx]
            
            if (!is.na(marker_z) && marker_z > high_threshold) {
              score <- score + high_weight
              matched_high <- c(matched_high, marker_names[col_idx])
              matching_details <- c(matching_details, paste0(marker_names[col_idx], "(+)"))
            }
          }
        }
        
        # Check for medium-expression markers
        for (marker in template$medium) {
          matching_indices <- which(mapped_markers == marker)
          
          if (length(matching_indices) > 0) {
            total_markers <- total_markers + medium_weight
            col_idx <- matching_indices[1]
            marker_z <- z_scores[i, col_idx]
            
            if (!is.na(marker_z) && marker_z > -0.2 && marker_z < 0.7) {  # Medium expression range
              score <- score + medium_weight
              matched_medium <- c(matched_medium, marker_names[col_idx])
              matching_details <- c(matching_details, paste0(marker_names[col_idx], "(~)"))
            }
          }
        }
        
        # Check for low-expression markers
        for (marker in template$low) {
          matching_indices <- which(mapped_markers == marker)
          
          if (length(matching_indices) > 0) {
            total_markers <- total_markers + low_weight
            col_idx <- matching_indices[1]
            marker_z <- z_scores[i, col_idx]
            
            if (!is.na(marker_z) && marker_z < low_threshold) {
              score <- score + low_weight
              matched_low <- c(matched_low, marker_names[col_idx])
              matching_details <- c(matching_details, paste0(marker_names[col_idx], "(-)"))
            }
          }
        }
        
        # Calculate weighted confidence score
        if (total_markers > 0) {
          confidence <- score / total_markers
          
          # Log match details for debugging
          log_text <- paste0("Cluster ", i, " vs ", pop_name, 
                             ": matched ", length(matched_high), "/", length(template$high), " high markers, ",
                             length(matched_medium), "/", length(template$medium), " medium markers, ",
                             length(matched_low), "/", length(template$low), " low markers. ",
                             "Confidence: ", round(confidence * 100, 1), "%")
          
          print(log_text)  # Add logging
          match_scores[[pop_name]] <- confidence
          
          # If this population is a better match than previous ones
          if (confidence > best_score && confidence > min_confidence) {
            best_score <- confidence
            best_match <- pop_name
            best_details <- paste(matching_details, collapse = ", ")
          }
        }
      }
      
      # Assign the best matching population
      results$Population[i] <- best_match
      results$Confidence[i] <- best_score * 100  # Convert to percentage
      results$MatchDetails[i] <- best_details
      
      # Log all match scores for this cluster
      print(paste("Cluster", i, "match scores:"))
      print(match_scores)
    }
    
    return(results)
  }
  
  # Handle the population identification button click
  observeEvent(input$identifyPopulations, {
    req(clustering_results())
    
    withProgress(message = 'Identifying cell populations...', value = 0, {
      # Get cluster centers
      centers <- clustering_results()$centers
      marker_names <- colnames(centers)
      
      # Run identification algorithm with user-configured thresholds
      populations <- identify_cell_populations(
        centers, 
        marker_names,
        high_threshold = input$highExpressionThreshold,
        low_threshold = input$lowExpressionThreshold,
        min_confidence = input$minConfidenceThreshold/100  # Convert from percentage
      )
      
      # Store results
      identified_populations(populations)
      
      # Show notification
      showNotification("Cell populations identified based on marker expression patterns", type = "message")
    })
  })
  
  # Population results table
  output$populationTable <- DT::renderDataTable({
    req(identified_populations())
    
    population_info <- identified_populations()
    
    # Add cluster sizes if clustering results exist
    if (!is.null(clustering_results())) {
      cluster_counts <- table(clustering_results()$cluster_ids)
      total_cells <- sum(cluster_counts)
      
      result_table <- data.frame(
        Cluster = population_info$Cluster,
        Population = population_info$Population,
        Count = as.numeric(cluster_counts[population_info$Cluster]),
        Percentage = round(as.numeric(cluster_counts[population_info$Cluster]) / total_cells * 100, 2),
        Confidence = round(population_info$Confidence, 1),
        MatchDetails = population_info$MatchDetails
      )
      
      # Sort by population name then by cluster number
      result_table <- result_table[order(result_table$Population, result_table$Cluster), ]
      
      DT::datatable(
        result_table,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        caption = "Identified Cell Populations"
      )
    }
  })
  
  # Update cluster visualization to include population labels
  output$clusterPlot <- renderPlotly({
    req(processedData(), clustering_results())
    
    # Check what dimension reduction method is available
    plot_data <- processedData()$plot_data
    
    # Choose appropriate dimensions
    if ("tsne1" %in% colnames(plot_data)) {
      dim1 <- "tsne1"
      dim2 <- "tsne2"
      dim_labels <- c("t-SNE 1", "t-SNE 2")
    } else if ("umap1" %in% colnames(plot_data)) {
      dim1 <- "umap1"
      dim2 <- "umap2"
      dim_labels <- c("UMAP 1", "UMAP 2")
    } else {
      # Fallback to first two markers if no dimension reduction available
      dim1 <- input$selectedMarkers[1]
      dim2 <- input$selectedMarkers[2]
      dim_labels <- input$selectedMarkers[1:2]
    }
    
    # Add cluster information
    plot_data$Cluster <- as.factor(clustering_results()$cluster_ids)
    
    # Add population labels if available
    if (!is.null(identified_populations()) && input$showPopulationLabels) {
      pop_data <- identified_populations()
      populations <- pop_data$Population[plot_data$Cluster]
      plot_data$Population <- populations
      
      hover_text <- paste(
        "Cluster:", plot_data$Cluster,
        "<br>Population:", plot_data$Population,
        "<br>", dim_labels[1], ":", plot_data[[dim1]],
        "<br>", dim_labels[2], ":", plot_data[[dim2]]
      )
    } else {
      hover_text <- paste(
        "Cluster:", plot_data$Cluster,
        "<br>", dim_labels[1], ":", plot_data[[dim1]],
        "<br>", dim_labels[2], ":", plot_data[[dim2]]
      )
    }
    
    # Create plot
    p <- plot_ly(plot_data, 
                 x = ~.data[[dim1]], 
                 y = ~.data[[dim2]],
                 color = ~Cluster, 
                 type = "scatter", 
                 mode = "markers",
                 marker = list(size = 8, opacity = 0.95),
                 text = hover_text) %>%
      layout(title = paste("Clusters from", clustering_results()$method),
             xaxis = list(title = dim_labels[1]),
             yaxis = list(title = dim_labels[2]),
             height = plot_settings$height,
             width = plot_settings$width)
    
    # Add cluster labels with population names if available
    if (!is.null(identified_populations()) && input$showPopulationLabels) {
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
        p <- p %>% add_annotations(
          x = cluster_centers$x[i],
          y = cluster_centers$y[i],
          text = paste0("Cluster ", cluster_centers$Cluster[i], ": ", cluster_centers$Population[i]),
          showarrow = TRUE,
          arrowhead = 0.5,
          arrowsize = 0.5,
          arrowwidth = 1,
          ax = 20,
          ay = -20,
          bgcolor = "rgba(255, 255, 255, 0.8)",
          bordercolor = "rgba(0, 0, 0, 0.5)",
          borderwidth = 1,
          font = list(size = 12)
        )
      }
    }
    
    return(p)
  })
  
  # --- CLEANED DATA ANALYSIS (from second app) ---
  
  cleaned_data <- reactive({
    req(input$cleanedFile)
    ext <- tools::file_ext(input$cleanedFile$name)
    switch(ext,
           csv = read_csv(input$cleanedFile$datapath),
           tsv = read_tsv(input$cleanedFile$datapath),
           xlsx = read.xlsx(input$cleanedFile$datapath),
           validate("Unsupported file format"))
  })


  # --- CLEANED DATA ANALYSIS CODE ---
  
  markers_cleaned <- reactive({
    req(cleaned_data())
    grep("^[48]", colnames(cleaned_data()), value = TRUE)
  })
  
  output$preview <- renderDT({
    req(cleaned_data())
    datatable(head(cleaned_data(), 10), options = list(scrollX = TRUE))
  })
  
  output$structure <- renderPrint({
    req(cleaned_data())
    list(
      Columns = colnames(cleaned_data()),
      CD4 = grep("^4", colnames(cleaned_data()), value = TRUE),
      CD8 = grep("^8", colnames(cleaned_data()), value = TRUE)
    )
  })
  
  output$analysis_type_ui <- renderUI({
    req(input$cleanedFile)
    selectInput("analysis_type", "Select Analysis Type", 
                choices = c("Marker Comparison", "Treatment Comparison", 
                            "Fold Change", "Summary Statistics"))
  })
  
  output$marker_ui <- renderUI({
    req(markers_cleaned())
    pickerInput("selected_markers", "Select Markers", choices = markers_cleaned(), 
                multiple = TRUE, selected = markers_cleaned()[1], options = list(`actions-box` = TRUE))
  })
  
  output$treatment_ui <- renderUI({
    req(cleaned_data())
    selectInput("selected_treatment", "Select Treatment Column", 
                choices = names(cleaned_data())[sapply(cleaned_data(), is.character)])
  })
  
  output$plot <- renderPlotly({
    req(input$run_cleaned, cleaned_data(), input$selected_markers, input$selected_treatment)
    df <- cleaned_data()
    df_long <- pivot_longer(df, cols = all_of(input$selected_markers),
                            names_to = "Marker", values_to = "Value")
    
    p <- ggplot(df_long, aes_string(x = input$selected_treatment, y = "Value", color = "Marker")) +
      geom_point(position = position_jitter(width = 0.2), alpha = 0.8, size = 3.5) +
      theme_minimal(base_size = 16) +
      theme(axis.title = element_text(size = 18),
            axis.text  = element_text(size = 15),
            legend.title = element_text(size = 16),
            legend.text  = element_text(size = 14))
    
    ggplotly(p)
  })
  
  output$dimred_plot <- renderPlotly({
    req(input$run_cleaned, cleaned_data(), input$selected_markers, input$selected_treatment)
    df <- cleaned_data()
    df_sel <- df[, input$selected_markers, drop = FALSE] %>%
      mutate(across(everything(), as.numeric)) %>%
      na.omit()
    
    if (nrow(df_sel) < 3) {
      showNotification("Not enough data for dimensionality reduction.", type = "error")
      return(NULL)
    }
    
    treatment <- df[as.numeric(rownames(df_sel)), input$selected_treatment]
    sample_ids <- if ("Sample" %in% colnames(df)) df[as.numeric(rownames(df_sel)), "Sample"] else paste("Sample", seq_len(nrow(df_sel)))
    
    set.seed(42)  # Ensure reproducibility
    dimred <- switch(input$dimred_method,
                     "t-SNE" = Rtsne(df_sel, perplexity = input$perplexity_cleaned, verbose = FALSE, check_duplicates = FALSE)$Y,
                     "UMAP" = umap(df_sel, n_neighbors = input$neighbors, min_dist = input$min_dist))
    
    cluster_labels <- as.factor(kmeans(df_sel, centers = input$n_clusters, nstart = 10)$cluster)
    
    dimred_df <- data.frame(Dim1 = dimred[,1], Dim2 = dimred[,2], 
                            Treatment = treatment, 
                            Cluster = cluster_labels,
                            Sample = sample_ids)
    
    plot_ly(dimred_df, x = ~Dim1, y = ~Dim2, color = ~Treatment,
            text = ~paste("Sample:", Sample, "<br>Cluster:", Cluster),
            colors = "Set1",
            type = 'scatter', mode = 'markers',
            marker = list(size = 12, line = list(width = 1, color = '#333')),
            width = input$plot_width, height = input$plot_height) %>%
      layout(title = list(text = paste(input$dimred_method, "Projection"), font = list(size = 20)),
             xaxis = list(title = "Dim 1", titlefont = list(size = 18), tickfont = list(size = 15),
                          scaleanchor = "y", scaleratio = 1),
             yaxis = list(title = "Dim 2", titlefont = list(size = 18), tickfont = list(size = 15)),
             margin = list(l = 50, r = 50, b = 50, t = 60))
    
  })
  
  output$summary_table <- renderDT({
    req(input$run_cleaned, cleaned_data(), input$selected_markers, input$selected_treatment)
    df <- cleaned_data()
    df_long <- pivot_longer(df, cols = all_of(input$selected_markers),
                            names_to = "Marker", values_to = "Value")
    
    summary_df <- df_long %>%
      group_by(across(all_of(input$selected_treatment)), Marker) %>%
      summarise(Mean = mean(Value, na.rm = TRUE),
                SD = sd(Value, na.rm = TRUE), .groups = "drop")
    
    datatable(summary_df)
  })
 
  # ---- BATCH ANALYSIS FUNCTIONALITY ----
  
  # Create reactive values to store batch samples and their results
  batchSamples <- reactiveVal(list())
  batchResults <- reactiveVal(list())
  
  # Function to add samples to the batch list
  observeEvent(input$addSample, {
    req(input$batchFile)
    
    # Get current sample list
    current_samples <- batchSamples()
    
    # Process new files
    for (i in 1:nrow(input$batchFile)) {
      file_data <- input$batchFile[i, ]
      sample_id <- paste0("sample_", length(current_samples) + 1)
      
      # Determine group based on filename pattern or default to "Unknown"
      group <- "Unknown"
      if (input$groupingVariable == "Filename Pattern") {
        if (grepl(input$patternControl, file_data$name, ignore.case = TRUE)) {
          group <- "Control"
        } else if (grepl(input$patternTreated, file_data$name, ignore.case = TRUE)) {
          group <- "Treated"
        }
      }
      
      # Add the sample to the list
      current_samples[[sample_id]] <- list(
        id = sample_id,
        name = file_data$name,
        path = file_data$datapath,
        group = group,
        added = Sys.time()
      )
    }
    
    # Update the sample list
    batchSamples(current_samples)
  })
  
  # Clear all samples
  observeEvent(input$clearSamples, {
    batchSamples(list())
    batchResults(list())
  })
  
  # UI for sample list management
  output$batchSampleList <- renderUI({
    samples <- batchSamples()
    if (length(samples) == 0) {
      return(div(
        style = "margin-top: 15px; text-align: center;",
        "No samples added yet. Use the file selector above to add samples."
      ))
    }
    
    # Create a panel for each sample
    sample_panels <- lapply(samples, function(sample) {
      div(
        style = "border: 1px solid #ddd; border-radius: 5px; padding: 10px; margin-bottom: 10px;",
        div(
          style = "display: flex; justify-content: space-between;",
          strong(sample$name),
          selectInput(
            inputId = paste0("group_", sample$id),
            label = NULL,
            choices = c("Control", "Treated", "Unknown"),
            selected = sample$group,
            width = "120px"
          )
        ),
        div(
          style = "display: flex; justify-content: space-between; margin-top: 5px;",
          span(paste("Added:", format(sample$added, "%Y-%m-%d %H:%M"))),
          actionButton(
            inputId = paste0("remove_", sample$id),
            label = "Remove",
            class = "btn-danger btn-sm"
          )
        )
      )
    })
    
    # Return the UI
    div(
      style = "max-height: 300px; overflow-y: auto; padding: 10px; border: 1px solid #eee; margin-top: 10px;",
      do.call(tagList, sample_panels)
    )
  })
  
  # Update sample groups when changed in UI
  observe({
    samples <- batchSamples()
    if (length(samples) == 0) return()
    
    # Check for group changes
    updated <- FALSE
    for (id in names(samples)) {
      group_input_id <- paste0("group_", id)
      if (!is.null(input[[group_input_id]])) {
        if (samples[[id]]$group != input[[group_input_id]]) {
          samples[[id]]$group <- input[[group_input_id]]
          updated <- TRUE
        }
      }
    }
    
    # Update samples if needed
    if (updated) {
      batchSamples(samples)
    }
  })
  
  # Handle sample removal buttons
  observe({
    samples <- batchSamples()
    if (length(samples) == 0) return()
    
    # Check for remove button clicks
    for (id in names(samples)) {
      # Local id to avoid capturing issues
      local_id <- id 
      remove_id <- paste0("remove_", local_id)
      
      observeEvent(input[[remove_id]], {
        current_samples <- batchSamples()
        current_samples[[local_id]] <- NULL
        batchSamples(current_samples)
      }, ignoreInit = TRUE, ignoreNULL = TRUE)
    }
  })
  
  # Display the sample table
  output$batchSampleTable <- DT::renderDataTable({
    samples <- batchSamples()
    if (length(samples) == 0) {
      return(data.frame(
        "Sample Name" = character(0),
        "Group" = character(0),
        "Status" = character(0)
      ))
    }
    
    # Create a data frame for the table
    df <- data.frame(
      "Sample ID" = sapply(samples, function(s) s$id),
      "Sample Name" = sapply(samples, function(s) s$name),
      "Group" = sapply(samples, function(s) s$group),
      "Status" = sapply(samples, function(s) {
        if (!is.null(batchResults()[[s$id]])) {
          "Analyzed"
        } else {
          "Pending"
        }
      }),
      stringsAsFactors = FALSE
    )
    
    DT::datatable(
      df,
      options = list(
        pageLength = 10,
        searching = TRUE,
        dom = 'tip',
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  # Create marker selection UI for batch analysis
  output$batchMarkerSelectUI <- renderUI({
    samples <- batchSamples()
    if (length(samples) == 0) {
      return(div("Add samples first to select markers"))
    }
    
    # Try to get markers from the first sample
    first_sample <- samples[[1]]
    
    # Load the sample data
    tryCatch({
      file_data <- loadFlowData(first_sample$path, first_sample$name)$data
      
      if (inherits(file_data, "flowFrame")) {
        exprs_data <- exprs(file_data)
        params <- parameters(file_data)
        choices <- setNames(colnames(exprs_data), paste0(colnames(exprs_data), " - ", params$desc))
      } else {
        exprs_data <- file_data
        choices <- colnames(exprs_data)
      }
      
      selectInput(
        "batchSelectedMarkers", 
        "Select Markers/Channels", 
        choices = choices, 
        selected = choices[1:min(8, length(choices))], 
        multiple = TRUE
      )
    }, error = function(e) {
      div("Error loading sample data:", e$message)
    })
  })
  
  # Handle save/load sample configuration 
  output$downloadSampleConfig <- downloadHandler(
    filename = function() {
      paste0("flowcyto_samples_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      samples <- batchSamples()
      if (length(samples) == 0) return(NULL)
      
      # Create a data frame for export
      df <- data.frame(
        id = sapply(samples, function(s) s$id),
        name = sapply(samples, function(s) s$name),
        path = sapply(samples, function(s) s$path),
        group = sapply(samples, function(s) s$group),
        stringsAsFactors = FALSE
      )
      
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Handle loading sample configuration
  observeEvent(input$uploadSampleConfig, {
    req(input$uploadSampleConfig)
    
    tryCatch({
      config <- read.csv(input$uploadSampleConfig$datapath, stringsAsFactors = FALSE)
      
      # Validate the config
      if (!all(c("id", "name", "path", "group") %in% colnames(config))) {
        showNotification("Invalid configuration file format", type = "error")
        return()
      }
      
      # Create sample list from config
      samples <- list()
      for (i in 1:nrow(config)) {
        samples[[config$id[i]]] <- list(
          id = config$id[i],
          name = config$name[i],
          path = config$path[i],
          group = config$group[i],
          added = Sys.time()
        )
      }
      
      batchSamples(samples)
      showNotification("Sample configuration loaded", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading configuration:", e$message), type = "error")
    })
  })
  
  # Run the batch analysis
  observeEvent(input$runBatchAnalysis, {
    samples <- batchSamples()
    
    if (length(samples) == 0) {
      showNotification("No samples to analyze", type = "warning")
      return()
    }
    
    req(input$batchSelectedMarkers)
    
    # Prepare common preprocessing parameters
    preprocessing_params <- list(
      markers = input$batchSelectedMarkers,
      transform = input$batchTransform,
      cofactor = input$batchCofactor,
      n_events = input$batchEvents,
      perform_qc = isTRUE(input$batchPerformQC),
      perform_gating = isTRUE(input$batchPerformGating),
      scale_data = TRUE,
      seed = 123
    )
    
    # Add QC settings if enabled
    if (isTRUE(input$batchPerformQC)) {
      preprocessing_params$qc_settings <- list(
        max_anomalies = input$batchMaxAnomalies / 100  # Convert from percentage to proportion
      )
    }
    
    # Add gating parameters if enabled
    if (isTRUE(input$batchPerformGating)) {
      # Parse debris gate parameters from comma-separated string
      debris_gate_params <- unlist(strsplit(input$batchDebrisGate, ",\\s*"))
      
      preprocessing_params$gates <- list(
        debris_gate = if (length(debris_gate_params) >= 2) debris_gate_params[1:2] else NULL,
        live_dead_gate = if (input$batchLiveDeadGate != "None") input$batchLiveDeadGate else NULL
      )
    }
    
    # Initialize results list
    results <- list()
    
    # Define a processing function for one sample
    processSample <- function(sample) {
      withProgress(
        message = paste("Processing", sample$name),
        value = 0, 
        {
          # Load the file data
          file_data <- loadFlowData(sample$path, sample$name)$data
          
          # Process the data using the common parameters
          incProgress(0.2, detail = "Preprocessing data...")
          preprocess_results <- preprocessFlowData(file_data, preprocessing_params)
          
          # Run dimensionality reduction
          incProgress(0.4, detail = paste("Running", input$batchDimRedMethod, "..."))
          
          # Perform dimensionality reduction
          if (input$batchDimRedMethod == "t-SNE") {
            perplexity_value <- min(input$batchPerplexity, max(5, nrow(preprocess_results$scaled_data) / 10))
            
            # Ensure the data is a matrix
            data_matrix <- as.matrix(preprocess_results$scaled_data)
            
            # Prepare t-SNE parameters
            tsne_params <- list(
              dims = 2,
              perplexity = perplexity_value,
              max_iter = input$batch_tsne_max_iter,
              verbose = FALSE
            )
            
            if (input$batch_use_barnes_hut) {
              # Use Barnes-Hut t-SNE
              tsne_params$theta <- input$batchTsneTheta
              incProgress(0.1, detail = "Running Barnes-Hut t-SNE...")
            } else {
              # Use exact t-SNE (very slow for large datasets)
              tsne_params$theta <- 0.0  # This triggers exact t-SNE
              incProgress(0.1, detail = "Running exact t-SNE (may be slow)...")
            }
            
            # Run t-SNE with the configured parameters
            dr_result <- do.call(Rtsne, c(list(X = data_matrix), tsne_params))
            
            reduced_data <- data.frame(dim1 = dr_result$Y[,1], dim2 = dr_result$Y[,2])
          }
          
          # Create plot data
          plot_data <- as.data.frame(preprocess_results$sampled_data)
          colnames(plot_data) <- input$batchSelectedMarkers
          
          # Add dimensionality reduction coordinates
          plot_data$dim1 <- reduced_data$dim1
          plot_data$dim2 <- reduced_data$dim2
          
          # Run clustering if enabled
          cluster_results <- NULL
          if (input$showBatchClustering) {
            incProgress(0.6, detail = paste("Clustering with", input$batchClusterMethod, "..."))
            
            # Extract marker data for clustering
            marker_data <- plot_data[, input$batchSelectedMarkers, drop = FALSE]
            
            # Run clustering based on selected method
            if (input$batchClusterMethod == "K-means") {
              set.seed(123)
              km <- kmeans(marker_data, centers = input$batchNumClusters, nstart = 10)
              cluster_ids <- km$cluster
              
              # Store cluster centers for intensity profiles
              centers <- km$centers
              colnames(centers) <- input$batchSelectedMarkers
              
              cluster_results <- list(
                cluster_ids = cluster_ids,
                centers = centers,
                method = "K-means"
              )
            }
            else if (input$batchClusterMethod == "FlowSOM") {
              tryCatch({
                # Create a flowFrame from the matrix with proper marker names
                marker_matrix <- as.matrix(marker_data)
                colnames(marker_matrix) <- input$batchSelectedMarkers
                fcs_data <- flowCore::flowFrame(marker_matrix)
                
                # Set proper column names in the flowFrame parameters
                params <- flowCore::parameters(fcs_data)
                params$name <- input$batchSelectedMarkers
                params$desc <- input$batchSelectedMarkers
                flowCore::parameters(fcs_data) <- params
                
                # Use ReadInput to prepare the data for FlowSOM
                fsom_input <- FlowSOM::ReadInput(fcs_data, transform = FALSE, scale = FALSE)
                
                # Add logging and notification
                logInfo <- paste("Running FlowSOM with grid", input$batchSomXdim, "x", input$batchSomYdim, 
                                "training for", input$batchSomRlen, "iterations, forming", input$batchSomClusters, "clusters")
                print(logInfo)
                showNotification(logInfo, type = "message", duration = 3)
                
                # Build the SOM with user-defined grid dimensions
                fsom <- FlowSOM::BuildSOM(fsom_input, 
                                         xdim = input$batchSomXdim, 
                                         ydim = input$batchSomYdim,
                                         rlen = input$batchSomRlen)
                
                # Get metaclusters using the user-specified number of clusters
                metacl <- FlowSOM::MetaClustering(fsom$map$codes, 
                                                 method = "metaClustering_consensus",
                                                 max = input$batchSomClusters)
                
                # Map cells to SOM clusters
                cell_som_clusters <- fsom$map$mapping[,1]
                # Then map to metaclusters
                cell_metaclusters <- metacl[cell_som_clusters]
                
                # Calculate metacluster centers
                unique_clusters <- sort(unique(cell_metaclusters))
                centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(marker_data))
                
                for (i in seq_along(unique_clusters)) {
                  cluster_idx <- which(cell_metaclusters == unique_clusters[i])
                  if (length(cluster_idx) > 0) {
                    centers[i,] <- colMeans(marker_data[cluster_idx, , drop = FALSE])
                  }
                }
                
                colnames(centers) <- input$batchSelectedMarkers
                rownames(centers) <- paste("Cluster", unique_clusters)
                
                cluster_results <- list(
                  cluster_ids = cell_metaclusters,
                  centers = centers,
                  method = "FlowSOM"
                )
              }, error = function(e) {
                showNotification(paste("FlowSOM clustering error:", e$message), type = "error")
                cluster_results <- NULL
              })
            }
            else if (input$batchClusterMethod == "DBSCAN") {
              tryCatch({
                # Scale data for DBSCAN
                scaled_markers <- scale(marker_data)
                
                # Log the number of cells for debugging
                logInfo <- paste("Running DBSCAN on", nrow(scaled_markers), "cells with eps =", 
                               input$batchDbscanEps, "and minPts =", input$batchDbscanMinPts)
                print(logInfo)
                
                # Run DBSCAN
                dbscan_result <- dbscan::dbscan(scaled_markers, eps = input$batchDbscanEps, 
                                              minPts = input$batchDbscanMinPts)
                cluster_ids <- dbscan_result$cluster
                
                # Handle noise points (cluster ID = 0)
                # Assign them to a new cluster (max cluster ID + 1)
                if (any(cluster_ids == 0)) {
                  max_cluster <- max(cluster_ids)
                  cluster_ids[cluster_ids == 0] <- max_cluster + 1
                }
                
                # Calculate cluster centers
                unique_clusters <- sort(unique(cluster_ids))
                centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(marker_data))
                
                for (i in seq_along(unique_clusters)) {
                  cluster_idx <- which(cluster_ids == unique_clusters[i])
                  if (length(cluster_idx) > 0) {
                    centers[i,] <- colMeans(marker_data[cluster_idx, , drop = FALSE])
                  }
                }
                
                colnames(centers) <- input$batchSelectedMarkers
                rownames(centers) <- paste("Cluster", unique_clusters)
                
                cluster_results <- list(
                  cluster_ids = cluster_ids,
                  centers = centers,
                  method = "DBSCAN"
                )
              }, error = function(e) {
                showNotification(paste("DBSCAN clustering error:", e$message), type = "error")
                cluster_results <- NULL
              })
            }
            else if (input$batchClusterMethod == "Phenograph") {
              tryCatch({
                # Determine appropriate k based on dataset size
                # For larger datasets, we can use larger k
                cell_count <- nrow(marker_data)
                
                # Adjust k if dataset is very small
                actual_k <- min(input$batchPhenoK, max(5, cell_count / 15))
                
                # Log the number of cells for debugging
                logInfo <- paste("Running Phenograph on", cell_count, "cells with k =", actual_k)
                print(logInfo)
                
                # Run Phenograph
                pheno_result <- Rphenograph(as.matrix(marker_data), k = actual_k)
                
                # Get cluster IDs
                cluster_ids <- as.numeric(membership(pheno_result[[2]]))
                
                # Calculate cluster centers
                unique_clusters <- sort(unique(cluster_ids))
                centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(marker_data))
                
                for (i in seq_along(unique_clusters)) {
                  cluster_idx <- which(cluster_ids == unique_clusters[i])
                  if (length(cluster_idx) > 0) {
                    centers[i,] <- colMeans(marker_data[cluster_idx, , drop = FALSE])
                  }
                }
                
                colnames(centers) <- input$batchSelectedMarkers
                rownames(centers) <- paste("Cluster", unique_clusters)
                
                cluster_results <- list(
                  cluster_ids = cluster_ids,
                  centers = centers,
                  method = "Phenograph"
                )
              }, error = function(e) {
                showNotification(paste("Phenograph clustering error:", e$message), type = "error")
                cluster_results <- NULL
              })
            }
            
            # Identify cell populations if enabled
            populations <- NULL
            if (input$batchIdentifyPops && !is.null(cluster_results)) {
              incProgress(0.8, detail = "Identifying cell populations...")
              
              # Get cluster centers and marker names
              centers <- cluster_results$centers
              marker_names <- colnames(centers)
              
              # Identify populations using the existing function
              populations <- identify_cell_populations(
                centers, 
                marker_names,
                high_threshold = input$batchHighExpressionThreshold,
                low_threshold = input$batchLowExpressionThreshold,
                min_confidence = input$batchMinConfidenceThreshold/100  # Convert from percentage
              )
            }
            
            # Add cluster IDs to plot data if clustering was successful
            if (!is.null(cluster_results)) {
              plot_data$Cluster <- as.factor(cluster_results$cluster_ids)
              
              # Add populations if identified
              if (!is.null(populations)) {
                # Map cluster IDs to population names
                population_map <- setNames(
                  populations$Population,
                  populations$Cluster
                )
                
                # Add population column to plot data
                plot_data$Population <- population_map[as.character(plot_data$Cluster)]
              }
            }
          }
          
          # Store final results
          sample_result <- list(
            id = sample$id,
            name = sample$name,
            group = sample$group,
            preprocess_results = preprocess_results,
            plot_data = plot_data,
            cluster_results = cluster_results,
            populations = populations,
            dim_red_method = input$batchDimRedMethod,
            num_cells = nrow(plot_data),
            processed_time = Sys.time()
          )
          
          return(sample_result)
        }
      )
    }
    
    # Process each sample
    withProgress(
      message = "Running batch analysis",
      value = 0, 
      {
        for (i in seq_along(samples)) {
          incProgress(amount = 1/length(samples), 
                     detail = paste("Processing sample", i, "of", length(samples)))
          
          sample <- samples[[i]]
          result <- processSample(sample)
          results[[sample$id]] <- result
        }
      }
    )
    
    # Update results
    batchResults(results)
    
    # Update sample selection dropdown for visualization
    updateSelectInput(session, "viewSample", 
                    choices = setNames(
                      names(results),
                      sapply(results, function(r) r$name)
                    ))
    
    # Update comparison dropdowns
    control_ids <- c()
    treated_ids <- c()
    control_names <- c()
    treated_names <- c()
    
    # Loop through results to find control and treated samples
    for (id in names(results)) {
      sample <- results[[id]]
      if (sample$group == "Control") {
        control_ids <- c(control_ids, id)
        control_names <- c(control_names, sample$name)
      } else if (sample$group == "Treated") {
        treated_ids <- c(treated_ids, id)
        treated_names <- c(treated_names, sample$name)
      }
    }
    
    if (length(control_ids) > 0) {
      updateSelectInput(session, "compareViewControl", 
                        choices = setNames(control_ids, control_names))
    }
    
    if (length(treated_ids) > 0) {
      updateSelectInput(session, "compareViewTreated",
                        choices = setNames(treated_ids, treated_names))
    }
    
    # Update metrics view sample selector
    updateSelectInput(session, "metricsViewSample",
                     choices = setNames(
                       names(results),
                       sapply(results, function(r) r$name)
                     ))
    
    # Update marker comparison dropdown
    if (length(results) > 0) {
      # Get all marker columns across samples
      all_markers <- unique(unlist(lapply(results, function(r) {
        setdiff(colnames(r$plot_data), c("dim1", "dim2", "Cluster", "Population"))
      })))
      
      # Update the dropdown
      if (length(all_markers) > 0) {
        updateSelectInput(session, "markerCompare", choices = all_markers)
      }
    }
    
    # Show batch analysis completion notification
    showNotification(paste("Batch analysis complete for", length(samples), "samples"), 
                    type = "message", 
                    duration = 5)
  })
  
  # Single sample view title
  output$sampleViewTitle <- renderText({
    req(input$viewSample)
    results <- batchResults()
    if (is.null(results) || is.null(results[[input$viewSample]])) {
      return("No sample selected")
    }
    
    sample <- results[[input$viewSample]]
    paste0(sample$name, " (", sample$group, ")")
  })
  
  # Render individual sample dimensionality reduction plot
  output$sampleDimensionalityPlot <- renderPlotly({
    req(input$viewSample)
    results <- batchResults()
    if (is.null(results) || is.null(results[[input$viewSample]])) {
      return(NULL)
    }
    
    sample <- results[[input$viewSample]]
    plot_data <- sample$plot_data
    
    # If clustering was performed, color by cluster
    if (!is.null(sample$cluster_results) && "Cluster" %in% colnames(plot_data)) {
      # Create hover text based on available information
      if ("Population" %in% colnames(plot_data)) {
        hover_text <- paste(
          "Cluster:", plot_data$Cluster,
          "<br>Population:", plot_data$Population,
          "<br>", sample$dim_red_method, "1:", round(plot_data$dim1, 2),
          "<br>", sample$dim_red_method, "2:", round(plot_data$dim2, 2)
        )
      } else {
        hover_text <- paste(
          "Cluster:", plot_data$Cluster,
          "<br>", sample$dim_red_method, "1:", round(plot_data$dim1, 2),
          "<br>", sample$dim_red_method, "2:", round(plot_data$dim2, 2)
        )
      }
      
      # Create a ggplot object first
      p <- ggplot(plot_data, aes(x = dim1, y = dim2, color = Cluster, text = hover_text)) +
        geom_point(alpha = 0.7, size = plot_settings$point_size/2) +
        get_color_palette() +
        labs(
          title = paste(sample$dim_red_method, "Plot Colored by Cluster"),
          x = paste(sample$dim_red_method, "1"),
          y = paste(sample$dim_red_method, "2"),
          color = "Cluster"
        ) +
        get_standard_theme()
      
      # Convert to plotly
      ggplotly(p, tooltip = "text") %>% 
        layout(
          height = plot_settings$height, 
          width = plot_settings$width,
          hoverlabel = list(
            bgcolor = "white",
            font = list(family = "Arial", size = plot_settings$font_size)
          )
        )
    } else {
      # If no clustering, create a simple plot
      hover_text <- paste(
        sample$dim_red_method, "1:", round(plot_data$dim1, 2),
        "<br>", sample$dim_red_method, "2:", round(plot_data$dim2, 2)
      )
      
      # If we have markers, add them to hover
      for (marker in input$batchSelectedMarkers) {
        if (marker %in% colnames(plot_data)) {
          hover_text <- paste0(hover_text, "<br>", marker, ": ", round(plot_data[[marker]], 2))
        }
      }
      
      # Create a ggplot object first
      p <- ggplot(plot_data, aes(x = dim1, y = dim2, text = hover_text)) +
        geom_point(alpha = 0.7, size = plot_settings$point_size/2, color = "#3366CC") +
        labs(
          title = paste(sample$dim_red_method, "Plot"),
          x = paste(sample$dim_red_method, "1"),
          y = paste(sample$dim_red_method, "2")
        ) +
        get_standard_theme()
      
      # Convert to plotly
      ggplotly(p, tooltip = "text") %>% 
        layout(
          height = plot_settings$height, 
          width = plot_settings$width,
          hoverlabel = list(
            bgcolor = "white",
            font = list(family = "Arial", size = plot_settings$font_size)
          )
        )
    }
  })
  
  # Render sample cluster plot
  output$sampleClusterPlot <- renderPlotly({
    req(input$viewSample)
    results <- batchResults()
    if (is.null(results) || is.null(results[[input$viewSample]])) {
      return(NULL)
    }
    
    sample <- results[[input$viewSample]]
    if (is.null(sample$cluster_results) || !("Cluster" %in% colnames(sample$plot_data))) {
      return(NULL)
    }
    
    plot_data <- sample$plot_data
    
    # Create hover text based on available information
    if ("Population" %in% colnames(plot_data)) {
      hover_text <- paste(
        "Cluster:", plot_data$Cluster,
        "<br>Population:", plot_data$Population,
        "<br>", sample$dim_red_method, "1:", round(plot_data$dim1, 2),
        "<br>", sample$dim_red_method, "2:", round(plot_data$dim2, 2)
      )
    } else {
      hover_text <- paste(
        "Cluster:", plot_data$Cluster,
        "<br>", sample$dim_red_method, "1:", round(plot_data$dim1, 2),
        "<br>", sample$dim_red_method, "2:", round(plot_data$dim2, 2)
      )
    }
    
    p <- plot_ly(plot_data, x = ~dim1, y = ~dim2, color = ~Cluster,
                text = hover_text, type = "scatter", mode = "markers",
                marker = list(size = 8, opacity = 0.7)) %>%
      layout(
        title = paste("Cluster Plot -", sample$cluster_results$method),
        xaxis = list(title = paste(sample$dim_red_method, "1")),
        yaxis = list(title = paste(sample$dim_red_method, "2")),
        colorbar = list(title = "Cluster")
      )
    
    return(p)
  })
  
  # Render sample heatmap
  output$sampleHeatmap <- renderPlot({
    req(input$viewSample)
    results <- batchResults()
    if (is.null(results) || is.null(results[[input$viewSample]])) {
      return(NULL)
    }
    
    sample <- results[[input$viewSample]]
    if (is.null(sample$cluster_results) || is.null(sample$cluster_results$centers)) {
      return(NULL)
    }
    
    # Get cluster centers
    centers <- sample$cluster_results$centers
    
    # Add population labels to heatmap if available
    if (!is.null(sample$populations) && input$batchShowPopLabels) {
      # Create a mapping of cluster IDs to population names
      pop_mapping <- setNames(
        sample$populations$Population,
        sample$populations$Cluster
      )
      
      # Add population names to row names
      rownames(centers) <- paste(rownames(centers), "-", 
                                pop_mapping[rownames(centers)])
    }
    
    # Convert to long format for heatmap
    centers_long <- reshape2::melt(centers)
    colnames(centers_long) <- c("Cluster", "Marker", "Value")
    
    # Create the heatmap
    ggplot(centers_long, aes(x = Marker, y = Cluster, fill = Value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
      labs(
        title = paste("Marker Expression by Cluster -", sample$name),
        fill = "Expression"
      ) +
      get_standard_theme() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = plot_settings$font_size * 1.2)
      )
  },
  width = function() plot_settings$width,
  height = function() plot_settings$height)
  
  # Render sample cluster statistics
  output$sampleClusterStats <- DT::renderDataTable({
    req(input$viewSample)
    results <- batchResults()
    if (is.null(results) || is.null(results[[input$viewSample]])) {
      return(NULL)
    }
    
    sample <- results[[input$viewSample]]
    if (is.null(sample$cluster_results) || !("Cluster" %in% colnames(sample$plot_data))) {
      return(NULL)
    }
    
    # Calculate cluster statistics
    plot_data <- sample$plot_data
    
    # Calculate cluster frequencies
    cluster_counts <- table(plot_data$Cluster)
    total_cells <- nrow(plot_data)
    
    # Create data frame for statistics
    stats_df <- data.frame(
      Cluster = names(cluster_counts),
      Count = as.numeric(cluster_counts),
      Percentage = round(as.numeric(cluster_counts) / total_cells * 100, 2)
    )
    
    # Add population information if available
    if (!is.null(sample$populations) && input$batchShowPopLabels) {
      pop_info <- sample$populations
      
      # Match cluster IDs to get population names
      stats_df$Population <- pop_info$Population[match(stats_df$Cluster, pop_info$Cluster)]
      stats_df$Confidence <- pop_info$Confidence[match(stats_df$Cluster, pop_info$Cluster)]
      
      # Reorder columns
      stats_df <- stats_df[, c("Cluster", "Population", "Count", "Percentage", "Confidence")]
    }
    
    DT::datatable(
      stats_df,
      options = list(
        pageLength = 15,
        scrollX = TRUE
      ),
      rownames = FALSE,
      caption = paste("Cluster Statistics for", sample$name)
    ) %>% formatRound(columns = c("Percentage", "Confidence"), digits = 2)
  })
  
  # Render comparison plots between control and treated samples
  output$controlSamplePlot <- renderPlotly({
    req(input$compareViewControl)
    results <- batchResults()
    if (is.null(results) || is.null(results[[input$compareViewControl]])) {
      return(NULL)
    }
    
    sample <- results[[input$compareViewControl]]
    plot_data <- sample$plot_data
    
    # If clustering was performed, color by cluster
    if (!is.null(sample$cluster_results) && "Cluster" %in% colnames(plot_data)) {
      # Create hover text with cluster info
      hover_text <- paste(
        "Cluster:", plot_data$Cluster,
        "<br>", sample$dim_red_method, "1:", round(plot_data$dim1, 2),
        "<br>", sample$dim_red_method, "2:", round(plot_data$dim2, 2)
      )
      
      # Add population if available
      if ("Population" %in% colnames(plot_data)) {
        hover_text <- paste0(hover_text, "<br>Population: ", plot_data$Population)
      }
      
      p <- plot_ly(plot_data, x = ~dim1, y = ~dim2, color = ~Cluster,
                  text = hover_text, type = "scatter", mode = "markers",
                  marker = list(size = 7, opacity = 0.7)) %>%
        layout(
          title = paste("Control:", sample$name),
          xaxis = list(title = paste(sample$dim_red_method, "1")),
          yaxis = list(title = paste(sample$dim_red_method, "2")),
          showlegend = FALSE
        )
    } else {
      # If no clustering, create a simple plot
      hover_text <- paste(
        sample$dim_red_method, "1:", round(plot_data$dim1, 2),
        "<br>", sample$dim_red_method, "2:", round(plot_data$dim2, 2)
      )
      
      p <- plot_ly(plot_data, x = ~dim1, y = ~dim2,
                  text = hover_text, type = "scatter", mode = "markers",
                  marker = list(size = 7, opacity = 0.7, color = "#3366CC")) %>%
        layout(
          title = paste("Control:", sample$name),
          xaxis = list(title = paste(sample$dim_red_method, "1")),
          yaxis = list(title = paste(sample$dim_red_method, "2"))
        )
    }
    
    return(p)
  })
  
  output$treatedSamplePlot <- renderPlotly({
    req(input$compareViewTreated)
    results <- batchResults()
    if (is.null(results) || is.null(results[[input$compareViewTreated]])) {
      return(NULL)
    }
    
    sample <- results[[input$compareViewTreated]]
    plot_data <- sample$plot_data
    
    # If clustering was performed, color by cluster
    if (!is.null(sample$cluster_results) && "Cluster" %in% colnames(plot_data)) {
      # Create hover text with cluster info
      hover_text <- paste(
        "Cluster:", plot_data$Cluster,
        "<br>", sample$dim_red_method, "1:", round(plot_data$dim1, 2),
        "<br>", sample$dim_red_method, "2:", round(plot_data$dim2, 2)
      )
      
      # Add population if available
      if ("Population" %in% colnames(plot_data)) {
        hover_text <- paste0(hover_text, "<br>Population: ", plot_data$Population)
      }
      
      p <- plot_ly(plot_data, x = ~dim1, y = ~dim2, color = ~Cluster,
                  text = hover_text, type = "scatter", mode = "markers",
                  marker = list(size = 7, opacity = 0.7)) %>%
        layout(
          title = paste("Treated:", sample$name),
          xaxis = list(title = paste(sample$dim_red_method, "1")),
          yaxis = list(title = paste(sample$dim_red_method, "2"))
        )
    } else {
      # If no clustering, create a simple plot
      hover_text <- paste(
        sample$dim_red_method, "1:", round(plot_data$dim1, 2),
        "<br>", sample$dim_red_method, "2:", round(plot_data$dim2, 2)
      )
      
      p <- plot_ly(plot_data, x = ~dim1, y = ~dim2,
                  text = hover_text, type = "scatter", mode = "markers",
                  marker = list(size = 7, opacity = 0.7, color = "#FF6633")) %>%
        layout(
          title = paste("Treated:", sample$name),
          xaxis = list(title = paste(sample$dim_red_method, "1")),
          yaxis = list(title = paste(sample$dim_red_method, "2"))
        )
    }
    
    return(p)
  })
  
  
  # Update marker dropdown for single sample view
  observe({
    req(input$viewSample)
    results <- batchResults()
    if (is.null(results) || is.null(results[[input$viewSample]])) {
      return()
    }
    
    sample <- results[[input$viewSample]]
    markers <- input$batchSelectedMarkers
    
    updateSelectInput(session, "sampleMarkerSelect", 
                     choices = markers,
                     selected = markers[1])
  })
  
  # Single sample marker expression by cluster
  output$markerExpressionByCluster <- renderPlotly({
    req(input$viewSample, input$sampleMarkerSelect)
    results <- batchResults()
    
    if (is.null(results) || is.null(results[[input$viewSample]]) || 
        is.null(results[[input$viewSample]]$cluster_results)) {
      return(NULL)
    }
    
    sample <- results[[input$viewSample]]
    marker <- input$sampleMarkerSelect
    
    # Check if marker exists in sample data
    if (!(marker %in% colnames(sample$plot_data))) {
      return(NULL)
    }
    
    # Create a boxplot of marker expression by cluster
    plot_data <- sample$plot_data
    
    # Create hover text
    hover_text <- paste(
      "Cluster:", plot_data$Cluster,
      "<br>", marker, ":", round(plot_data[[marker]], 2)
    )
    
    # Add population info if available
    if ("Population" %in% colnames(plot_data)) {
      hover_text <- paste0(hover_text, "<br>Population: ", plot_data$Population)
    }
    
    # Calculate summary stats for each cluster
    cluster_stats <- plot_data %>%
      group_by(Cluster) %>%
      summarize(
        Median = median(.data[[marker]], na.rm = TRUE),
        Mean = mean(.data[[marker]], na.rm = TRUE),
        Max = max(.data[[marker]], na.rm = TRUE),
        Min = min(.data[[marker]], na.rm = TRUE),
        Q1 = quantile(.data[[marker]], 0.25, na.rm = TRUE),
        Q3 = quantile(.data[[marker]], 0.75, na.rm = TRUE),
        Count = n(),
        .groups = 'drop'
      ) %>%
      arrange(Mean)  # Sort by mean expression
    
    # Create a violin plot with boxplot overlay
    p <- plot_ly() %>%
      add_trace(
        data = cluster_stats,
        x = ~reorder(Cluster, Mean),
        y = ~Mean,
        type = "scatter",
        mode = "markers",
        name = "Mean",
        marker = list(color = "red", size = 10, symbol = "diamond")
      )
    
    # Add violin plots for each cluster
    for (cluster in unique(plot_data$Cluster)) {
      # Subset data for this cluster
      cluster_data <- plot_data[plot_data$Cluster == cluster, ]
      
      # Add violin trace
      p <- p %>% add_trace(
        x = rep(cluster, nrow(cluster_data)),
        y = cluster_data[[marker]],
        type = "violin",
        name = paste("Cluster", cluster),
        box = list(visible = TRUE),
        meanline = list(visible = TRUE),
        legendgroup = cluster,
        showlegend = FALSE,
        hoverinfo = "skip"
      )
    }
    
    # Apply layout
    p <- p %>% layout(
      title = paste("Distribution of", marker, "by Cluster"),
      xaxis = list(title = "Cluster"),
      yaxis = list(title = marker),
      violinmode = "group",
      hovermode = "closest"
    )
    
    return(p)
  })
  
  # Update cluster comparison dropdowns
  observe({
    results <- batchResults()
    if (is.null(results)) return()
    
    # Get control and treated samples
    control_ids <- c()
    treated_ids <- c()
    control_names <- c()
    treated_names <- c()
    
    # Loop through results to find control and treated samples
    for (id in names(results)) {
      sample <- results[[id]]
      if (sample$group == "Control") {
        control_ids <- c(control_ids, id)
        control_names <- c(control_names, sample$name)
      } else if (sample$group == "Treated") {
        treated_ids <- c(treated_ids, id)
        treated_names <- c(treated_names, sample$name)
      }
    }
    
    # Update control dropdown
    if (length(control_ids) > 0) {
      updateSelectInput(session, "clusterCompareControl", 
                       choices = setNames(control_ids, control_names))
    }
    
    # Update treated dropdown
    if (length(treated_ids) > 0) {
      updateSelectInput(session, "clusterCompareTreated",
                       choices = setNames(treated_ids, treated_names))
    }
  })
  
  # Cluster mapping heatmap
  output$clusterMappingHeatmap <- renderPlot({
    req(input$clusterCompareControl, input$clusterCompareTreated)
    results <- batchResults()
    
    if (is.null(results) || is.null(results[[input$clusterCompareControl]]) || 
        is.null(results[[input$clusterCompareTreated]])) {
      return(NULL)
    }
    
    # Get the samples
    control_sample <- results[[input$clusterCompareControl]]
    treated_sample <- results[[input$clusterCompareTreated]]
    
    # Check if clustering was performed for both samples
    if (is.null(control_sample$cluster_results) || is.null(treated_sample$cluster_results)) {
      return(NULL)
    }
    
    # Get cluster centers
    control_centers <- control_sample$cluster_results$centers
    treated_centers <- treated_sample$cluster_results$centers
    
    # Calculate similarity matrix
    similarity_matrix <- matrix(NA, 
                              nrow = nrow(control_centers), 
                              ncol = nrow(treated_centers))
    
    for (i in 1:nrow(control_centers)) {
      for (j in 1:nrow(treated_centers)) {
        # Calculate Euclidean distance between cluster centers
        dist_val <- sqrt(sum((control_centers[i,] - treated_centers[j,])^2))
        # Convert distance to similarity (invert)
        similarity_matrix[i,j] <- 1 / (1 + dist_val)
      }
    }
    
    # Set row/column names
    rownames(similarity_matrix) <- paste0("C", rownames(control_centers))
    colnames(similarity_matrix) <- paste0("T", rownames(treated_centers))
    
    # Add population labels if available
    if (!is.null(control_sample$populations) && !is.null(treated_sample$populations)) {
      # Get population mappings
      control_pop_map <- setNames(
        control_sample$populations$Population,
        paste0("C", control_sample$populations$Cluster)
      )
      
      treated_pop_map <- setNames(
        treated_sample$populations$Population,
        paste0("T", treated_sample$populations$Cluster)
      )
      
      # Update row/column names with population information
      rownames(similarity_matrix) <- paste0(rownames(similarity_matrix), " (", 
                                         control_pop_map[rownames(similarity_matrix)], ")")
      
      colnames(similarity_matrix) <- paste0(colnames(similarity_matrix), " (", 
                                         treated_pop_map[colnames(similarity_matrix)], ")")
    }
    
    # Convert to data frame for ggplot
    sim_df <- reshape2::melt(similarity_matrix, varnames = c("Control", "Treated"), 
                           value.name = "Similarity")
    
    # Get color palette based on settings
    palette_colors <- switch(plot_settings$color_palette,
                           "viridis" = viridisLite::viridis(10),
                           "plasma" = viridisLite::plasma(10),
                           "blues" = colorRampPalette(c("white", "darkblue"))(10),
                           "reds" = colorRampPalette(c("white", "darkred"))(10),
                           viridisLite::viridis(10))  # Default
    
    # Create heatmap
    ggplot(sim_df, aes(x = Treated, y = Control, fill = Similarity)) +
      geom_tile() +
      scale_fill_gradientn(colors = palette_colors) +
      get_standard_theme() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = plot_settings$font_size * 0.8),
        axis.text.y = element_text(size = plot_settings$font_size * 0.8),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, size = plot_settings$font_size * 1.2)
      ) +
      labs(
        title = "Cluster Similarity Between Control and Treated Samples",
        subtitle = paste(control_sample$name, "vs", treated_sample$name),
        fill = "Similarity"
      ) +
      geom_text(aes(label = sprintf("%.2f", Similarity)), size = plot_settings$font_size * 0.25)
  },
  width = function() plot_settings$width,
  height = function() plot_settings$height)
  
  # Signature marker heatmap
  output$signatureMarkerHeatmap <- renderPlot({
    req(input$clusterCompareControl, input$clusterCompareTreated)
    results <- batchResults()
    
    if (is.null(results) || is.null(results[[input$clusterCompareControl]]) || 
        is.null(results[[input$clusterCompareTreated]])) {
      return(NULL)
    }
    
    # Get the samples
    control_sample <- results[[input$clusterCompareControl]]
    treated_sample <- results[[input$clusterCompareTreated]]
    
    # Check if clustering was performed for both samples
    if (is.null(control_sample$cluster_results) || is.null(treated_sample$cluster_results)) {
      return(NULL)
    }
    
    # Get markers
    markers <- input$batchSelectedMarkers
    
    # Combine cluster centers from both samples
    control_centers <- control_sample$cluster_results$centers
    treated_centers <- treated_sample$cluster_results$centers
    
    # Create combined centers data frame
    control_df <- as.data.frame(control_centers) %>%
      tibble::rownames_to_column("Cluster") %>%
      mutate(Sample = "Control", 
             ClusterID = paste0("C", Cluster))
    
    treated_df <- as.data.frame(treated_centers) %>%
      tibble::rownames_to_column("Cluster") %>%
      mutate(Sample = "Treated", 
             ClusterID = paste0("T", Cluster))
    
    # Combine data
    combined_centers <- bind_rows(control_df, treated_df)
    
    # Add population labels if available
    if (!is.null(control_sample$populations) && !is.null(treated_sample$populations)) {
      # Create mapping for control populations
      control_pop_map <- setNames(
        control_sample$populations$Population,
        control_sample$populations$Cluster
      )
      
      # Create mapping for treated populations
      treated_pop_map <- setNames(
        treated_sample$populations$Population,
        treated_sample$populations$Cluster
      )
      
      # Add population column
      combined_centers$Population <- NA
      for (i in 1:nrow(combined_centers)) {
        if (combined_centers$Sample[i] == "Control") {
          combined_centers$Population[i] <- control_pop_map[combined_centers$Cluster[i]]
        } else {
          combined_centers$Population[i] <- treated_pop_map[combined_centers$Cluster[i]]
        }
      }
      
      # Update ClusterID with population
      combined_centers$ClusterID <- paste0(combined_centers$ClusterID, " (", 
                                         combined_centers$Population, ")")
    }
    
    # Convert to long format
    combined_long <- combined_centers %>%
      select(-Cluster, -Population) %>%
      pivot_longer(cols = all_of(markers), names_to = "Marker", values_to = "Expression")
    
    # Scale expression values for heatmap
    combined_scaled <- combined_long %>%
      group_by(Marker) %>%
      mutate(Scaled_Expr = scale(Expression)[,1]) %>%
      ungroup()
    
    # Create the heatmap
    ggplot(combined_scaled, aes(x = Marker, y = ClusterID, fill = Scaled_Expr)) +
      geom_tile() +
      scale_fill_gradient2(low = "navy", mid = "white", high = "firebrick3", midpoint = 0) +
      facet_grid(Sample ~ ., scales = "free_y", space = "free_y") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        axis.text.y = element_text(size = 14),
        panel.grid = element_blank(),
        strip.text = element_text(size = 14, face = "bold"),
        strip.background = element_rect(fill = "lightgray", color = NA),
        plot.title = element_text(hjust = 0.5, size = 14)
      ) +
      labs(
        title = "Marker Expression Profiles by Cluster",
        subtitle = paste(control_sample$name, "vs", treated_sample$name),
        x = "Marker",
        y = "Cluster",
        fill = "Z-score"
      )
  })
  
  # Make Raw Data Analysis visualization options affect plots directly
  observe({
    # Only update when explicitly changed in Raw Data tab to avoid conflicts with global settings
    if (!is.null(input$plotWidth) && !is.null(input$plotHeight)) {
      isolate({
        plot_settings$width <- input$plotWidth
        plot_settings$height <- input$plotHeight
      })
    }
  })
}


shinyApp(ui, server)
