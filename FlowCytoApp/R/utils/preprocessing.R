# Preprocessing utility functions for Flow Cytometry Analysis

# Spillover compensation functions
computeSpilloverMatrix <- function(control_files, marker_assignments, unstained_file_path, unstained_file_name) {
  # control_files: named list of file paths
  # marker_assignments: named vector mapping file names to marker names
  # unstained_file_path: path to unstained control file
  # unstained_file_name: name of unstained control file
  
  tryCatch({
    # Load control files
    control_flowframes <- list()
    
    for (file_name in names(control_files)) {
      file_path <- control_files[[file_name]]
      if (file_name %in% names(marker_assignments)) {
        marker_name <- marker_assignments[[file_name]]
        control_flowframes[[marker_name]] <- read.FCS(file_path, transformation = FALSE)
      }
    }
    
    # Load unstained control
    unstained_flowframe <- read.FCS(unstained_file_path, transformation = FALSE)
    control_flowframes[["unstained"]] <- unstained_flowframe
    
    # Create flowSet from control files
    if (length(control_flowframes) < 3) {  # At least 2 stains + 1 unstained
      stop("At least 2 single-stain controls plus 1 unstained control are required for spillover computation")
    }
    
    # Convert to flowSet
    control_set <- flowSet(control_flowframes)
    
    # Compute spillover matrix using flowStats
    spillover_matrix <- spillover(control_set, 
                                unstained = "unstained",  # Use the unstained control
                                patt = ".*",              # Pattern to match all markers
                                method = "median") 
    
    return(list(
      spillover_matrix = spillover_matrix,
      control_files = names(control_flowframes),
      success = TRUE,
      message = paste("Spillover matrix computed successfully using", length(control_flowframes) - 1, "single-stain controls and 1 unstained control")
    ))
    
  }, error = function(e) {
    return(list(
      spillover_matrix = NULL,
      success = FALSE,
      message = paste("Error computing spillover matrix:", e$message)
    ))
  })
}

# Function to apply compensation to flow data
applyCompensation <- function(flow_data, spillover_matrix) {
  if (is.null(spillover_matrix)) {
    return(list(
      data = flow_data,
      compensated = FALSE,
      message = "No spillover matrix provided"
    ))
  }
  
  tryCatch({
    # Create compensation object
    comp_obj <- compensation(spillover_matrix)
    
    # Apply compensation
    compensated_data <- compensate(flow_data, comp_obj)
    
    return(list(
      data = compensated_data,
      compensated = TRUE,
      message = "Compensation applied successfully"
    ))
    
  }, error = function(e) {
    return(list(
      data = flow_data,
      compensated = FALSE,
      message = paste("Error applying compensation:", e$message)
    ))
  })
}

# Function to validate spillover matrix
validateSpilloverMatrix <- function(spillover_matrix, flow_data_channels) {
  if (is.null(spillover_matrix)) {
    return(list(valid = FALSE, message = "No spillover matrix provided"))
  }
  
  # Check if matrix is square
  if (nrow(spillover_matrix) != ncol(spillover_matrix)) {
    return(list(valid = FALSE, message = "Spillover matrix must be square"))
  }
  
  # Check if diagonal elements are close to 1
  diag_elements <- diag(spillover_matrix)
  if (any(abs(diag_elements - 1) > 0.1)) {
    return(list(valid = FALSE, message = "Diagonal elements should be close to 1"))
  }
  
  # Check if channel names match
  matrix_channels <- colnames(spillover_matrix)
  matching_channels <- intersect(matrix_channels, flow_data_channels)
  
  if (length(matching_channels) < 2) {
    return(list(valid = FALSE, message = "Not enough matching channels between matrix and data"))
  }
  
  return(list(
    valid = TRUE, 
    message = paste("Matrix valid for", length(matching_channels), "channels"),
    matching_channels = matching_channels
  ))
}

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
    live_dead_gate = NULL,
    live_dead_threshold = 1000
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
        gate_ranges <- list(c(-Inf, gates$live_dead_threshold))  # Threshold value, adjust as needed
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

# Arcsinh transformation function
asinhTransform <- function(x, cofactor = 5) {
  asinh(x / cofactor)
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
    perform_compensation = FALSE,
    spillover_matrix = NULL,
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
  
  # Step 1: Compensation (applied first, before QC and gating)
  if (params$perform_compensation && !is.null(params$spillover_matrix)) {
    compensation_result <- applyCompensation(input_data, params$spillover_matrix)
    results$compensated_data <- compensation_result$data
    results$metrics$compensation <- list(
      applied = compensation_result$compensated,
      message = compensation_result$message
    )
    # Use compensated data for subsequent steps
    current_data <- results$compensated_data
  } else {
    results$compensated_data <- input_data
    results$metrics$compensation <- list(
      applied = FALSE,
      message = "Compensation not requested or no spillover matrix provided"
    )
    current_data <- input_data
  }
  
  # Step 2: Quality Control
  if (params$perform_qc) {
    qc_result <- performQC(current_data, params$qc_settings)
    results$qc_data <- qc_result$data
    results$metrics$qc <- qc_result$metrics
  } else {
    results$qc_data <- current_data
  }
  
  # Step 3: Gating
  if (params$perform_gating) {
    gating_result <- performGating(results$qc_data, params$gates)
    results$gated_data <- gating_result$data
    results$metrics$gating <- gating_result$metrics
  } else {
    results$gated_data <- results$qc_data
  }
  
  # Step 4: Transform data
  transformed_data <- transformData(
    results$gated_data, 
    params$markers, 
    params$transform, 
    params$cofactor
  )
  results$transformed_data <- transformed_data
  
  # Step 5: Sample cells
  sampled_result <- sampleCells(transformed_data, params$n_events, params$seed)
  results$sampled_data <- sampled_result$data
  results$sampled_indices <- sampled_result$indices
  
  # Step 6: Scale data
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