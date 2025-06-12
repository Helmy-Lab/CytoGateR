# Advanced Gating Helper Functions
# All libraries loaded in global.R

# Boolean Gate Helper Functions -----------------------------------------------

#' Create Boolean Gate
#' @param gs GatingSet object
#' @param parent Parent population name
#' @param alias New population name
#' @param logic Boolean logic string (e.g., "CD4+&CD8-", "CD3+|CD19+")
#' @return Updated GatingSet with boolean gate
createBooleanGate <- function(gs, parent = "root", alias, logic) {
  tryCatch({
    # Parse logic string to identify referenced populations
    populations <- extractPopulationsFromLogic(logic)
    
    # Validate that all referenced populations exist
    available_pops <- gs_get_pop_paths(gs, path = "auto")
    missing_pops <- populations[!populations %in% available_pops]
    
    if (length(missing_pops) > 0) {
      stop(paste("Population(s) not found:", paste(missing_pops, collapse = ", ")))
    }
    
    # Create boolean filter
    bool_filter <- booleanFilter(logic, filterId = alias)
    
    # Add boolean gate to GatingSet
    gs_pop_add(gs, bool_filter, parent = parent, name = alias)
    
    # Recompute statistics
    recompute(gs)
    
    return(gs)
    
  }, error = function(e) {
    stop(paste("Error creating boolean gate:", e$message))
  })
}

#' Extract population names from boolean logic string
#' @param logic Boolean logic string
#' @return Vector of population names referenced in the logic
extractPopulationsFromLogic <- function(logic) {
  # Remove boolean operators and parentheses
  clean_logic <- gsub("[\\&\\|\\!\\(\\)]", " ", logic)
  
  # Split by whitespace and remove empty strings
  populations <- trimws(strsplit(clean_logic, "\\s+")[[1]])
  populations <- populations[populations != ""]
  
  return(unique(populations))
}

# Clustering-based Gate Helper Functions -------------------------------------- 

#' Create clustering-based gate using flowClust
#' @param flowframe FlowFrame object
#' @param channels Channels to use for clustering
#' @param K Number of clusters
#' @param gate_name Name for the resulting gate
#' @param cluster_id Which cluster to gate (if NULL, creates gates for all clusters)
#' @return Gate object(s)
createClusterGate <- function(flowframe, channels, K = 2, gate_name = "Cluster", cluster_id = NULL) {
  tryCatch({
    # Extract data for clustering
    data_matrix <- exprs(flowframe)[, channels, drop = FALSE]
    
    # Remove any NA or infinite values
    complete_rows <- complete.cases(data_matrix) & 
                     apply(data_matrix, 1, function(x) all(is.finite(x)))
    data_matrix <- data_matrix[complete_rows, , drop = FALSE]
    
    if (nrow(data_matrix) < K * 10) {
      stop("Insufficient data points for clustering")
    }
    
    # Perform flowClust clustering
    cluster_result <- flowClust(data_matrix, K = K, level = 0.95)
    
    # Create gates based on cluster results
    if (is.null(cluster_id)) {
      # Create gates for all clusters
      gates <- list()
      for (i in 1:K) {
        gate_filter <- flowClust2Filter(cluster_result, cluster = i)
        gate_filter@filterId <- paste0(gate_name, "_", i)
        gates[[i]] <- gate_filter
      }
      names(gates) <- paste0(gate_name, "_", 1:K)
      return(gates)
    } else {
      # Create gate for specific cluster
      gate_filter <- flowClust2Filter(cluster_result, cluster = cluster_id)
      gate_filter@filterId <- gate_name
      return(gate_filter)
    }
    
  }, error = function(e) {
    stop(paste("Error creating cluster gate:", e$message))
  })
}

#' Create density-based gate using flowStats
#' @param flowframe FlowFrame object
#' @param channels Channels to use for density estimation
#' @param gate_name Name for the resulting gate
#' @param percentile Percentile threshold for density gating
#' @return Gate object
createDensityGate <- function(flowframe, channels, gate_name = "DensityGate", percentile = 0.95) {
  tryCatch({
    if (length(channels) == 1) {
      # 1D density gate
      data_vector <- exprs(flowframe)[, channels]
      data_vector <- data_vector[is.finite(data_vector)]
      
      # Calculate density
      density_result <- density(data_vector)
      threshold <- quantile(data_vector, percentile)
      
      # Create interval gate
      gate_matrix <- matrix(c(-Inf, threshold), 
                            nrow = 2, ncol = 1,
                            dimnames = list(c("min", "max"), channels))
      
      gate <- rectangleGate(.gate = gate_matrix, filterId = gate_name)
      
    } else if (length(channels) == 2) {
      # 2D density gate
      data_matrix <- exprs(flowframe)[, channels, drop = FALSE]
      complete_rows <- complete.cases(data_matrix)
      data_matrix <- data_matrix[complete_rows, , drop = FALSE]
      
      # Calculate 2D density and create contour
      density_2d <- MASS::kde2d(data_matrix[, 1], data_matrix[, 2])
      contour_level <- quantile(as.vector(density_2d$z), percentile)
      
      # Extract contour lines
      contour_lines <- grDevices::contourLines(density_2d, levels = contour_level)
      
      if (length(contour_lines) > 0) {
        # Use the largest contour
        largest_contour <- contour_lines[[which.max(sapply(contour_lines, function(x) length(x$x)))]]
        
        # Create polygon gate
        boundaries <- matrix(c(largest_contour$x, largest_contour$y), 
                             ncol = 2,
                             dimnames = list(NULL, channels))
        
        gate <- polygonGate(.gate = boundaries, filterId = gate_name)
      } else {
        stop("Could not create density contour")
      }
      
    } else {
      stop("Density gating supports only 1 or 2 channels")
    }
    
    return(gate)
    
  }, error = function(e) {
    stop(paste("Error creating density gate:", e$message))
  })
}

# Gate Template Management Functions -------------------------------------------

#' Load gating template from CSV file
#' @param filename CSV file path
#' @return gatingTemplate object
loadGatingTemplate <- function(filename) {
  tryCatch({
    if (!file.exists(filename)) {
      stop(paste("Template file not found:", filename))
    }
    
    # Read CSV file
    template_data <- read.csv(filename, stringsAsFactors = FALSE)
    
    # Validate template structure
    required_cols <- c("alias", "pop", "parent", "dims", "gating_method")
    missing_cols <- required_cols[!required_cols %in% colnames(template_data)]
    
    if (length(missing_cols) > 0) {
      stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    }
    
    # Create gatingTemplate object
    gt <- gatingTemplate(filename)
    
    return(gt)
    
  }, error = function(e) {
    stop(paste("Error loading gating template:", e$message))
  })
}

#' Apply gating template to GatingSet
#' @param gs GatingSet object
#' @param template_file Path to gating template CSV file or gatingTemplate object
#' @return Updated GatingSet
applyGatingTemplate <- function(gs, template_file) {
  tryCatch({
    if (is.character(template_file)) {
      gt <- loadGatingTemplate(template_file)
    } else {
      gt <- template_file
    }
    
    # Apply template using openCyto
    gt_gating(gt, gs)
    
    # Recompute all statistics
    recompute(gs)
    
    return(gs)
    
  }, error = function(e) {
    stop(paste("Error applying gating template:", e$message))
  })
}

#' Generate gating template from existing GatingSet
#' @param gs GatingSet object
#' @param filename Output CSV file path
#' @param method Method to use for template generation ("simple" or "detailed")
#' @return Data frame with template structure
generateTemplateFromGS <- function(gs, filename = NULL, method = "simple") {
  tryCatch({
    # Get all population paths
    all_pops <- gs_get_pop_paths(gs, path = "auto")
    
    if (length(all_pops) <= 1) {
      # Only root population exists
      template_data <- data.frame(
        alias = character(0),
        pop = character(0),
        parent = character(0),
        dims = character(0),
        gating_method = character(0),
        gating_args = character(0),
        collapseDataForGating = logical(0),
        groupBy = character(0),
        preprocessing_method = character(0),
        preprocessing_args = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      # Extract gate information
      template_rows <- list()
      
      for (pop in all_pops[-1]) {  # Exclude root
        gate_info <- gs_pop_get_gate(gs, pop)
        gate_obj <- gate_info[[1]]
        
        # Get parent population
        parent_pop <- gs_pop_get_parent(gs, pop)
        if (parent_pop == "") parent_pop <- "root"
        
        # Get gate parameters
        gate_params <- parameters(gate_obj)
        dims_str <- paste(gate_params, collapse = ",")
        
        # Determine gating method based on gate type
        if (method == "detailed") {
          gating_method <- determineGatingMethod(gate_obj)
          gating_args <- serializeGateArgs(gate_obj)
        } else {
          gating_method <- "cyto_gate_draw"
          gating_args <- ""
        }
        
        template_rows[[length(template_rows) + 1]] <- data.frame(
          alias = basename(pop),
          pop = "+",
          parent = basename(parent_pop),
          dims = dims_str,
          gating_method = gating_method,
          gating_args = gating_args,
          collapseDataForGating = TRUE,
          groupBy = "NA",
          preprocessing_method = if (method == "detailed") "pp_cyto_gate_draw" else "",
          preprocessing_args = "",
          stringsAsFactors = FALSE
        )
      }
      
      template_data <- do.call(rbind, template_rows)
    }
    
    # Save to file if filename provided
    if (!is.null(filename)) {
      write.csv(template_data, filename, row.names = FALSE)
    }
    
    return(template_data)
    
  }, error = function(e) {
    stop(paste("Error generating template:", e$message))
  })
}

#' Determine appropriate gating method for a gate object
#' @param gate_obj Gate object
#' @return String indicating gating method
determineGatingMethod <- function(gate_obj) {
  if (is(gate_obj, "rectangleGate")) {
    # Check if it's an interval gate (1D)
    if (length(parameters(gate_obj)) == 1) {
      return("mindensity")  # or "interval" depending on preference
    } else {
      return("rectangleGate")
    }
  } else if (is(gate_obj, "polygonGate")) {
    return("polygonGate")
  } else if (is(gate_obj, "ellipsoidGate")) {
    return("ellipsoidGate")
  } else if (is(gate_obj, "quadGate")) {
    return("quadGate")
  } else if (is(gate_obj, "booleanFilter")) {
    return("boolGate")
  } else {
    return("cyto_gate_draw")
  }
}

#' Serialize gate arguments for template
#' @param gate_obj Gate object
#' @return String representation of gate arguments
serializeGateArgs <- function(gate_obj) {
  # This is a simplified serialization
  # In practice, you might want more sophisticated serialization
  if (is(gate_obj, "booleanFilter")) {
    return(gate_obj@deparse)
  } else {
    return("gate=gate_obj")
  }
}

# Gate Validation and Quality Control Functions -------------------------------

#' Validate gate coordinates
#' @param gate_coords Coordinates from plot selection
#' @param gate_type Type of gate being created
#' @param channels Channel names
#' @return Boolean indicating if coordinates are valid
validateGateCoordinates <- function(gate_coords, gate_type, channels) {
  if (is.null(gate_coords) || nrow(gate_coords) == 0) {
    return(FALSE)
  }
  
  switch(gate_type,
    "rectangle" = {
      # Need at least 2 points for rectangle
      return(nrow(gate_coords) >= 2 && ncol(gate_coords) >= 2)
    },
    "polygon" = {
      # Need at least 3 points for polygon
      return(nrow(gate_coords) >= 3 && ncol(gate_coords) >= 2)
    },
    "interval" = {
      # Need at least 1 point for interval
      return(nrow(gate_coords) >= 1)
    },
    "threshold" = {
      # Need at least 1 point for threshold
      return(nrow(gate_coords) >= 1)
    },
    "ellipse" = {
      # Need at least 3 points to define ellipse
      return(nrow(gate_coords) >= 3 && ncol(gate_coords) >= 2)
    },
    {
      return(FALSE)
    }
  )
}

#' Check gate quality metrics
#' @param gs GatingSet object
#' @param population Population name
#' @return List with quality metrics
checkGateQuality <- function(gs, population) {
  tryCatch({
    # Get population statistics
    stats <- gs_pop_get_stats(gs, population)
    
    metrics <- list(
      event_count = stats$Count,
      parent_frequency = stats$ParentFreq,
      total_frequency = stats$TotalFreq
    )
    
    # Add warnings for low event counts
    if (metrics$event_count < 100) {
      metrics$warnings <- "Low event count (< 100 events)"
    } else if (metrics$event_count < 1000) {
      metrics$warnings <- "Moderate event count (< 1000 events)"
    } else {
      metrics$warnings <- "Good event count"
    }
    
    # Add warnings for extreme frequencies
    if (metrics$parent_frequency < 0.01) {
      metrics$frequency_warning <- "Very low frequency (< 1%)"
    } else if (metrics$parent_frequency > 0.95) {
      metrics$frequency_warning <- "Very high frequency (> 95%)"
    } else {
      metrics$frequency_warning <- "Normal frequency range"
    }
    
    return(metrics)
    
  }, error = function(e) {
    return(list(error = paste("Error checking gate quality:", e$message)))
  })
}

# Advanced Gate Operations Functions ------------------------------------------

#' Copy gate from one population to another
#' @param gs GatingSet object
#' @param source_pop Source population with gate to copy
#' @param target_parent Target parent population
#' @param new_name New name for copied gate
#' @return Updated GatingSet
copyGate <- function(gs, source_pop, target_parent, new_name) {
  tryCatch({
    # Get the gate from source population
    gate_info <- gs_pop_get_gate(gs, source_pop)
    gate_obj <- gate_info[[1]]
    
    # Create new gate with different name
    gate_obj@filterId <- new_name
    
    # Add gate to target parent
    gs_pop_add(gs, gate_obj, parent = target_parent, name = new_name)
    
    # Recompute statistics
    recompute(gs)
    
    return(gs)
    
  }, error = function(e) {
    stop(paste("Error copying gate:", e$message))
  })
}

#' Transform gate coordinates (e.g., for compensation changes)
#' @param gate_obj Gate object to transform
#' @param transform_func Transformation function
#' @return Transformed gate object
transformGate <- function(gate_obj, transform_func) {
  tryCatch({
    if (is(gate_obj, "rectangleGate")) {
      # Transform rectangle coordinates
      new_min <- transform_func(gate_obj@min)
      new_max <- transform_func(gate_obj@max)
      
      gate_matrix <- rbind(new_min, new_max)
      rownames(gate_matrix) <- c("min", "max")
      
      new_gate <- rectangleGate(.gate = gate_matrix, filterId = gate_obj@filterId)
      
    } else if (is(gate_obj, "polygonGate")) {
      # Transform polygon boundaries
      new_boundaries <- apply(gate_obj@boundaries, 1, transform_func)
      if (is.vector(new_boundaries)) {
        new_boundaries <- t(new_boundaries)
      } else {
        new_boundaries <- t(new_boundaries)
      }
      colnames(new_boundaries) <- colnames(gate_obj@boundaries)
      
      new_gate <- polygonGate(.gate = new_boundaries, filterId = gate_obj@filterId)
      
    } else {
      # For other gate types, return original gate
      warning("Gate transformation not implemented for this gate type")
      new_gate <- gate_obj
    }
    
    return(new_gate)
    
  }, error = function(e) {
    stop(paste("Error transforming gate:", e$message))
  })
}

# Gate Statistics and Analysis Functions -----------------------------------

#' Calculate comprehensive statistics for all populations
#' @param gs GatingSet object
#' @param include_mfi Whether to include MFI calculations
#' @return Data frame with population statistics
calculatePopulationStats <- function(gs, include_mfi = FALSE) {
  tryCatch({
    # Get basic statistics
    basic_stats <- gs_pop_get_stats(gs)
    
    # Add sample information
    sample_names <- sampleNames(gs)
    stats_expanded <- data.frame()
    
    for (i in seq_along(sample_names)) {
      sample_stats <- basic_stats[basic_stats$sample == sample_names[i], ]
      sample_stats$sample_name <- sample_names[i]
      stats_expanded <- rbind(stats_expanded, sample_stats)
    }
    
    # Calculate additional metrics
    stats_expanded$percent_of_parent <- stats_expanded$ParentFreq * 100
    stats_expanded$percent_of_total <- stats_expanded$TotalFreq * 100
    
    # Add MFI if requested
    if (include_mfi && ncol(exprs(gs_pop_get_data(gs)[[1]])) > 0) {
      # This would require specifying which channels to calculate MFI for
      # For now, we'll skip this to avoid complexity
      warning("MFI calculation not implemented in this version")
    }
    
    return(stats_expanded)
    
  }, error = function(e) {
    stop(paste("Error calculating population statistics:", e$message))
  })
}

#' Export gating hierarchy as a tree structure
#' @param gs GatingSet object
#' @return List representing the hierarchy tree
exportHierarchyTree <- function(gs) {
  tryCatch({
    all_pops <- gs_get_pop_paths(gs, path = "auto")
    
    # Create tree structure
    tree <- list()
    
    for (pop in all_pops) {
      path_parts <- strsplit(pop, "/")[[1]]
      current_level <- tree
      
      for (part in path_parts) {
        if (part == "") next  # Skip empty parts
        
        if (!part %in% names(current_level)) {
          current_level[[part]] <- list()
        }
        current_level <- current_level[[part]]
      }
    }
    
    return(tree)
    
  }, error = function(e) {
    stop(paste("Error exporting hierarchy tree:", e$message))
  })
} 