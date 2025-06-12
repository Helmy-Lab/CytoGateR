# Advanced Gating Helper Functions
# All libraries loaded in global.R

# Helper function to add existing gates to plots
addExistingGates <- function(p, gs, sample_idx, parent, x_channel, y_channel) {
  tryCatch({
    # Get existing gates for this parent population
    if (parent == "root") {
      children <- gs_pop_get_children(gs[[sample_idx]], path = "auto")
    } else {
      children <- gs_pop_get_children(gs[[sample_idx]], parent, path = "auto")
    }
    
    if (length(children) > 0) {
      for (child in children) {
        tryCatch({
          gate <- gs_pop_get_gate(gs, child)[[1]]
          
          # Add gate visualization based on gate type
          if (is(gate, "rectangleGate")) {
            p <- addRectangleGateToPlot(p, gate, x_channel, y_channel)
          } else if (is(gate, "polygonGate")) {
            p <- addPolygonGateToPlot(p, gate, x_channel, y_channel)
          } else if (is(gate, "ellipsoidGate")) {
            p <- addEllipseGateToPlot(p, gate, x_channel, y_channel)
          }
          
        }, error = function(e) {
          # Ignore individual gate visualization errors
        })
      }
    }
  }, error = function(e) {
    # Return original plot if error
  })
  return(p)
}

# Add rectangle gate visualization
addRectangleGateToPlot <- function(p, gate, x_channel, y_channel) {
  tryCatch({
    gate_params <- parameters(gate)
    
    if (x_channel %in% gate_params && y_channel %in% gate_params) {
      x_min <- gate@min[x_channel]
      x_max <- gate@max[x_channel]
      y_min <- gate@min[y_channel]
      y_max <- gate@max[y_channel]
      
      p <- p + 
        annotate("rect", 
                 xmin = x_min, xmax = x_max,
                 ymin = y_min, ymax = y_max,
                 fill = NA, color = "red", linewidth = 1.5, alpha = 0.8) +
        annotate("text", x = (x_min + x_max)/2, y = (y_min + y_max)/2,
                 label = gate@filterId, color = "red", size = 3)
    }
    
    return(p)
    
  }, error = function(e) {
    return(p)
  })
}

# Add polygon gate visualization
addPolygonGateToPlot <- function(p, gate, x_channel, y_channel) {
  tryCatch({
    gate_params <- parameters(gate)
    
    if (x_channel %in% gate_params && y_channel %in% gate_params) {
      boundaries <- gate@boundaries
      poly_data <- data.frame(
        x = boundaries[, x_channel],
        y = boundaries[, y_channel]
      )
      
      # Close the polygon
      poly_data <- rbind(poly_data, poly_data[1, ])
      
      p <- p + 
        geom_path(data = poly_data, aes(x = x, y = y),
                  color = "red", linewidth = 1.5, alpha = 0.8) +
        annotate("text", x = mean(poly_data$x), y = mean(poly_data$y),
                 label = gate@filterId, color = "red", size = 3)
    }
    
    return(p)
    
  }, error = function(e) {
    return(p)
  })
}

# Add ellipse gate visualization
addEllipseGateToPlot <- function(p, gate, x_channel, y_channel) {
  tryCatch({
    gate_params <- parameters(gate)
    
    if (x_channel %in% gate_params && y_channel %in% gate_params) {
      # Extract ellipse parameters
      mean_vals <- gate@mean
      cov_matrix <- gate@cov
      distance <- gate@distance
      
      # Generate ellipse points
      ellipse_points <- generateEllipsePoints(mean_vals, cov_matrix, distance)
      colnames(ellipse_points) <- c("x", "y")
      ellipse_df <- as.data.frame(ellipse_points)
      
      p <- p + 
        geom_path(data = ellipse_df, aes(x = x, y = y),
                  color = "red", linewidth = 1.5, alpha = 0.8) +
        annotate("text", x = mean_vals[x_channel], y = mean_vals[y_channel],
                 label = gate@filterId, color = "red", size = 3)
    }
    
    return(p)
    
  }, error = function(e) {
    return(p)
  })
}

# Generate ellipse points for visualization
generateEllipsePoints <- function(center, cov_matrix, distance, n_points = 100) {
  tryCatch({
    # Eigenvalue decomposition
    eig <- eigen(cov_matrix)
    
    # Generate circle points
    theta <- seq(0, 2*pi, length.out = n_points)
    circle <- cbind(cos(theta), sin(theta))
    
    # Scale by square root of eigenvalues and distance
    scaled <- circle %*% diag(sqrt(eig$values * distance))
    
    # Rotate by eigenvectors
    rotated <- scaled %*% t(eig$vectors)
    
    # Translate to center
    ellipse <- sweep(rotated, 2, center, "+")
    
    return(ellipse)
    
  }, error = function(e) {
    # Return simple circle if ellipse generation fails
    theta <- seq(0, 2*pi, length.out = n_points)
    radius <- sqrt(distance)
    circle <- cbind(cos(theta) * radius + center[1], 
                    sin(theta) * radius + center[2])
    return(circle)
  })
}

# Advanced gate creation functions

# Create polygon gate from coordinates
createPolygonGate <- function(coords, x_channel, y_channel, alias) {
  tryCatch({
    if (nrow(coords) < 3) {
      stop("A minimum of 3 points is required to construct a polygon gate.")
    }
    
    # Prepare gate matrix
    gate_matrix <- as.matrix(coords[, c("x", "y")])
    colnames(gate_matrix) <- c(x_channel, y_channel)
    
    # Create polygon gate
    gate <- polygonGate(.gate = gate_matrix, filterId = alias)
    
    return(gate)
    
  }, error = function(e) {
    stop(paste("Error creating polygon gate:", e$message))
  })
}

# Create rectangle gate from coordinates
createRectangleGate <- function(coords, x_channel, y_channel, alias) {
  tryCatch({
    if (nrow(coords) < 2) {
      stop("A minimum of 2 points is required to construct a rectangle gate.")
    }
    
    # Calculate bounds
    min_x <- min(coords$x)
    max_x <- max(coords$x)
    min_y <- min(coords$y) 
    max_y <- max(coords$y)
    
    # Create gate matrix
    gate_matrix <- matrix(c(min_x, max_x, min_y, max_y), 
                          ncol = 2, nrow = 2,
                          dimnames = list(c("min", "max"), c(x_channel, y_channel)))
    
    # Create rectangle gate
    gate <- rectangleGate(.gate = gate_matrix, filterId = alias)
    
    return(gate)
    
  }, error = function(e) {
    stop(paste("Error creating rectangle gate:", e$message))
  })
}

# Create ellipse gate from coordinates
createEllipseGate <- function(coords, x_channel, y_channel, alias) {
  tryCatch({
    if (nrow(coords) < 3) {
      stop("A minimum of 3 points is required to construct an ellipse gate.")
    }
    
    # Calculate center and covariance
    center <- c(mean(coords$x), mean(coords$y))
    names(center) <- c(x_channel, y_channel)
    
    # Calculate covariance matrix
    data_matrix <- as.matrix(coords[, c("x", "y")])
    colnames(data_matrix) <- c(x_channel, y_channel)
    cov_matrix <- cov(data_matrix)
    
    # Set distance (adjust as needed)
    distance <- 4  # Mahalanobis distance
    
    # Create ellipse gate
    gate <- ellipsoidGate(.gate = list(mu = center, sigma = cov_matrix, distance = distance),
                          filterId = alias)
    
    return(gate)
    
  }, error = function(e) {
    stop(paste("Error creating ellipse gate:", e$message))
  })
}

# Create interval gate (1D)
createIntervalGate <- function(coords, channel, alias) {
  tryCatch({
    if (nrow(coords) < 2) {
      stop("A minimum of 2 points is required to construct an interval gate.")
    }
    
    # Calculate bounds
    min_val <- min(coords$x)
    max_val <- max(coords$x)
    
    # Create gate matrix
    gate_matrix <- matrix(c(min_val, max_val), 
                          ncol = 1, nrow = 2,
                          dimnames = list(c("min", "max"), c(channel)))
    
    # Create rectangle gate (1D)
    gate <- rectangleGate(.gate = gate_matrix, filterId = alias)
    
    return(gate)
    
  }, error = function(e) {
    stop(paste("Error creating interval gate:", e$message))
  })
}

# Create threshold gate
createThresholdGate <- function(coords, channel, alias, direction = "above") {
  tryCatch({
    if (nrow(coords) < 1) {
      stop("At least 1 point is required to construct a threshold gate.")
    }
    
    # Calculate threshold value
    threshold_val <- median(coords$x)
    
    # Create gate matrix based on direction
    if (direction == "above") {
      gate_matrix <- matrix(c(threshold_val, Inf), 
                            ncol = 1, nrow = 2,
                            dimnames = list(c("min", "max"), c(channel)))
    } else {
      gate_matrix <- matrix(c(-Inf, threshold_val), 
                            ncol = 1, nrow = 2,
                            dimnames = list(c("min", "max"), c(channel)))
    }
    
    # Create rectangle gate
    gate <- rectangleGate(.gate = gate_matrix, filterId = alias)
    
    return(gate)
    
  }, error = function(e) {
    stop(paste("Error creating threshold gate:", e$message))
  })
}

# Create boundary gate (opposite of threshold)
createBoundaryGate <- function(coords, channel, alias) {
  tryCatch({
    # Use threshold gate with "below" direction
    return(createThresholdGate(coords, channel, alias, direction = "below"))
    
  }, error = function(e) {
    stop(paste("Error creating boundary gate:", e$message))
  })
}

# Create quadrant gates
createQuadrantGates <- function(coords, x_channel, y_channel, alias_prefix) {
  tryCatch({
    if (nrow(coords) < 1) {
      stop("At least 1 point is required to construct quadrant gates.")
    }
    
    # Calculate crosshair position
    x_split <- median(coords$x)
    y_split <- median(coords$y)
    
    # Create four quadrant gates
    gates <- list()
    
    # Q1: ++ (upper right)
    gates[[paste0(alias_prefix, "_Q1_", x_channel, "+", y_channel, "+")]] <- 
      rectangleGate(.gate = matrix(c(x_split, Inf, y_split, Inf), 
                                   ncol = 2, nrow = 2,
                                   dimnames = list(c("min", "max"), c(x_channel, y_channel))),
                    filterId = paste0(alias_prefix, "_Q1"))
    
    # Q2: -+ (upper left)
    gates[[paste0(alias_prefix, "_Q2_", x_channel, "-", y_channel, "+")]] <- 
      rectangleGate(.gate = matrix(c(-Inf, x_split, y_split, Inf), 
                                   ncol = 2, nrow = 2,
                                   dimnames = list(c("min", "max"), c(x_channel, y_channel))),
                    filterId = paste0(alias_prefix, "_Q2"))
    
    # Q3: -- (lower left)
    gates[[paste0(alias_prefix, "_Q3_", x_channel, "-", y_channel, "-")]] <- 
      rectangleGate(.gate = matrix(c(-Inf, x_split, -Inf, y_split), 
                                   ncol = 2, nrow = 2,
                                   dimnames = list(c("min", "max"), c(x_channel, y_channel))),
                    filterId = paste0(alias_prefix, "_Q3"))
    
    # Q4: +- (lower right)
    gates[[paste0(alias_prefix, "_Q4_", x_channel, "+", y_channel, "-")]] <- 
      rectangleGate(.gate = matrix(c(x_split, Inf, -Inf, y_split), 
                                   ncol = 2, nrow = 2,
                                   dimnames = list(c("min", "max"), c(x_channel, y_channel))),
                    filterId = paste0(alias_prefix, "_Q4"))
    
    return(gates)
    
  }, error = function(e) {
    stop(paste("Error creating quadrant gates:", e$message))
  })
}

# Boolean gate helpers (simplified implementation)

# Create boolean gate from expression
createBooleanGate <- function(expression, population_names, alias) {
  tryCatch({
    # This is a simplified implementation
    # In a full implementation, you would parse the expression
    # and create proper boolean filters
    
    # For now, return a placeholder
    showNotification("Boolean gate creation is not yet fully implemented", 
                     type = "message")
    return(NULL)
    
  }, error = function(e) {
    stop(paste("Error creating boolean gate:", e$message))
  })
}

# Population statistics helpers

# Calculate gate center for labeling
calculateGateCenter <- function(gate, channels) {
  tryCatch({
    if (is(gate, "rectangleGate")) {
      x_center <- mean(c(gate@min[channels[1]], gate@max[channels[1]]))
      y_center <- mean(c(gate@min[channels[2]], gate@max[channels[2]]))
      return(c(x = x_center, y = y_center))
      
    } else if (is(gate, "polygonGate")) {
      boundaries <- gate@boundaries
      x_center <- mean(boundaries[, channels[1]])
      y_center <- mean(boundaries[, channels[2]])
      return(c(x = x_center, y = y_center))
      
    } else if (is(gate, "ellipsoidGate")) {
      return(gate@mean[channels])
    }
    
    return(c(x = 0, y = 0))
    
  }, error = function(e) {
    return(c(x = 0, y = 0))
  })
}

# Calculate population percentage
calculatePopulationPercentage <- function(gs, sample_idx, population) {
  tryCatch({
    if (population == "root") {
      return(100)
    }
    
    # Get population counts
    pop_counts <- gs_pop_get_count_fast(gs[sample_idx])
    
    if (population %in% pop_counts$Population) {
      parent_count <- pop_counts$Count[pop_counts$Population == "root"]
      pop_count <- pop_counts$Count[pop_counts$Population == population]
      
      if (parent_count > 0) {
        return(round((pop_count / parent_count) * 100, 2))
      }
    }
    
    return(0)
    
  }, error = function(e) {
    return(0)
  })
}

# Export functions for gating templates

# Convert GatingSet to gating template
convertToGatingTemplate <- function(gs, template_name = "gating_template") {
  tryCatch({
    # Get population paths
    pop_paths <- gs_get_pop_paths(gs, path = "auto")
    
    if (length(pop_paths) <= 1) {
      # Only root exists
      return(data.frame(
        alias = character(0),
        pop = character(0),
        parent = character(0),
        dims = character(0),
        gating_method = character(0),
        gating_args = character(0),
        collapseDataForGating = logical(0),
        groupBy = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    # Create template structure
    template_data <- data.frame(
      alias = pop_paths[-1],  # Exclude root
      pop = rep("+", length(pop_paths) - 1),
      parent = rep("root", length(pop_paths) - 1),  # Simplified
      dims = rep("", length(pop_paths) - 1),
      gating_method = rep("cyto_gate_draw", length(pop_paths) - 1),
      gating_args = rep("", length(pop_paths) - 1),
      collapseDataForGating = rep(TRUE, length(pop_paths) - 1),
      groupBy = rep("", length(pop_paths) - 1),
      stringsAsFactors = FALSE
    )
    
    return(template_data)
    
  }, error = function(e) {
    stop(paste("Error converting to gating template:", e$message))
  })
} 