# Gating-specific Plotting Functions
# All libraries loaded in global.R

# Interactive Gate Drawing Plots ------------------------------------------

#' Create interactive plot for gate drawing
#' @param flowframe FlowFrame object or data from GatingSet population
#' @param x_channel X-axis channel
#' @param y_channel Y-axis channel (NULL for 1D plots)
#' @param existing_gates List of existing gates to overlay
#' @param transformation Transformation to apply
#' @param sample_size Number of events to subsample for plotting
#' @return plotly object
createInteractiveGatingPlot <- function(flowframe, x_channel, y_channel = NULL, 
                                        existing_gates = NULL, transformation = "logicle",
                                        sample_size = 10000) {
  tryCatch({
    # Extract data
    if (is(flowframe, "flowFrame")) {
      data_matrix <- exprs(flowframe)
    } else {
      data_matrix <- flowframe
    }
    
    # Subsample if necessary
    if (nrow(data_matrix) > sample_size) {
      sample_indices <- sample(nrow(data_matrix), sample_size)
      data_matrix <- data_matrix[sample_indices, , drop = FALSE]
    }
    
    # Apply transformation
    if (!is.null(transformation) && transformation != "linear") {
      data_matrix <- applyTransformation(data_matrix, transformation)
    }
    
    # Create data frame
    plot_data <- data.frame(
      x = data_matrix[, x_channel],
      y = if (!is.null(y_channel)) data_matrix[, y_channel] else rep(0, nrow(data_matrix))
    )
    
    # Remove infinite values
    plot_data <- plot_data[is.finite(plot_data$x) & is.finite(plot_data$y), ]
    
    if (is.null(y_channel)) {
      # 1D histogram
      p <- ggplot(plot_data, aes(x = x)) +
        geom_histogram(bins = 50, alpha = 0.7, fill = "steelblue") +
        labs(x = x_channel, y = "Count") +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white")
        )
      
    } else {
      # 2D density plot
      p <- ggplot(plot_data, aes(x = x, y = y)) +
        geom_hex(bins = 50, alpha = 0.8) +
        scale_fill_gradient(low = "lightblue", high = "darkblue") +
        labs(x = x_channel, y = y_channel) +
        theme_minimal() +
        theme(
          plot.background = element_rect(fill = "white"),
          panel.background = element_rect(fill = "white"),
          legend.position = "right"
        )
    }
    
    # Add existing gates if provided
    if (!is.null(existing_gates) && length(existing_gates) > 0) {
      p <- addGatesToPlot(p, existing_gates, x_channel, y_channel)
    }
    
    # Convert to plotly for interactivity
    ply <- ggplotly(p, tooltip = c("x", "y")) %>%
      config(
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("zoom2d", "pan2d", "select2d", "lasso2d", 
                                   "zoomIn2d", "zoomOut2d", "autoScale2d",
                                   "resetScale2d", "hoverClosestCartesian", 
                                   "hoverCompareCartesian"),
        displaylogo = FALSE
      ) %>%
      layout(
        dragmode = "select",
        selectdirection = "any"
      )
    
    return(ply)
    
  }, error = function(e) {
    stop(paste("Error creating interactive gating plot:", e$message))
  })
}

#' Add existing gates to a ggplot
#' @param p ggplot object
#' @param gates List of gate objects
#' @param x_channel X-axis channel
#' @param y_channel Y-axis channel
#' @return Updated ggplot object
addGatesToPlot <- function(p, gates, x_channel, y_channel = NULL) {
  for (gate in gates) {
    if (is(gate, "rectangleGate")) {
      # Rectangle gate
      if (is.null(y_channel)) {
        # 1D interval
        x_min <- gate@min[x_channel]
        x_max <- gate@max[x_channel]
        p <- p + geom_vline(xintercept = c(x_min, x_max), 
                           color = "red", linetype = "dashed", size = 1)
      } else {
        # 2D rectangle
        x_min <- gate@min[x_channel]
        x_max <- gate@max[x_channel]
        y_min <- gate@min[y_channel]
        y_max <- gate@max[y_channel]
        
        rect_data <- data.frame(
          x = c(x_min, x_max, x_max, x_min, x_min),
          y = c(y_min, y_min, y_max, y_max, y_min)
        )
        
        p <- p + geom_path(data = rect_data, aes(x = x, y = y), 
                          color = "red", size = 1, inherit.aes = FALSE)
      }
      
    } else if (is(gate, "polygonGate")) {
      # Polygon gate
      if (!is.null(y_channel)) {
        boundaries <- gate@boundaries
        if (x_channel %in% colnames(boundaries) && y_channel %in% colnames(boundaries)) {
          poly_data <- data.frame(
            x = boundaries[, x_channel],
            y = boundaries[, y_channel]
          )
          # Close the polygon
          poly_data <- rbind(poly_data, poly_data[1, ])
          
          p <- p + geom_path(data = poly_data, aes(x = x, y = y), 
                            color = "red", size = 1, inherit.aes = FALSE)
        }
      }
      
    } else if (is(gate, "ellipsoidGate")) {
      # Ellipsoid gate
      if (!is.null(y_channel)) {
        # This is a simplified ellipse drawing - in practice you'd want more sophisticated ellipse calculation
        center <- gate@mean[c(x_channel, y_channel)]
        # For simplicity, draw as circle - proper ellipse requires eigenvalue decomposition
        if (length(center) == 2) {
          p <- p + annotate("point", x = center[1], y = center[2], 
                           color = "red", size = 3, shape = 4)
        }
      }
    }
  }
  
  return(p)
}

# Population Visualization Functions --------------------------------------

#' Create gating hierarchy tree plot
#' @param gs GatingSet object
#' @param population_stats Data frame with population statistics
#' @return ggplot object
createHierarchyTreePlot <- function(gs, population_stats = NULL) {
  tryCatch({
    # Get all population paths
    all_pops <- gs_get_pop_paths(gs, path = "auto")
    
    if (is.null(population_stats)) {
      population_stats <- gs_pop_get_stats(gs)
    }
    
    # Create tree structure data
    tree_data <- data.frame()
    
    for (pop in all_pops) {
      # Get depth level
      depth <- length(strsplit(pop, "/")[[1]]) - 1
      if (pop == "/") depth <- 0
      
      # Get population name
      pop_name <- basename(pop)
      if (pop_name == "" || pop == "/") pop_name <- "root"
      
      # Get parent
      parent <- dirname(pop)
      if (parent == "/" || parent == ".") parent <- "root"
      else parent <- basename(parent)
      
      # Get statistics
      if (pop %in% population_stats$pop) {
        stats <- population_stats[population_stats$pop == pop, ]
        if (nrow(stats) > 0) {
          count <- stats$Count[1]
          freq <- stats$ParentFreq[1] * 100
        } else {
          count <- NA
          freq <- NA
        }
      } else {
        count <- NA
        freq <- NA
      }
      
      tree_data <- rbind(tree_data, data.frame(
        population = pop_name,
        parent = parent,
        depth = depth,
        count = count,
        frequency = freq,
        full_path = pop,
        stringsAsFactors = FALSE
      ))
    }
    
    # Create positions for tree layout
    tree_data$x <- NA
    tree_data$y <- -tree_data$depth
    
    # Simple layout: spread populations at each level
    for (d in unique(tree_data$depth)) {
      level_pops <- tree_data[tree_data$depth == d, ]
      n_pops <- nrow(level_pops)
      if (n_pops > 0) {
        tree_data[tree_data$depth == d, "x"] <- seq(-n_pops/2, n_pops/2, length.out = n_pops)
      }
    }
    
    # Create node labels
    tree_data$label <- paste0(
      tree_data$population,
      if (!all(is.na(tree_data$count))) 
        paste0("\n(", formatC(tree_data$count, format = "d", big.mark = ","), " events)")
      else "",
      if (!all(is.na(tree_data$frequency))) 
        paste0("\n", round(tree_data$frequency, 1), "%")
      else ""
    )
    
    # Create the tree plot
    p <- ggplot(tree_data, aes(x = x, y = y)) +
      geom_point(aes(size = count, color = frequency), alpha = 0.8) +
      geom_text(aes(label = label), vjust = -0.5, size = 3) +
      scale_size_continuous(name = "Events", trans = "log10", 
                           labels = function(x) formatC(x, format = "d", big.mark = ",")) +
      scale_color_gradient(name = "Frequency (%)", low = "lightblue", high = "darkred") +
      labs(title = "Gating Hierarchy Tree", x = "", y = "Gating Level") +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")
      )
    
    # Add connecting lines between parent and child populations
    for (i in 1:nrow(tree_data)) {
      if (tree_data$parent[i] != "root" && tree_data$parent[i] != tree_data$population[i]) {
        parent_row <- tree_data[tree_data$population == tree_data$parent[i], ]
        if (nrow(parent_row) > 0) {
          p <- p + geom_segment(
            x = parent_row$x[1], y = parent_row$y[1],
            xend = tree_data$x[i], yend = tree_data$y[i],
            color = "gray50", alpha = 0.6
          )
        }
      }
    }
    
    return(p)
    
  }, error = function(e) {
    stop(paste("Error creating hierarchy tree plot:", e$message))
  })
}

#' Create population comparison plot
#' @param gs GatingSet object
#' @param populations Vector of population names to compare
#' @param channels Vector of channels to compare
#' @param plot_type Type of comparison plot ("density", "histogram", "violin")
#' @return ggplot object
createPopulationComparisonPlot <- function(gs, populations, channels, plot_type = "density") {
  tryCatch({
    # Extract data for each population
    plot_data <- data.frame()
    
    for (pop in populations) {
      pop_data <- gs_pop_get_data(gs, pop)
      
      for (sample_name in names(pop_data)) {
        sample_data <- exprs(pop_data[[sample_name]])
        
        for (channel in channels) {
          if (channel %in% colnames(sample_data)) {
            channel_data <- sample_data[, channel]
            channel_data <- channel_data[is.finite(channel_data)]
            
            pop_df <- data.frame(
              value = channel_data,
              population = pop,
              channel = channel,
              sample = sample_name,
              stringsAsFactors = FALSE
            )
            
            plot_data <- rbind(plot_data, pop_df)
          }
        }
      }
    }
    
    if (nrow(plot_data) == 0) {
      stop("No data found for specified populations and channels")
    }
    
    # Create comparison plot based on type
    if (plot_type == "density") {
      p <- ggplot(plot_data, aes(x = value, color = population, fill = population)) +
        geom_density(alpha = 0.3) +
        facet_wrap(~ channel, scales = "free") +
        scale_fill_brewer(type = "qual", palette = "Set2") +
        scale_color_brewer(type = "qual", palette = "Set2") +
        labs(title = "Population Density Comparison", x = "Fluorescence Intensity", y = "Density") +
        theme_minimal()
        
    } else if (plot_type == "histogram") {
      p <- ggplot(plot_data, aes(x = value, fill = population)) +
        geom_histogram(alpha = 0.7, position = "identity", bins = 50) +
        facet_wrap(~ channel, scales = "free") +
        scale_fill_brewer(type = "qual", palette = "Set2") +
        labs(title = "Population Histogram Comparison", x = "Fluorescence Intensity", y = "Count") +
        theme_minimal()
        
    } else if (plot_type == "violin") {
      p <- ggplot(plot_data, aes(x = population, y = value, fill = population)) +
        geom_violin(alpha = 0.7) +
        geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
        facet_wrap(~ channel, scales = "free_y") +
        scale_fill_brewer(type = "qual", palette = "Set2") +
        labs(title = "Population Distribution Comparison", x = "Population", y = "Fluorescence Intensity") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
    } else {
      stop("Unsupported plot type. Use 'density', 'histogram', or 'violin'.")
    }
    
    p <- p + theme(
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      legend.position = "bottom"
    )
    
    return(p)
    
  }, error = function(e) {
    stop(paste("Error creating population comparison plot:", e$message))
  })
}

# Gate Quality Visualization Functions --------------------------------

#' Create gate quality assessment plot
#' @param gs GatingSet object
#' @param population Population to assess
#' @param channels Channels to visualize
#' @return ggplot object
createGateQualityPlot <- function(gs, population, channels) {
  tryCatch({
    # Get parent and child population data
    parent_pop <- gs_pop_get_parent(gs, population)
    
    parent_data <- gs_pop_get_data(gs, parent_pop)
    child_data <- gs_pop_get_data(gs, population)
    
    if (length(parent_data) == 0 || length(child_data) == 0) {
      stop("No data found for parent or child population")
    }
    
    # Use first sample for visualization
    parent_exprs <- exprs(parent_data[[1]])
    child_exprs <- exprs(child_data[[1]])
    
    # Create combined data frame
    plot_data <- data.frame()
    
    for (channel in channels[1:min(2, length(channels))]) {  # Limit to 2 channels for visualization
      if (channel %in% colnames(parent_exprs)) {
        # Parent population data
        parent_df <- data.frame(
          value = parent_exprs[, channel],
          population = "Parent",
          channel = channel,
          stringsAsFactors = FALSE
        )
        
        # Child population data
        if (channel %in% colnames(child_exprs)) {
          child_df <- data.frame(
            value = child_exprs[, channel],
            population = "Gated",
            channel = channel,
            stringsAsFactors = FALSE
          )
          
          plot_data <- rbind(plot_data, parent_df, child_df)
        }
      }
    }
    
    if (nrow(plot_data) == 0) {
      stop("No valid data found for specified channels")
    }
    
    # Remove infinite values
    plot_data <- plot_data[is.finite(plot_data$value), ]
    
    # Create overlay density plot
    p <- ggplot(plot_data, aes(x = value, color = population, fill = population)) +
      geom_density(alpha = 0.3, size = 1) +
      facet_wrap(~ channel, scales = "free") +
      scale_color_manual(values = c("Parent" = "blue", "Gated" = "red")) +
      scale_fill_manual(values = c("Parent" = "blue", "Gated" = "red")) +
      labs(
        title = paste("Gate Quality Assessment:", population),
        subtitle = "Overlay of parent and gated populations",
        x = "Fluorescence Intensity",
        y = "Density"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"),
        legend.position = "bottom"
      )
    
    return(p)
    
  }, error = function(e) {
    stop(paste("Error creating gate quality plot:", e$message))
  })
}

# Transformation Helper Functions ----------------------------------------

#' Apply transformation to data matrix
#' @param data_matrix Numeric matrix of flow cytometry data
#' @param transformation Type of transformation ("linear", "log", "logicle", "arcsinh")
#' @return Transformed data matrix
applyTransformation <- function(data_matrix, transformation = "logicle") {
  if (transformation == "linear") {
    return(data_matrix)
  } else if (transformation == "log") {
    # Add small constant to avoid log(0)
    return(log10(data_matrix + 1))
  } else if (transformation == "logicle") {
    # Apply logicle transformation (requires flowCore)
    if (requireNamespace("flowCore", quietly = TRUE)) {
      # Simple logicle approximation
      return(asinh(data_matrix / 150))
    } else {
      warning("flowCore not available, using arcsinh transformation")
      return(asinh(data_matrix / 150))
    }
  } else if (transformation == "arcsinh") {
    return(asinh(data_matrix / 150))
  } else {
    warning("Unknown transformation, using linear")
    return(data_matrix)
  }
}

#' Create transformation preview plot
#' @param data_vector Numeric vector of data
#' @param transformations Vector of transformation names to compare
#' @param channel_name Name of the channel for labeling
#' @return ggplot object
createTransformationPreview <- function(data_vector, transformations = c("linear", "log", "logicle", "arcsinh"), 
                                       channel_name = "Channel") {
  # Remove infinite and missing values
  data_vector <- data_vector[is.finite(data_vector) & data_vector > 0]
  
  if (length(data_vector) == 0) {
    stop("No valid data points for transformation preview")
  }
  
  # Create comparison data
  transform_data <- data.frame()
  
  for (trans in transformations) {
    transformed <- applyTransformation(matrix(data_vector, ncol = 1), trans)[, 1]
    
    trans_df <- data.frame(
      value = transformed,
      transformation = trans,
      stringsAsFactors = FALSE
    )
    
    transform_data <- rbind(transform_data, trans_df)
  }
  
  # Create comparison plot
  p <- ggplot(transform_data, aes(x = value, color = transformation)) +
    geom_density(size = 1, alpha = 0.8) +
    facet_wrap(~ transformation, scales = "free") +
    scale_color_brewer(type = "qual", palette = "Set1") +
    labs(
      title = paste("Transformation Comparison:", channel_name),
      x = "Transformed Value",
      y = "Density"
    ) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white"),
      panel.background = element_rect(fill = "white"),
      legend.position = "none"
    )
  
  return(p)
} 