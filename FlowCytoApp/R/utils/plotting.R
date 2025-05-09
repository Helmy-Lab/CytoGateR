# Plotting utility functions for Flow Cytometry Analysis

# Function to create a dimensionality reduction plot with ggplot2
createDimReductionPlot <- function(plot_data, dim1, dim2, colorBy = NULL, 
                                   color_palette = "viridis", point_size = 3, font_size = 12,
                                   title = NULL, xlab = NULL, ylab = NULL) {
  
  # Set default axis labels if not provided
  if (is.null(xlab)) xlab <- dim1
  if (is.null(ylab)) ylab <- dim2
  
  # Create plot based on coloring variable
  if (!is.null(colorBy) && colorBy != "None" && colorBy != "Cluster" && 
      colorBy %in% colnames(plot_data)) {
    # Color by numeric marker expression
    p <- ggplot(plot_data, aes(x = .data[[dim1]], y = .data[[dim2]])) +
      geom_point(aes(color = .data[[colorBy]]), alpha = 0.7, size = point_size/2) +
      scale_color_viridis_c(name = colorBy) +
      labs(title = title, x = xlab, y = ylab)
  } 
  else if (!is.null(colorBy) && colorBy == "Cluster" && "Cluster" %in% colnames(plot_data)) {
    # Color by cluster
    p <- ggplot(plot_data, aes(x = .data[[dim1]], y = .data[[dim2]])) +
      geom_point(aes(color = Cluster), alpha = 0.7, size = point_size/2) +
      labs(title = title, x = xlab, y = ylab, color = "Cluster")
    
    # Apply appropriate color palette
    if (color_palette == "viridis") {
      p <- p + scale_color_viridis_d()
    } else if (color_palette == "plasma") {
      p <- p + scale_color_viridis_d(option = "plasma")
    } else if (color_palette == "magma") {
      p <- p + scale_color_viridis_d(option = "magma")
    } else if (color_palette == "inferno") {
      p <- p + scale_color_viridis_d(option = "inferno")
    } else if (color_palette == "blues") {
      p <- p + scale_color_brewer(palette = "Blues")
    } else if (color_palette == "reds") {
      p <- p + scale_color_brewer(palette = "Reds")
    } else if (color_palette == "brewer_paired") {
      p <- p + scale_color_brewer(palette = "Paired")
    } else if (color_palette == "brewer_brbg") {
      p <- p + scale_color_brewer(palette = "BrBG")
    }
  }
  else {
    # No coloring
    p <- ggplot(plot_data, aes(x = .data[[dim1]], y = .data[[dim2]])) +
      geom_point(alpha = 0.7, size = point_size/2, color = "#3366CC") +
      labs(title = title, x = xlab, y = ylab)
  }
  
  # Apply standard theme
  p <- p + theme_minimal(base_size = font_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey80", fill = NA)
    )
  
  return(p)
}

# Function to convert ggplot to plotly with standard settings
convertToPlotly <- function(ggplot_obj, width = 800, height = 600, font_size = 12) {
  p <- ggplotly(ggplot_obj, width = width, height = height) %>%
    layout(
      hoverlabel = list(
        bgcolor = "white",
        font = list(family = "Arial", size = font_size)
      ),
      margin = list(l = 50, r = 50, b = 50, t = 50)
    )
  
  return(p)
}

# Function to create a heatmap of cluster expression profiles
createClusterHeatmap <- function(centers, method = "Hierarchical", 
                                 title = "Cluster Expression Profiles",
                                 font_size = 12, cluster_rows = TRUE,
                                 population_data = NULL) {
  
  # Convert matrix to data frame for ggplot
  centers_df <- as.data.frame(centers) %>%
    tibble::rownames_to_column("Cluster") %>%
    reshape2::melt(id.vars = "Cluster", variable.name = "Marker", value.name = "Expression")
  
  # If population data is available, use it to modify cluster labels
  if (!is.null(population_data) && "Population" %in% colnames(population_data)) {
    # Create a mapping from cluster to population
    pop_mapping <- setNames(
      population_data$Population,
      as.character(population_data$Cluster)
    )
    
    # Add population labels to centers_df
    centers_df$ClusterLabel <- sapply(centers_df$Cluster, function(clust) {
      cluster_num <- gsub("Cluster ", "", clust)
      if (cluster_num %in% names(pop_mapping)) {
        paste0(clust, " (", pop_mapping[cluster_num], ")")
      } else {
        clust
      }
    })
  } else {
    # If no population data, use cluster as is
    centers_df$ClusterLabel <- centers_df$Cluster
  }
  
  # Create heatmap
  p <- ggplot(centers_df, aes(x = Marker, y = ClusterLabel, fill = Expression)) +
    geom_tile() +
    scale_fill_gradient2(low = "navy", mid = "white", high = "firebrick3") +
    labs(
      title = title,
      subtitle = paste("Clustering method:", method),
      x = "Markers", 
      y = "Clusters",
      fill = "Expression"
    ) +
    theme_minimal(base_size = font_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(face = "bold"),
      panel.grid = element_blank()
    )
  
  return(p)
}

# Function to create a cluster comparison heatmap between two samples
createClusterComparisonHeatmap <- function(control_centers, treated_centers, 
                                           control_pops = NULL, treated_pops = NULL,
                                           font_size = 12) {
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
  if (!is.null(control_pops) && !is.null(treated_pops)) {
    # Get population mappings
    control_pop_map <- setNames(
      control_pops$Population,
      paste0("C", control_pops$Cluster)
    )
    
    treated_pop_map <- setNames(
      treated_pops$Population,
      paste0("T", treated_pops$Cluster)
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
  
  # Create heatmap
  p <- ggplot(sim_df, aes(x = Treated, y = Control, fill = Similarity)) +
    geom_tile() +
    scale_fill_viridis_c() +
    theme_minimal(base_size = font_size) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = font_size * 0.8),
      axis.text.y = element_text(size = font_size * 0.8),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = font_size * 1.2)
    ) +
    labs(
      title = "Cluster Similarity Between Control and Treated Samples",
      fill = "Similarity"
    ) +
    geom_text(aes(label = sprintf("%.2f", Similarity)), size = font_size * 0.25)
  
  return(p)
}

# Function to create signature marker heatmap for comparing samples
createSignatureMarkerHeatmap <- function(control_centers, treated_centers, 
                                         markers, control_pops = NULL, treated_pops = NULL,
                                         font_size = 12, control_name = "Control", 
                                         treated_name = "Treated") {
  # Create combined centers data frame
  control_df <- as.data.frame(control_centers) %>%
    tibble::rownames_to_column("Cluster") %>%
    mutate(Sample = control_name, 
           ClusterID = paste0("C", Cluster))
  
  treated_df <- as.data.frame(treated_centers) %>%
    tibble::rownames_to_column("Cluster") %>%
    mutate(Sample = treated_name, 
           ClusterID = paste0("T", Cluster))
  
  # Combine data
  combined_centers <- bind_rows(control_df, treated_df)
  
  # Add population labels if available
  if (!is.null(control_pops) && !is.null(treated_pops)) {
    # Create mapping for control populations
    control_pop_map <- setNames(
      control_pops$Population,
      control_pops$Cluster
    )
    
    # Create mapping for treated populations
    treated_pop_map <- setNames(
      treated_pops$Population,
      treated_pops$Cluster
    )
    
    # Add population column
    combined_centers$Population <- NA
    for (i in 1:nrow(combined_centers)) {
      if (combined_centers$Sample[i] == control_name) {
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
  p <- ggplot(combined_scaled, aes(x = Marker, y = ClusterID, fill = Scaled_Expr)) +
    geom_tile() +
    scale_fill_gradient2(low = "navy", mid = "white", high = "firebrick3", midpoint = 0) +
    facet_grid(Sample ~ ., scales = "free_y", space = "free_y") +
    theme_minimal(base_size = font_size) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(),
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold"),
      strip.background = element_rect(fill = "lightgray", color = NA),
      plot.title = element_text(hjust = 0.5)
    ) +
    labs(
      title = "Marker Expression Profiles by Cluster",
      x = "Marker",
      y = "Cluster",
      fill = "Z-score"
    )
  
  return(p)
}

# Function to create a boxplot of marker expression by cluster
createMarkerExpressionPlot <- function(plot_data, marker, color_palette = "viridis", 
                                       font_size = 12, include_violin = TRUE) {
  
  # Create base plot
  p <- ggplot(plot_data, aes(x = Cluster, y = .data[[marker]], fill = Cluster)) 
  
  # Add violin layer if requested
  if (include_violin) {
    p <- p + geom_violin(alpha = 0.7)
  }
  
  # Add boxplot layer
  p <- p + 
    geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = NA) +
    labs(
      title = paste("Distribution of", marker, "by Cluster"),
      x = "Cluster",
      y = marker
    ) +
    theme_minimal(base_size = font_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
  
  # Get the number of clusters to properly handle color palette expansion
  n_clusters <- length(unique(plot_data$Cluster))
  
  # Generate appropriate palette with enough colors
  if (color_palette == "viridis") {
    # Generate exactly as many colors as needed with viridis
    palette_colors <- viridis::viridis(n_clusters)
    p <- p + scale_fill_manual(values = palette_colors)
  } else if (color_palette == "plasma") {
    # Generate exactly as many colors as needed with plasma
    palette_colors <- viridis::plasma(n_clusters)
    p <- p + scale_fill_manual(values = palette_colors)
  } else if (color_palette == "magma") {
    # Generate exactly as many colors as needed with magma
    palette_colors <- viridis::magma(n_clusters)
    p <- p + scale_fill_manual(values = palette_colors)
  } else if (color_palette == "inferno") {
    # Generate exactly as many colors as needed with inferno
    palette_colors <- viridis::inferno(n_clusters)
    p <- p + scale_fill_manual(values = palette_colors)
  } else if (color_palette == "blues") {
    # Blues only has 9 colors max, so we need to handle expansion
    max_blues <- 9
    min_colors <- 3 # RColorBrewer requires minimum 3 colors
    
    if (n_clusters < min_colors) {
      # If fewer than minimum, get minimum then take what we need
      palette_colors <- RColorBrewer::brewer.pal(min_colors, "Blues")[1:n_clusters]
    } else if (n_clusters <= max_blues) {
      palette_colors <- RColorBrewer::brewer.pal(n_clusters, "Blues")
    } else {
      # Get max colors then recycle
      palette_colors <- RColorBrewer::brewer.pal(max_blues, "Blues")
      palette_colors <- rep_len(palette_colors, n_clusters)
    }
    p <- p + scale_fill_manual(values = palette_colors)
  } else if (color_palette == "reds") {
    # Reds only has 9 colors max
    max_reds <- 9
    min_colors <- 3 # RColorBrewer requires minimum 3 colors
    
    if (n_clusters < min_colors) {
      palette_colors <- RColorBrewer::brewer.pal(min_colors, "Reds")[1:n_clusters]
    } else if (n_clusters <= max_reds) {
      palette_colors <- RColorBrewer::brewer.pal(n_clusters, "Reds")
    } else {
      palette_colors <- RColorBrewer::brewer.pal(max_reds, "Reds")
      palette_colors <- rep_len(palette_colors, n_clusters)
    }
    p <- p + scale_fill_manual(values = palette_colors)
  } else if (color_palette == "brewer_paired") {
    # Paired has 12 colors max
    max_paired <- 12
    min_colors <- 3
    
    if (n_clusters < min_colors) {
      palette_colors <- RColorBrewer::brewer.pal(min_colors, "Paired")[1:n_clusters]
    } else if (n_clusters <= max_paired) {
      palette_colors <- RColorBrewer::brewer.pal(n_clusters, "Paired")
    } else {
      palette_colors <- RColorBrewer::brewer.pal(max_paired, "Paired")
      palette_colors <- rep_len(palette_colors, n_clusters)
    }
    p <- p + scale_fill_manual(values = palette_colors)
  } else if (color_palette == "brewer_brbg") {
    # BrBG has 11 colors max
    max_brbg <- 11
    min_colors <- 3
    
    if (n_clusters < min_colors) {
      palette_colors <- RColorBrewer::brewer.pal(min_colors, "BrBG")[1:n_clusters]
    } else if (n_clusters <= max_brbg) {
      palette_colors <- RColorBrewer::brewer.pal(n_clusters, "BrBG")
    } else {
      palette_colors <- RColorBrewer::brewer.pal(max_brbg, "BrBG")
      palette_colors <- rep_len(palette_colors, n_clusters)
    }
    p <- p + scale_fill_manual(values = palette_colors)
  } else {
    # Default to viridis if unrecognized palette
    palette_colors <- viridis::viridis(n_clusters)
    p <- p + scale_fill_manual(values = palette_colors)
  }
  
  return(p)
}