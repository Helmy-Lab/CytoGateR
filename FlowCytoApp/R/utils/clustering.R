# Clustering utility functions for Flow Cytometry Analysis

# Function to run K-means clustering
runKmeansClustering <- function(marker_data, num_clusters = 8) {
  set.seed(123)  # For reproducibility
  
  tryCatch({
    # Run K-means
    km <- kmeans(marker_data, centers = num_clusters, nstart = 10)
    cluster_ids <- km$cluster
    
    # Store cluster centers for intensity profiles
    centers <- km$centers
    colnames(centers) <- colnames(marker_data)
    
    # Create results object
    results <- list(
      cluster_ids = cluster_ids,
      centers = centers,
      method = "K-means"
    )
    
    return(results)
  }, error = function(e) {
    warning("K-means clustering failed: ", e$message)
    return(NULL)
  })
}

# Function to run DBSCAN clustering
runDbscanClustering <- function(marker_data, eps = 0.5, minPts = 5) {
  tryCatch({
    # Scale data for DBSCAN
    scaled_markers <- scale(marker_data)
    
    # Run DBSCAN
    dbscan_result <- dbscan::dbscan(scaled_markers, eps = eps, minPts = minPts)
    cluster_ids <- dbscan_result$cluster
    
    # Handle noise points (cluster 0)
    if (any(cluster_ids == 0)) {
      max_cluster <- max(cluster_ids)
      cluster_ids[cluster_ids == 0] <- max_cluster + 1
    }
    
    # Calculate cluster centers (mean of each cluster)
    unique_clusters <- sort(unique(cluster_ids))
    centers <- matrix(NA, nrow = length(unique_clusters), ncol = ncol(marker_data))
    
    for (i in seq_along(unique_clusters)) {
      cluster_idx <- which(cluster_ids == unique_clusters[i])
      if (length(cluster_idx) > 0) {
        centers[i,] <- colMeans(marker_data[cluster_idx, , drop = FALSE])
      }
    }
    
    colnames(centers) <- colnames(marker_data)
    rownames(centers) <- paste("Cluster", unique_clusters)
    
    # Create results object
    results <- list(
      cluster_ids = cluster_ids,
      centers = centers,
      method = "DBSCAN"
    )
    
    return(results)
  }, error = function(e) {
    warning("DBSCAN clustering failed: ", e$message)
    return(NULL)
  })
}

# Function to run FlowSOM clustering
runFlowSomClustering <- function(marker_data, xdim = 6, ydim = 6, n_metaclusters = 12, rlen = 10) {
  tryCatch({
    # Create a flowFrame from the matrix with proper marker names
    marker_matrix <- as.matrix(marker_data)
    colnames(marker_matrix) <- colnames(marker_data)
    fcs_data <- flowCore::flowFrame(marker_matrix)
    
    # Set proper column names in the flowFrame parameters
    params <- flowCore::parameters(fcs_data)
    params$name <- colnames(marker_data)
    params$desc <- colnames(marker_data)
    flowCore::parameters(fcs_data) <- params
    
    # Use ReadInput to properly prepare the data for FlowSOM
    fsom_input <- FlowSOM::ReadInput(fcs_data, transform = FALSE, scale = FALSE)
    
    # Build the SOM with validated parameters
    fsom <- FlowSOM::BuildSOM(
      fsom_input, 
      colsToUse = NULL,  # Use all columns
      xdim = xdim,       # Validated x dimension
      ydim = ydim,       # Validated y dimension
      rlen = rlen,       # Training iterations
      silent = FALSE     # Show progress
    )
    
    # Get metaclusters
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
    
    colnames(centers) <- colnames(marker_data)
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
    
    return(results)
  }, error = function(e) {
    warning("FlowSOM clustering failed: ", e$message)
    return(NULL)
  })
}

# Function to run Phenograph clustering
runPhenographClustering <- function(marker_data, k = 30) {
  tryCatch({
    # Run Phenograph
    pheno_result <- Rphenograph(as.matrix(marker_data), k = k)
    
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
    
    colnames(centers) <- colnames(marker_data)
    rownames(centers) <- paste("Cluster", unique_clusters)
    
    # Create results object
    results <- list(
      cluster_ids = cluster_ids,
      centers = centers,
      pheno_object = pheno_result,
      method = "Phenograph"
    )
    
    return(results)
  }, error = function(e) {
    warning("Phenograph clustering failed: ", e$message)
    return(NULL)
  })
}

# Unified function to run clustering based on method
runClustering <- function(marker_data, method, params = list()) {
  if (method == "K-means") {
    num_clusters <- if (!is.null(params$num_clusters)) params$num_clusters else 8
    return(runKmeansClustering(marker_data, num_clusters))
  } 
  else if (method == "DBSCAN") {
    eps <- if (!is.null(params$eps)) params$eps else 0.5
    minPts <- if (!is.null(params$minPts)) params$minPts else 5
    return(runDbscanClustering(marker_data, eps, minPts))
  }
  else if (method == "FlowSOM") {
    xdim <- if (!is.null(params$xdim)) params$xdim else 6
    ydim <- if (!is.null(params$ydim)) params$ydim else 6
    n_metaclusters <- if (!is.null(params$n_metaclusters)) params$n_metaclusters else 12
    rlen <- if (!is.null(params$rlen)) params$rlen else 10
    return(runFlowSomClustering(marker_data, xdim, ydim, n_metaclusters, rlen))
  }
  else if (method == "Phenograph") {
    k <- if (!is.null(params$k)) params$k else 30
    return(runPhenographClustering(marker_data, k))
  }
  else {
    stop("Unsupported clustering method")
  }
}

# Function to identify cell populations based on marker expression patterns
identify_cell_populations <- function(cluster_centers, marker_names, 
                                      high_threshold = 0.5,  
                                      low_threshold = -0.4,  
                                      min_confidence = 0.3) { 
  
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
    # "CD4+ T cells" = list(
    #   high = c("CD3", "CD4"),
    #   medium = c(),
    #   low = c("FOXP3", "BCL6")
    # ),
    # "Regulatory T cells" = list(
    #   high = c("CD3", "CD4", "FOXP3"),
    #   medium = c(),
    #   low = c("BCL6", "CXCR5")
    # ),
    # "T follicular helper cells" = list(
    #   high = c("CD3", "CD4", "CXCR5", "BCL6"),
    #   medium = c(),
    #   low = c("FOXP3")
    # ),
    # "MHCII+ APCs" = list(
    #   high = c("MHCII"),
    #   medium = c(),
    #   low = c("CD3", "CD4")
    # ),
    # "Proliferating T cells" = list(
    #   high = c("CD3", "KI67"),
    #   medium = c(),
    #   low = c()
    # ),
    # "Proliferating Tregs" = list(
    #   high = c("CD3", "CD4", "FOXP3", "KI67"),
    #   medium = c(),
    #   low = c("BCL6", "CXCR5")
    # ),
    # "Proliferating Tfh cells" = list(
    #   high = c("CD3", "CD4", "CXCR5", "BCL6", "KI67"),
    #   medium = c(),
    #   low = c("FOXP3")
    # ),
    # "Activated T cells" = list(
    #   high = c("CD3", "MHCII"),
    #   medium = c(),
    #   low = c()
    # ),
    "Total T cells" = list(
      high = c("CD3"),
      medium = c(),
      low = c("MHCII")
    ),
    "CD4+ T cells" = list(
      high = c("CD3", "CD4"),
      medium = c(),
      low = c()
    ),
    "CD8+ T cells" = list(
      high = c("CD3"),
      medium = c(),
      low = c("CD4")
    ),
    "T helper 1 (TH1)" = list(
      high = c("CD3", "CD4", "CXCR3"),
      medium = c(),
      low = c("FOXP3")
    ),
    "T regulatory (Treg)" = list(
      high = c("CD3", "CD4", "FOXP3"),
      medium = c(),
      low = c("CXCR3")
    ),
    "T follicular helper (TFH)" = list(
      high = c("CD3", "CD4", "CXCR5", "BCL6"),
      medium = c(),
      low = c()
    ),
    "B cells" = list(
      high = c("MHCII"),
      medium = c(),
      low = c("CD3")
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
    
    # Special cases
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
  }
  
  return(results)
}