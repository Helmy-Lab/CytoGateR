# clustering_utils.R
# Utility functions for clustering and cell type identification in flow cytometry data

# ---- Required Libraries ----
library(flowCore)
library(FlowSOM)
library(dbscan)
library(Rphenograph)
library(igraph)

# ---- K-means Clustering ----
runKmeansClustering <- function(marker_data, num_clusters = 8) {
  set.seed(123)
  tryCatch({
    km <- kmeans(marker_data, centers = num_clusters, nstart = 10)
    cluster_ids <- km$cluster
    centers <- km$centers
    colnames(centers) <- colnames(marker_data)
    list(cluster_ids = cluster_ids, centers = centers, method = "K-means")
  }, error = function(e) {
    warning("K-means clustering failed: ", e$message)
    NULL
  })
}

# ---- DBSCAN Clustering ----
runDbscanClustering <- function(marker_data, eps = 0.5, minPts = 5) {
  tryCatch({
    scaled_markers <- scale(marker_data)
    dbscan_result <- dbscan::dbscan(scaled_markers, eps = eps, minPts = minPts)
    cluster_ids <- dbscan_result$cluster
    if (any(cluster_ids == 0)) {
      max_cluster <- max(cluster_ids)
      cluster_ids[cluster_ids == 0] <- max_cluster + 1
    }
    unique_clusters <- sort(unique(cluster_ids))
    centers <- t(sapply(unique_clusters, function(cl) {
      colMeans(marker_data[cluster_ids == cl, , drop = FALSE])
    }))
    colnames(centers) <- colnames(marker_data)
    rownames(centers) <- paste("Cluster", unique_clusters)
    list(cluster_ids = cluster_ids, centers = centers, method = "DBSCAN")
  }, error = function(e) {
    warning("DBSCAN clustering failed: ", e$message)
    NULL
  })
}

# ---- FlowSOM Clustering ----
runFlowSomClustering <- function(marker_data, xdim = 6, ydim = 6, n_metaclusters = 12, rlen = 10) {
  tryCatch({
    fcs_data <- flowFrame(as.matrix(marker_data))
    params <- flowCore::parameters(fcs_data)
    params$name <- colnames(marker_data)
    params$desc <- colnames(marker_data)
    flowCore::parameters(fcs_data) <- params
    fsom_input <- FlowSOM::ReadInput(fcs_data, transform = FALSE, scale = FALSE)
    fsom <- FlowSOM::BuildSOM(fsom_input, xdim = xdim, ydim = ydim, rlen = rlen, silent = TRUE)
    metacl <- FlowSOM::MetaClustering(fsom$map$codes, method = "metaClustering_consensus", max = n_metaclusters)
    cell_som_clusters <- fsom$map$mapping[,1]
    cell_metaclusters <- metacl[cell_som_clusters]
    unique_clusters <- sort(unique(cell_metaclusters))
    centers <- t(sapply(unique_clusters, function(cl) {
      colMeans(marker_data[cell_metaclusters == cl, , drop = FALSE])
    }))
    colnames(centers) <- colnames(marker_data)
    rownames(centers) <- paste("Cluster", unique_clusters)
    list(cluster_ids = cell_metaclusters, centers = centers,
         som_object = fsom, som_clusters = cell_som_clusters,
         metaclusters = metacl, method = "FlowSOM")
  }, error = function(e) {
    warning("FlowSOM clustering failed: ", e$message)
    NULL
  })
}

# ---- Phenograph Clustering ----
runPhenographClustering <- function(marker_data, k = 30) {
  tryCatch({
    pheno_result <- Rphenograph(as.matrix(marker_data), k = k)
    cluster_ids <- as.numeric(membership(pheno_result[[2]]))
    unique_clusters <- sort(unique(cluster_ids))
    centers <- t(sapply(unique_clusters, function(cl) {
      colMeans(marker_data[cluster_ids == cl, , drop = FALSE])
    }))
    colnames(centers) <- colnames(marker_data)
    rownames(centers) <- paste("Cluster", unique_clusters)
    list(cluster_ids = cluster_ids, centers = centers, pheno_object = pheno_result, method = "Phenograph")
  }, error = function(e) {
    warning("Phenograph clustering failed: ", e$message)
    NULL
  })
}

# ---- Unified Dispatcher ----
runClustering <- function(marker_data, method, params = list()) {
  switch(method,
         "K-means" = runKmeansClustering(marker_data, params$num_clusters %||% 8),
         "DBSCAN"  = runDbscanClustering(marker_data, params$eps %||% 0.5, params$minPts %||% 5),
         "FlowSOM" = runFlowSomClustering(marker_data,
                                          xdim = params$xdim %||% 6,
                                          ydim = params$ydim %||% 6,
                                          n_metaclusters = params$n_metaclusters %||% 12,
                                          rlen = params$rlen %||% 10),
         "Phenograph" = runPhenographClustering(marker_data, params$k %||% 30),
         stop("Unsupported clustering method"))
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# ---- Cell Type Identification ----
identify_cell_populations <- function(cluster_centers, marker_names,
                                      high_threshold = 0.5,
                                      low_threshold = -0.4,
                                      min_confidence = 0.3) {
  results <- data.frame(
    Cluster = 1:nrow(cluster_centers),
    Population = rep("Unknown", nrow(cluster_centers)),
    Confidence = rep(0, nrow(cluster_centers)),
    MatchDetails = rep("", nrow(cluster_centers)),
    stringsAsFactors = FALSE
  )
  
  cell_population_templates <- list(
    "Total T cells" = list(high = c("CD3"), medium = c(), low = c("MHCII")),
    "CD4+ T cells" = list(high = c("CD3", "CD4"), medium = c(), low = c()),
    "CD8+ T cells" = list(high = c("CD3"), medium = c(), low = c("CD4")),
    "T helper 1 (TH1)" = list(high = c("CD3", "CD4", "CXCR3"), medium = c(), low = c("FOXP3")),
    "T regulatory (Treg)" = list(high = c("CD3", "CD4", "FOXP3"), medium = c(), low = c("CXCR3")),
    "T follicular helper (TFH)" = list(high = c("CD3", "CD4", "CXCR5", "BCL6"), medium = c(), low = c()),
    "B cells" = list(high = c("MHCII"), medium = c(), low = c("CD3"))
  )
  
  manual_mapping <- list(
    "FL5-A - CD3 FITC FITC-A" = "CD3", "FL13-A - CD4 APCCy7 APC-A750-A" = "CD4",
    "FL11-A - MHC II AF647 APC-A" = "MHCII", "FL12-A - Foxp3 A700 APC-A700-A" = "FOXP3",
    "FL1-A - CXCR5 BV421 PB450-A" = "CXCR5", "FL6-A - CXCR3 PE PE-A" = "CXCR3",
    "FL7-A - BCl6 PE PE-A" = "BCL6", "FL10-A - Ki67 PECy7 PC7-A" = "KI67",
    "FL3-A - Live Dead BV570 Violet610-A" = "LIVEDEAD",
    "FL5-A" = "CD3", "FL13-A" = "CD4", "FL11-A" = "MHCII", "FL12-A" = "FOXP3",
    "FL1-A" = "CXCR5", "FL6-A" = "CXCR3", "FL7-A" = "BCL6", "FL10-A" = "KI67", "FL3-A" = "LIVEDEAD",
    "CD3" = "CD3", "CD4" = "CD4", "MHC" = "MHCII", "FOXP3" = "FOXP3", "CXCR5" = "CXCR5",
    "CXCR3" = "CXCR3", "BCL6" = "BCL6", "KI67" = "KI67", "DEAD" = "LIVEDEAD"
  )
  
  clean_marker_name <- function(name) {
    name <- toupper(name)
    name <- gsub("FL[0-9]+-[A-Z]+ *- *", "", name)
    name <- gsub(" +[A-Z0-9]+(-[A-Z0-9]+)*$", "", name)
    name <- gsub("[-_\\s]+", "", name)
    name
  }
  
  normalized_markers <- sapply(marker_names, clean_marker_name)
  mapped_markers <- sapply(seq_along(marker_names), function(i) {
    raw <- marker_names[i]
    norm <- normalized_markers[i]
    manual_mapping[[raw]] %||% manual_mapping[[norm]] %||% norm
  })
  
  z_scores <- apply(cluster_centers, 2, function(x) {
    if (sd(x, na.rm=TRUE) == 0) rep(0, length(x)) else (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
  })
  colnames(z_scores) <- colnames(cluster_centers)
  
  for (i in 1:nrow(cluster_centers)) {
    best_match <- "Unknown"
    best_score <- 0
    best_details <- ""
    for (pop_name in names(cell_population_templates)) {
      template <- cell_population_templates[[pop_name]]
      score <- 0
      total_weight <- 0
      match_details <- c()
      
      for (marker in template$high) {
        idx <- which(mapped_markers == marker)
        if (length(idx)) {
          z <- z_scores[i, idx[1]]
          total_weight <- total_weight + 1
          if (!is.na(z) && z > high_threshold) {
            score <- score + 1
            match_details <- c(match_details, paste0(marker_names[idx[1]], "(+)"))
          }
        }
      }
      for (marker in template$medium) {
        idx <- which(mapped_markers == marker)
        if (length(idx)) {
          z <- z_scores[i, idx[1]]
          total_weight <- total_weight + 0.7
          if (!is.na(z) && z > -0.2 && z < 0.7) {
            score <- score + 0.7
            match_details <- c(match_details, paste0(marker_names[idx[1]], "(~)"))
          }
        }
      }
      for (marker in template$low) {
        idx <- which(mapped_markers == marker)
        if (length(idx)) {
          z <- z_scores[i, idx[1]]
          total_weight <- total_weight + 0.5
          if (!is.na(z) && z < low_threshold) {
            score <- score + 0.5
            match_details <- c(match_details, paste0(marker_names[idx[1]], "(-)"))
          }
        }
      }
      
      if (total_weight > 0) {
        confidence <- score / total_weight
        if (confidence > best_score && confidence > min_confidence) {
          best_score <- confidence
          best_match <- pop_name
          best_details <- paste(match_details, collapse = ", ")
        }
      }
    }
    
    results$Population[i] <- best_match
    results$Confidence[i] <- round(best_score * 100, 1)
    results$MatchDetails[i] <- best_details
  }
  
  return(results)
}

