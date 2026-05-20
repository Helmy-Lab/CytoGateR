# Helper utility functions for Flow Cytometry Analysis

# Function to safely extract marker names from flow data
getMarkerNames <- function(data) {
  if (inherits(data, "flowFrame")) {
    # For flowFrame objects, get parameters
    params <- parameters(data)
    choices <- setNames(colnames(exprs(data)), paste0(colnames(exprs(data)), " - ", params$desc))
    return(choices)
  } else {
    # For data frames or matrices
    return(colnames(data))
  }
}

# Function to format cluster statistics for display
formatClusterStats <- function(cluster_ids, total_cells = NULL, population_data = NULL) {
  # Calculate cluster counts
  cluster_counts <- table(cluster_ids)
  
  # Set total cells if not provided
  if (is.null(total_cells)) {
    total_cells <- length(cluster_ids)
  }
  
  # Create base statistics data frame
  stats_df <- data.frame(
    Cluster = names(cluster_counts),
    Count = as.numeric(cluster_counts),
    Percentage = round(as.numeric(cluster_counts) / total_cells * 100, 2)
  )
  
  # Add population information if available
  if (!is.null(population_data)) {
    # Match cluster IDs to get population names
    stats_df$Population <- population_data$Population[match(stats_df$Cluster, population_data$Cluster)]
    stats_df$Confidence <- population_data$Confidence[match(stats_df$Cluster, population_data$Cluster)]
    
    # Reorder columns
    stats_df <- stats_df[, c("Cluster", "Population", "Count", "Percentage", "Confidence")]
  }
  
  return(stats_df)
}

# Function to generate safe, unique IDs for samples
generateSampleId <- function(name, prefix = "sample_") {
  # Remove any special characters and spaces
  safe_name <- gsub("[^a-zA-Z0-9]", "_", name)
  
  # Add timestamp for uniqueness
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  
  # Create ID
  id <- paste0(prefix, safe_name, "_", timestamp)
  
  return(id)
}

# Safely evaluate a user-supplied gating expression without arbitrary code execution.
# Walks the parsed AST and permits only: column names, numeric/logical literals,
# comparison operators (>, <, >=, <=, ==, !=), logical operators (&, |, !, &&, ||),
# and unary +/- on numeric literals.  Everything else is rejected.
safeEvalGateExpr <- function(expr_str, df) {
  expr_str <- trimws(expr_str)
  if (!nzchar(expr_str) || nchar(expr_str) > 500) return(NULL)

  parsed <- tryCatch(parse(text = expr_str), error = function(e) NULL)
  if (is.null(parsed) || length(parsed) == 0) return(NULL)

  allowed_cols <- colnames(df)
  safe_binary  <- c(">", "<", ">=", "<=", "==", "!=", "&", "|", "&&", "||")
  safe_unary   <- c("!", "-", "+")

  is_safe <- function(node) {
    if (is.numeric(node) || is.logical(node)) return(TRUE)
    if (is.name(node)) return(as.character(node) %in% allowed_cols)
    if (is.call(node)) {
      fn   <- as.character(node[[1]])
      args <- as.list(node)[-1]
      if (fn %in% safe_binary) return(all(vapply(args, is_safe, logical(1))))
      if (fn %in% safe_unary && length(args) == 1) return(is_safe(args[[1]]))
      return(FALSE)
    }
    FALSE
  }

  if (!is_safe(parsed[[1]])) return(NULL)

  tryCatch({
    env    <- list2env(as.list(df), parent = emptyenv())
    result <- eval(parsed[[1]], envir = env)
    if (is.logical(result) && length(result) == nrow(df)) result else NULL
  }, error = function(e) NULL)
}

# Function to safely read Excel or CSV files
safeReadFile <- function(file_path, file_name = NULL) {
  if (is.null(file_name)) {
    file_name <- basename(file_path)
  }

  # Reject path traversal: strip to bare filename then validate characters
  file_name <- basename(file_name)
  if (!grepl("^[a-zA-Z0-9._\\- ]+$", file_name)) {
    warning("Invalid filename detected")
    return(NULL)
  }

  ext <- tolower(tools::file_ext(file_name))

  tryCatch({
    switch(ext,
           "xlsx" = read.xlsx(file_path),
           "csv"  = read.csv(file_path, stringsAsFactors = FALSE),
           "tsv"  = read.delim(file_path, stringsAsFactors = FALSE),
           "txt"  = read.delim(file_path, stringsAsFactors = FALSE),
           stop("Unsupported file format")
    )
  }, error = function(e) {
    warning(paste("Error reading file:", e$message))
    return(NULL)
  })
}

# Function to create sample/control groups from file patterns
autoGroupSamples <- function(file_names, control_pattern = "control|ctrl", 
                             treated_pattern = "treated|sample|test") {
  groups <- sapply(file_names, function(name) {
    if (grepl(control_pattern, name, ignore.case = TRUE)) {
      return("Control")
    } else if (grepl(treated_pattern, name, ignore.case = TRUE)) {
      return("Treated")
    } else {
      return("Unknown")
    }
  })
  
  return(groups)
}

# Function to validate marker selection (ensure enough markers are selected)
validateMarkerSelection <- function(markers, min_markers = 2) {
  if (length(markers) < min_markers) {
    return(list(
      valid = FALSE,
      message = paste("Please select at least", min_markers, "markers for analysis")
    ))
  }
  
  return(list(valid = TRUE))
}

# Function to format preprocessing metrics for display
formatPreprocessingMetrics <- function(preprocess_results) {
  metrics <- preprocess_results$metrics
  
  # Format QC metrics
  qc_metrics <- if (!is.null(metrics$qc)) {
    list(
      initial_count = metrics$qc$initial_count,
      after_qc = metrics$qc$final_count,
      qc_removed_pct = round(metrics$qc$removed_pct * 100, 1)
    )
  } else {
    list(
      initial_count = nrow(preprocess_results$raw_data),
      after_qc = nrow(preprocess_results$raw_data),
      qc_removed_pct = 0
    )
  }
  
  # Format gating metrics
  gating_metrics <- if (!is.null(metrics$gating)) {
    list(
      after_gating = metrics$gating$final_count,
      gating_removed_pct = round(metrics$gating$removed_pct * 100, 1)
    )
  } else {
    list(
      after_gating = qc_metrics$after_qc,
      gating_removed_pct = 0
    )
  }
  
  # Format final metrics
  final_metrics <- list(
    initial_count = qc_metrics$initial_count,
    after_qc = qc_metrics$after_qc,
    qc_removed_pct = qc_metrics$qc_removed_pct,
    after_gating = gating_metrics$after_gating,
    gating_removed_pct = gating_metrics$gating_removed_pct,
    final_analyzed = nrow(preprocess_results$sampled_data)
  )
  
  return(final_metrics)
}