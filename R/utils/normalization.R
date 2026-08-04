# R/utils/normalization.R
# ---------------------------------------------------------------------------
# Framework §2.3 — Cross-sample normalisation for batch analysis.
# gaussNorm (flowStats) is applied AFTER arcsinh transform and BEFORE scaling.
# All samples are aligned TO a user-designated reference sample by seeding
# gaussNorm's base landmarks from that reference. §6.4: errors are NOT swallowed.
# This file is auto-sourced by global.R (list.files("R/utils")).
# ---------------------------------------------------------------------------

#' Align sampled, transformed expression across samples to a reference.
#'
#' @param sampled_list Named list (names = sample ids). Each element a numeric
#'   matrix/data.frame whose columns are the selected markers (this is
#'   preprocess_results$sampled_data: arcsinh-transformed + downsampled, UNSCALED).
#' @param markers Character vector of marker/channel names (column order).
#' @param reference_id Name (id) of the reference sample; must be in sampled_list.
#' @param max_lms Max landmarks (peaks) per channel. Default 2.
#' @return list(normalized = <named list of matrices, same shape as input>,
#'              params = <reproducible record: reference, channels, landmarks, ...>)
applyGaussNorm <- function(sampled_list, markers, reference_id,
                           max_lms = 2,
                           peak_density_thr = 0.05,
                           peak_distance_thr = 0.05) {

  if (!requireNamespace("flowStats", quietly = TRUE)) {
    stop("flowStats is required for cross-sample normalisation but is not installed.")
  }
  if (is.null(reference_id) || !reference_id %in% names(sampled_list)) {
    stop("Normalisation reference sample '", reference_id,
         "' was not found among the uploaded samples.")
  }

  # Nested so it travels to the future worker inside this function's closure.
  .detect_landmarks <- function(x, max_lms, density_thr) {
    x <- x[is.finite(x)]
    if (length(x) < 2) return(if (length(x)) x[1] else 0)
    d <- stats::density(x)
    peak_idx <- which(diff(sign(diff(d$y))) == -2) + 1L      # interior local maxima
    if (length(peak_idx) == 0) return(stats::median(x))
    keep <- d$y[peak_idx] >= density_thr * max(d$y[peak_idx])
    peak_idx <- peak_idx[keep]
    ord <- order(d$y[peak_idx], decreasing = TRUE)           # tallest peaks first
    positions <- d$x[peak_idx][ord]
    if (length(positions) > max_lms) positions <- positions[seq_len(max_lms)]
    sort(positions)
  }

  sample_ids <- names(sampled_list)

  # 1. Build a flowSet: one flowFrame per sample, columns = markers.
  frames <- lapply(sample_ids, function(sid) {
    m <- as.matrix(sampled_list[[sid]])
    if (ncol(m) != length(markers)) {
      stop("Sample '", sid, "' has ", ncol(m),
           " columns but ", length(markers), " markers were selected.")
    }
    colnames(m) <- markers
    flowCore::flowFrame(m)
  })
  names(frames) <- sample_ids
  fs <- as(frames, "flowSet")   # named list -> flowSet (preserves sampleNames)

  # 2. Seed base landmarks from the REFERENCE so others align to it (R2).
  ref <- flowCore::exprs(frames[[reference_id]])
  base_lms <- lapply(markers, function(ch)
    .detect_landmarks(ref[, ch], max_lms = max_lms, density_thr = peak_density_thr))
  names(base_lms) <- markers

  # 3. Run gaussNorm. §6.4: let any error propagate (surfaces in the UI).
  #    VERIFY arg names / return element once against installed flowStats (see spec §5).
  norm <- flowStats::gaussNorm(
    flowset           = fs,
    channel.names     = markers,
    max.lms           = max_lms,
    base.lms          = base_lms,
    peak.density.thr  = peak_density_thr,
    peak.distance.thr = peak_distance_thr
  )
  fs_norm <- norm$flowset

  # 4. Extract normalised matrices, preserving names/order.
  normalized <- lapply(sample_ids, function(sid) {
    m <- flowCore::exprs(fs_norm[[sid]])
    colnames(m) <- markers
    m
  })
  names(normalized) <- sample_ids

  params <- list(
    method            = "gaussNorm (flowStats)",
    reference         = reference_id,
    channels          = markers,
    max_landmarks     = max_lms,
    base_landmarks    = base_lms,          # target peak positions per channel (R3)
    sample_confidence = norm$confidence,   # gaussNorm per-sample confidence
    flowStats_version = as.character(utils::packageVersion("flowStats")),
    timestamp         = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  list(normalized = normalized, params = params)
}
