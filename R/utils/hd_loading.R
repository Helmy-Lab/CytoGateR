# R/utils/hd_loading.R
# ---------------------------------------------------------------------------
# Framework §2.4 — High-dimensional panel support: metadata-first inspection
# and memory-bounded (strided) event loading for large FCS files.
# Auto-sourced by global.R (list.files("R/utils")).
# ---------------------------------------------------------------------------

#' Read ONLY FCS metadata (parameters + keywords), not the full event matrix.
#' which.lines = 1 parses the full parameter block and all keywords while
#' reading just one event -- enough to present the channel list before a full
#' load (§2.4 [H1]).
#' @return a flowFrame with 1 event; use parameters(), colnames(), keyword().
readFCSmetadata <- function(datapath) {
  flowCore::read.FCS(datapath, which.lines = 1, transformation = FALSE)
}

#' Total events recorded in an FCS file, from the $TOT keyword (no full read).
#' Keywords are parsed from the header, so $TOT is the TRUE total even when the
#' frame was read with which.lines = 1.
#' @param x a datapath (character) or an already-read flowFrame.
fcsEventCount <- function(x) {
  meta <- if (methods::is(x, "flowFrame")) x else readFCSmetadata(x)
  tot  <- suppressWarnings(as.numeric(flowCore::keyword(meta, "$TOT")[[1]]))
  if (length(tot) == 0 || is.na(tot)) nrow(meta) else as.integer(tot)
}

#' Load an FCS file with a hard cap on events held in memory.
#' If the file has more than `max_events`, events are read on a regular stride
#' via read.FCS(which.lines = seq(1, n, by = step)) so ~max_events evenly spaced
#' events are loaded (§2.4 [H2]); otherwise the file is read in full.
#' @return list(ff, n_total, n_loaded, subsampled, step)
readFCSchunked <- function(datapath, max_events = 500000, transformation = FALSE) {
  n_total <- fcsEventCount(datapath)
  if (is.na(n_total) || n_total <= max_events) {
    ff <- flowCore::read.FCS(datapath, transformation = transformation)
    return(list(ff = ff, n_total = n_total, n_loaded = nrow(ff),
                subsampled = FALSE, step = 1L))
  }
  step  <- as.integer(ceiling(n_total / max_events))
  lines <- seq(1L, n_total, by = step)
  ff <- flowCore::read.FCS(datapath, which.lines = lines,
                           transformation = transformation)
  list(ff = ff, n_total = n_total, n_loaded = nrow(ff),
       subsampled = TRUE, step = step)
}
