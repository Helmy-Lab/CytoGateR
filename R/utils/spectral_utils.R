# R/utils/spectral_utils.R
# ---------------------------------------------------------------------------
# Framework §2.4 — Spectral flow detection.
# A file is flagged spectral when it has many channels (> threshold) AND carries
# NO conventional spillover keyword ($SPILL / $SPILLOVER). Spectral data must be
# unmixed upstream (instrument software / CATALYST); standard spillover
# compensation must NOT be applied to it. §6.4: detection is surfaced in the UI,
# never a silent skip. Auto-sourced by global.R (list.files("R/utils")).
# ---------------------------------------------------------------------------

# Keyword names that, if present, indicate a conventional spillover matrix.
# Compared case-insensitively. Kept faithful to §2.4 ($SPILL / $SPILLOVER) plus
# the common non-'$' variants so a compensated file is never mis-flagged.
.SPILLOVER_KEYWORDS <- c("$SPILLOVER", "$SPILL", "SPILLOVER", "SPILL")

#' Does an already-loaded flowFrame look spectral?
#'
#' @param ff a flowFrame (a metadata-only read is sufficient; events not needed).
#' @param channel_threshold channel count above which, absent a spillover
#'   keyword, the file is treated as spectral. Default 30 (per §2.4 [S1]).
#' @return list(is_spectral, n_channels, has_spillover)
isSpectralFlowFrame <- function(ff, channel_threshold = 30) {
  stopifnot(methods::is(ff, "flowFrame"))
  n_channels    <- ncol(ff)
  kw_names      <- names(flowCore::keyword(ff))
  has_spillover <- any(toupper(.SPILLOVER_KEYWORDS) %in% toupper(kw_names))
  list(
    is_spectral   = (n_channels > channel_threshold) && !has_spillover,
    n_channels    = n_channels,
    has_spillover = has_spillover
  )
}

#' Metadata-only spectral check straight from a file path (cheap: reads 1 event).
#'
#' @param datapath path to an FCS file.
#' @param channel_threshold see isSpectralFlowFrame().
#' @return isSpectralFlowFrame() list plus $error (NULL on success).
detectSpectralFile <- function(datapath, channel_threshold = 30) {
  tryCatch({
    meta <- flowCore::read.FCS(datapath, which.lines = 1, transformation = FALSE)
    res  <- isSpectralFlowFrame(meta, channel_threshold)
    res$error <- NULL
    res
  }, error = function(e) {
    list(is_spectral = FALSE, n_channels = NA_integer_,
         has_spillover = NA, error = conditionMessage(e))
  })
}

# The exact user-facing warning required by §2.4 [S2]. Single literal — do not
# reflow; the text must match the framework verbatim.
SPECTRAL_WARNING_TEXT <- "This appears to be a spectral flow file. Spectral unmixing must be performed upstream (e.g. in instrument software or CATALYST) before import. Standard spillover compensation will not be applied."
