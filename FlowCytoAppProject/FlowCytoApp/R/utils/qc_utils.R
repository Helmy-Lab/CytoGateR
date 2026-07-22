# ==============================================================================
# qc_utils.R — Quality Control utilities
#
# Implements CytoGateR v2 Framework §2.2 (Incomplete Preprocessing / QA Workflow)
# and enforces §6.4 (No Silent Fallbacks).
#
# Design contract:
#   performQC() NEVER returns unfiltered data disguised as QC-passed data.
#   Every failure mode either (a) returns status = "ERROR" with data = NULL, or
#   (b) throws, so the caller must surface it in the UI. There is no path where
#   QC quietly does nothing and the user believes it ran.
# ==============================================================================


# ------------------------------------------------------------------------------
# QC status constants
# ------------------------------------------------------------------------------
QC_STATUS <- list(
  PASS  = "Pass",   # QC ran, removal within normal range
  WARN  = "Warn",   # QC ran, removal elevated but below the hard threshold
  SKIP  = "Skip",   # step not applicable / not requested
  OK    = "OK",     # final row: enough events remain to analyse
  ERROR = "Error"   # step failed or removal exceeded threshold - analysis blocked
)

# Rendered as an HTML entity so this source file stays pure ASCII. Non-ASCII
# literals in R sources are a known portability hazard on Windows locales.
EM_DASH <- "&mdash;"


#' Run time-based quality control on a single flowFrame
#'
#' Wraps flowAI::flow_auto_qc. Unlike the previous implementation, this function
#' does not revert to unfiltered data on failure. See §6.4.
#'
#' @param flow_data  A flowFrame (FCS). Non-flowFrame input returns status SKIP.
#' @param qc_settings List of overrides. Supported:
#'   min_cells      Minimum events that must remain post-QC (default 100)
#'   max_anomalies  Max fraction of events flowAI may remove (default 0.10)
#'   warn_anomalies Fraction above which status becomes WARN (default max/2)
#'   alpha          flowAI significance level for the flow-rate check (0.01)
#'   sample_name    Label used in messages and report filenames
#'   report_dir     Directory for the flowAI HTML anomaly report. NULL = tempdir.
#'   write_report   Whether to generate the HTML report (default TRUE)
#'
#' @return list(data, status, error, metrics)
#'   On ERROR, data is NULL and error holds a user-facing message.
performQC <- function(flow_data, qc_settings = list()) {

  default_settings <- list(
    min_cells      = 100,
    max_anomalies  = 0.10,
    warn_anomalies = NULL,
    alpha          = 0.01,
    sample_name    = "sample",
    report_dir     = NULL,
    write_report   = TRUE
  )
  settings <- modifyList(default_settings, qc_settings[!vapply(qc_settings, is.null, logical(1))])

  # Derive the warn threshold if not supplied: halfway to the hard limit.
  if (is.null(settings$warn_anomalies)) {
    settings$warn_anomalies <- settings$max_anomalies / 2
  }

  sample_name <- settings$sample_name

  # ---- Non-FCS input: QC genuinely does not apply. Report SKIP, not PASS. ----
  if (!inherits(flow_data, "flowFrame")) {
    n <- nrow(flow_data)
    return(list(
      data   = flow_data,
      status = QC_STATUS$SKIP,
      error  = NULL,
      metrics = list(
        sample_name   = sample_name,
        initial_count = n,
        final_count   = n,
        removed_count = 0,
        removed_pct   = 0,
        report_path   = NULL,
        message       = "Time-based QC applies to FCS files only; step skipped."
      )
    ))
  }

  initial_count <- tryCatch(nrow(flow_data), error = function(e) NULL)

  # Malformed / unreadable FCS: fail loudly rather than crashing the session.
  # Framework 9.1 requires zero crashes on malformed FCS upload.
  if (is.null(initial_count) || length(initial_count) != 1 || is.na(initial_count)) {
    return(list(
      data = NULL, status = QC_STATUS$ERROR,
      error = sprintf(
        "Could not read an event count from sample '%s'. The file may be malformed or truncated.",
        sample_name
      ),
      metrics = list(
        sample_name = sample_name, initial_count = NA_integer_, final_count = 0,
        removed_count = 0, removed_pct = 0, report_path = NULL
      )
    ))
  }

  # Guard: a file that is already too small cannot be meaningfully QC'd.
  if (initial_count < settings$min_cells) {
    return(list(
      data   = NULL,
      status = QC_STATUS$ERROR,
      error  = sprintf(
        "Sample '%s' contains only %s events, below the minimum of %s required for analysis.",
        sample_name, format(initial_count, big.mark = ",", scientific = FALSE), format(settings$min_cells, big.mark = ",", scientific = FALSE)
      ),
      metrics = list(
        sample_name = sample_name, initial_count = initial_count,
        final_count = 0, removed_count = 0, removed_pct = 0, report_path = NULL
      )
    ))
  }

  # ---- Report destination -----------------------------------------------------
  report_dir <- settings$report_dir
  if (is.null(report_dir)) {
    report_dir <- file.path(tempdir(), "cytogater_qc")
  }
  dir.create(report_dir, showWarnings = FALSE, recursive = TRUE)

  safe_name <- gsub("[^A-Za-z0-9_.-]", "_", sample_name)

  # ---- Run flowAI -------------------------------------------------------------
  # NOTE: the argument is alphaFR, not alpha. The previous code passed `alpha=`,
  # which only worked via R partial argument matching — fragile and easy to break.
  qc_out <- tryCatch({
    flowAI::flow_auto_qc(
      flow_data,
      alphaFR        = settings$alpha,
      html_report    = if (isTRUE(settings$write_report)) paste0("_", safe_name, "_QC") else FALSE,
      mini_report    = FALSE,
      fcs_QC         = FALSE,
      fcs_highQ      = FALSE,
      fcs_lowQ       = FALSE,
      folder_results = report_dir,
      output         = 1
    )
  }, error = function(e) {
    structure(list(message = conditionMessage(e)), class = "cytogater_qc_failure")
  })

  # flowAI failed outright — surface it. Do NOT fall back to unfiltered data.
  if (inherits(qc_out, "cytogater_qc_failure")) {
    return(list(
      data   = NULL,
      status = QC_STATUS$ERROR,
      error  = sprintf("Time-based QC failed for sample '%s': %s", sample_name, qc_out$message),
      metrics = list(
        sample_name = sample_name, initial_count = initial_count,
        final_count = 0, removed_count = 0, removed_pct = 0, report_path = NULL
      )
    ))
  }

  # flowAI can return a list depending on `output`; normalise to a flowFrame.
  if (is.list(qc_out) && !inherits(qc_out, "flowFrame")) {
    qc_out <- qc_out[[1]]
  }

  if (!inherits(qc_out, "flowFrame")) {
    return(list(
      data   = NULL,
      status = QC_STATUS$ERROR,
      error  = sprintf(
        "Time-based QC returned an unexpected result type for sample '%s'. Expected a flowFrame, got %s.",
        sample_name, class(qc_out)[1]
      ),
      metrics = list(
        sample_name = sample_name, initial_count = initial_count,
        final_count = 0, removed_count = 0, removed_pct = 0, report_path = NULL
      )
    ))
  }

  final_count   <- nrow(qc_out)
  removed_count <- initial_count - final_count
  removed_pct   <- if (initial_count > 0) removed_count / initial_count else 0

  report_path <- locateQCReport(report_dir, safe_name)

  base_metrics <- list(
    sample_name   = sample_name,
    initial_count = initial_count,
    final_count   = final_count,
    removed_count = removed_count,
    removed_pct   = removed_pct,
    report_path   = report_path
  )

  # ---- Failure condition 1: too many anomalies -------------------------------
  # Previously this silently reverted to unfiltered data with a console warning.
  # Framework §2.2 requires a visible error instead.
  if (removed_pct > settings$max_anomalies) {
    return(list(
      data   = NULL,
      status = QC_STATUS$ERROR,
      error  = sprintf(
        paste0("Time-based QC flagged %.1f%% of events in sample '%s' as anomalous, ",
               "exceeding the %.1f%% threshold. This sample was NOT analysed. ",
               "Review the flowAI anomaly report, then either exclude this sample or ",
               "raise the threshold deliberately."),
        removed_pct * 100, sample_name, settings$max_anomalies * 100
      ),
      metrics = base_metrics
    ))
  }

  # ---- Failure condition 2: too few events survive ---------------------------
  if (final_count < settings$min_cells) {
    return(list(
      data   = NULL,
      status = QC_STATUS$ERROR,
      error  = sprintf(
        paste0("Only %s events remained in sample '%s' after time-based QC, ",
               "below the minimum of %s. This sample was NOT analysed."),
        format(final_count, big.mark = ",", scientific = FALSE), sample_name,
        format(settings$min_cells, big.mark = ",", scientific = FALSE)
      ),
      metrics = base_metrics
    ))
  }

  # ---- Success (Pass or Warn) -------------------------------------------------
  status <- if (removed_pct > settings$warn_anomalies) QC_STATUS$WARN else QC_STATUS$PASS

  list(
    data    = qc_out,
    status  = status,
    error   = NULL,
    metrics = base_metrics
  )
}


#' Locate the HTML anomaly report flowAI wrote for a sample
#'
#' flowAI's report filename depends on the input identifier, so match on the
#' sample token rather than assuming an exact name.
#' @return Absolute path, or NULL if no report was produced.
locateQCReport <- function(report_dir, safe_name) {
  if (is.null(report_dir) || !dir.exists(report_dir)) return(NULL)

  candidates <- list.files(report_dir, pattern = "\\.html$", full.names = TRUE)
  if (length(candidates) == 0) return(NULL)

  hit <- candidates[grepl(safe_name, basename(candidates), fixed = TRUE)]
  chosen <- if (length(hit) > 0) hit else candidates

  # Most recently written wins when several match.
  chosen[order(file.mtime(chosen), decreasing = TRUE)][1]
}


#' Build the per-sample QC summary table required by Framework §2.2
#'
#' Produces the four-row structure specified in the framework. Rows for steps
#' that have not run yet are shown as "Skip" rather than being hidden, so the
#' user can always see what did and did not execute.
#'
#' @param qc_metrics   metrics list from performQC(); may be NULL
#' @param qc_status    status string from performQC(); may be NULL
#' @param livedead     Optional list(removed = <int>, applied = <logical>)
#' @param sample_name  Sample label
#' @return data.frame with columns Metric, Events, Percent, Status
buildQCSummaryTable <- function(qc_metrics = NULL,
                                qc_status  = NULL,
                                livedead   = NULL,
                                sample_name = "sample") {

  loaded <- if (!is.null(qc_metrics$initial_count)) qc_metrics$initial_count else NA_integer_

  pct <- function(n) {
    if (is.null(n) || length(n) != 1 || is.na(n)) return(EM_DASH)
    if (is.null(loaded) || is.na(loaded) || loaded == 0) return(EM_DASH)
    sprintf("%.1f%%", 100 * n / loaded)
  }
  fmt <- function(n) {
    if (is.null(n) || length(n) != 1 || is.na(n)) return(EM_DASH)
    format(n, big.mark = ",", scientific = FALSE, trim = TRUE)
  }

  # Row 1 — events loaded
  rows <- list(data.frame(
    Metric  = "Events loaded from FCS",
    Events  = fmt(loaded),
    Percent = if (is.na(loaded)) EM_DASH else "100%",
    Status  = EM_DASH,
    stringsAsFactors = FALSE
  ))

  # Row 2 — time-based QC
  qc_removed <- if (!is.null(qc_metrics$removed_count)) qc_metrics$removed_count else NA_integer_
  rows[[2]] <- data.frame(
    Metric  = "Removed: time-based QC (flowAI)",
    Events  = fmt(qc_removed),
    Percent = pct(qc_removed),
    Status  = if (is.null(qc_status)) QC_STATUS$SKIP else qc_status,
    stringsAsFactors = FALSE
  )

  # Row 3 — live/dead gate
  ld_applied <- isTRUE(livedead$applied)
  ld_removed <- if (ld_applied && !is.null(livedead$removed)) livedead$removed else NA_integer_
  rows[[3]] <- data.frame(
    Metric  = "Removed: live/dead gate",
    Events  = fmt(ld_removed),
    Percent = pct(ld_removed),
    Status  = if (ld_applied) QC_STATUS$PASS else QC_STATUS$SKIP,
    stringsAsFactors = FALSE
  )

  # Row 4 — final N
  final_n <- if (!is.null(qc_metrics$final_count)) qc_metrics$final_count else NA_integer_
  if (!is.na(final_n) && ld_applied && !is.na(ld_removed)) {
    final_n <- final_n - ld_removed
  }
  final_status <- if (identical(qc_status, QC_STATUS$ERROR)) {
    QC_STATUS$ERROR
  } else if (is.null(final_n) || is.na(final_n)) {
    # Nothing has run yet - do not imply the sample is cleared for analysis.
    QC_STATUS$SKIP
  } else if (final_n <= 0) {
    QC_STATUS$ERROR
  } else {
    QC_STATUS$OK
  }
  rows[[4]] <- data.frame(
    Metric  = "Final events for analysis",
    Events  = fmt(final_n),
    Percent = pct(final_n),
    Status  = final_status,
    stringsAsFactors = FALSE
  )

  out <- do.call(rbind, rows)
  attr(out, "sample_name") <- sample_name
  out
}


#' Colour-code a QC status string for display in the summary table
qcStatusBadge <- function(status) {
  colour <- switch(status,
    "Pass"  = "#28a745",
    "OK"    = "#28a745",
    "Warn"  = "#fd7e14",
    "Error" = "#dc3545",
    "Skip"  = "#6c757d",
    "#6c757d"
  )
  sprintf("<span style='color:%s;font-weight:600;'>%s</span>", colour, status)
}
