# ==============================================================================
# session_utils.R -- Shiny session lifecycle helpers
#
# Implements CytoGateR v2 Framework Section 2.1 (Server Disconnections):
# every module that launches a future_promise() must (a) know why a session
# ended (graceful close vs. unexpected drop) and (b) guard its promise
# handlers so a result/error that resolves after the browser disconnected is
# discarded -- loudly, via message(), never silently.
#
# These are pure helper functions (no reactive context required) so the
# guard logic used inside future_promise() %...>% / %...!% handlers can be
# unit-tested directly, without spinning up a Shiny session or a real
# multisession worker.
# ==============================================================================

#' Determine why a Shiny session ended
#'
#' @param session A Shiny session object, or any object exposing an
#'   `isClosed()` method that returns a logical.
#' @return One of `"graceful (browser/tab closed)"`, `"unexpected disconnect"`,
#'   or `"unknown"` if `session$isClosed()` itself errors.
sessionEndReason <- function(session) {
  tryCatch({
    if (isTRUE(session$isClosed())) {
      "graceful (browser/tab closed)"
    } else {
      "unexpected disconnect"
    }
  }, error = function(e) "unknown")
}

#' Log that a module's session ended
#'
#' @param module_id Character module id (the `id` argument passed to the
#'   module's `*ModuleServer()` function).
#' @param reason Character disconnect reason, typically from
#'   `sessionEndReason()`.
#' @param job_running Logical; TRUE if an analysis future_promise() was still
#'   outstanding when the session ended.
#' @return Invisibly, the message string that was logged.
logSessionEnded <- function(module_id, reason, job_running = FALSE) {
  if (isTRUE(job_running)) {
    message("[CytoGateR] Module '", module_id, "': session ended with an analysis job ",
            "still running; its result will be discarded when it resolves.")
  }
  msg <- paste0("[CytoGateR] Session ended for module: ", module_id, " (reason: ", reason, ")")
  message(msg)
  invisible(msg)
}

#' Framework 2.1 guard: should a promise handler discard its result?
#'
#' plan(multisession) workers are a fixed pool shared by every session in the
#' app, so an in-flight future cannot be safely killed when one session ends
#' without risking other users' jobs. The safe substitute is to let the
#' future finish and then check this flag before touching any reactive
#' value or UI element tied to the now-dead session.
#'
#' @param session_closed Logical flag toggled by the module's
#'   `session$onSessionEnded()` handler.
#' @return TRUE if the promise handler should discard its result and return
#'   early; FALSE otherwise. Always returns a single logical, even if
#'   `session_closed` is NA, NULL, or non-logical.
isSessionClosedGuard <- function(session_closed) {
  isTRUE(session_closed)
}

#' Log that a future_promise() outcome was discarded after session end
#'
#' Framework Section 6.4 (No Silent Fallbacks): discarding a stale result is
#' intentional and correct, but it must always be logged so it is visible in
#' server logs -- never a silent no-op.
#'
#' @param module_id Character module id.
#' @param outcome One of `"success"` or `"failure"`.
#' @param detail Optional character detail (e.g. the error message) to
#'   include when `outcome == "failure"`.
#' @return Invisibly, the message string that was logged.
logDiscardedAfterSessionEnd <- function(module_id, outcome = c("success", "failure"), detail = NULL) {
  outcome <- match.arg(outcome)
  msg <- if (outcome == "success") {
    paste0("[CytoGateR] Module '", module_id,
           "': analysis finished after session end; result discarded.")
  } else {
    paste0("[CytoGateR] Module '", module_id, "': analysis failed after session end",
           if (!is.null(detail)) paste0(" (", detail, ")") else "",
           "; not shown to user.")
  }
  message(msg)
  invisible(msg)
}
