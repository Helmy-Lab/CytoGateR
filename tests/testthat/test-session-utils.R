# Tests for R/utils/session_utils.R -- the Framework 2.1 session_closed guard
# logic used inside future_promise() %...>% / %...!% handlers across
# processed_data_module.R, raw_data_module.R, clustering_module.R, and
# batch_analysis_module.R.
#
# These are pure-function unit tests: no Shiny session, no future worker, no
# reactive context. That is deliberate -- the guard logic itself must be
# correct and fast to check in isolation, independent of the async plumbing
# around it.

test_that("isSessionClosedGuard returns TRUE only for TRUE", {
  expect_true(isSessionClosedGuard(TRUE))
  expect_false(isSessionClosedGuard(FALSE))
})

test_that("isSessionClosedGuard is defensive against non-logical/missing flags", {
  # A promise handler must never error out on this check -- if anything is
  # ambiguous, the safe default is "session is NOT closed" (isTRUE() semantics).
  expect_false(isSessionClosedGuard(NA))
  expect_false(isSessionClosedGuard(NULL))
  expect_false(isSessionClosedGuard(1))          # numeric, not logical TRUE
  expect_false(isSessionClosedGuard("TRUE"))     # character, not logical TRUE
  expect_false(isSessionClosedGuard(c(TRUE, TRUE))) # length > 1, isTRUE() is FALSE
})

test_that("sessionEndReason reports graceful close when isClosed() is TRUE", {
  fake_session <- list(isClosed = function() TRUE)
  expect_equal(sessionEndReason(fake_session), "graceful (browser/tab closed)")
})

test_that("sessionEndReason reports unexpected disconnect when isClosed() is FALSE", {
  fake_session <- list(isClosed = function() FALSE)
  expect_equal(sessionEndReason(fake_session), "unexpected disconnect")
})

test_that("sessionEndReason never throws, even if isClosed() errors", {
  fake_session <- list(isClosed = function() stop("boom"))
  expect_equal(sessionEndReason(fake_session), "unknown")
})

test_that("sessionEndReason never throws if the session object is malformed", {
  # e.g. isClosed() missing entirely, or session is NULL
  expect_equal(sessionEndReason(list()), "unknown")
  expect_equal(sessionEndReason(NULL), "unknown")
})

test_that("logSessionEnded logs the module id and reason", {
  expect_message(
    logSessionEnded("raw_data", "graceful (browser/tab closed)"),
    "Session ended for module: raw_data \\(reason: graceful \\(browser/tab closed\\)\\)"
  )
})

test_that("logSessionEnded adds an extra warning line when a job is still running", {
  msgs <- testthat::capture_messages(
    logSessionEnded("clustering", "unexpected disconnect", job_running = TRUE)
  )
  expect_length(msgs, 2)
  expect_match(msgs[1], "session ended with an analysis job.*still running", perl = TRUE)
  expect_match(msgs[2], "Session ended for module: clustering \\(reason: unexpected disconnect\\)")
})

test_that("logSessionEnded logs only one line when no job is running", {
  msgs <- testthat::capture_messages(
    logSessionEnded("clustering", "unexpected disconnect", job_running = FALSE)
  )
  expect_length(msgs, 1)
})

test_that("logSessionEnded returns the logged text invisibly", {
  result <- withVisible(logSessionEnded("settings", "unknown"))
  expect_false(result$visible)
  expect_match(result$value, "Session ended for module: settings")
})

test_that("logDiscardedAfterSessionEnd logs a success message without a detail suffix", {
  expect_message(
    logDiscardedAfterSessionEnd("processed_data", outcome = "success"),
    "analysis finished after session end; result discarded"
  )
  # No stray "(...)" detail should appear on a success outcome.
  msg <- testthat::capture_messages(
    logDiscardedAfterSessionEnd("processed_data", outcome = "success")
  )
  expect_false(grepl("\\(.*\\)", msg))
})

test_that("logDiscardedAfterSessionEnd logs a failure message including the error detail", {
  expect_message(
    logDiscardedAfterSessionEnd("batch_analysis", outcome = "failure", detail = "disk full"),
    "analysis failed after session end \\(disk full\\); not shown to user"
  )
})

test_that("logDiscardedAfterSessionEnd omits the parenthetical when no detail is given", {
  msg <- testthat::capture_messages(
    logDiscardedAfterSessionEnd("batch_analysis", outcome = "failure")
  )
  expect_match(msg, "analysis failed after session end; not shown to user")
  expect_false(grepl("\\(.*\\)", msg))
})

test_that("logDiscardedAfterSessionEnd rejects an invalid outcome", {
  expect_error(logDiscardedAfterSessionEnd("raw_data", outcome = "timeout"))
})

test_that("end-to-end: a closed session causes the guard to skip and log discard", {
  # Mirrors the exact sequence inside a module's future_promise() handler:
  #   1. session ends -> session_closed flips to TRUE, disconnect is logged
  #   2. the future resolves afterwards -> handler checks the guard and
  #      discards the result instead of writing to a dead reactive.
  session_closed <- FALSE
  fake_session <- list(isClosed = function() TRUE)

  end_msg <- testthat::capture_messages(
    { session_closed <- TRUE; logSessionEnded("raw_data", sessionEndReason(fake_session)) }
  )
  expect_true(session_closed)
  expect_match(end_msg, "reason: graceful \\(browser/tab closed\\)")

  wrote_reactive <- FALSE
  discard_msg <- testthat::capture_messages({
    if (isSessionClosedGuard(session_closed)) {
      logDiscardedAfterSessionEnd("raw_data", outcome = "success")
    } else {
      wrote_reactive <- TRUE
    }
  })
  expect_false(wrote_reactive)
  expect_match(discard_msg, "result discarded")
})

test_that("end-to-end: an open session lets the handler proceed normally", {
  session_closed <- FALSE

  wrote_reactive <- FALSE
  if (isSessionClosedGuard(session_closed)) {
    logDiscardedAfterSessionEnd("raw_data", outcome = "success")
  } else {
    wrote_reactive <- TRUE
  }
  expect_true(wrote_reactive)
})
