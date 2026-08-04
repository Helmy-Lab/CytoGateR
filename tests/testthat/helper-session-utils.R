# Sources the module under test directly, since CytoGateR is a Shiny app
# rather than an installed package (no DESCRIPTION/NAMESPACE to rely on).
# Tries a path relative to the project root first (the working directory
# when tests are run via `testthat::test_dir("tests/testthat")` from the
# project root), then falls back to a path relative to this helper file's
# location (when run via `Rscript tests/testthat.R` from elsewhere).
.source_relative_to_root <- function(rel_path) {
  candidates <- c(rel_path, file.path("..", "..", rel_path))
  for (p in candidates) {
    if (file.exists(p)) {
      source(p)
      return(invisible(TRUE))
    }
  }
  stop("Could not locate '", rel_path, "' to source for tests. ",
       "Run tests with the project root as the working directory.")
}

.source_relative_to_root("R/utils/session_utils.R")
