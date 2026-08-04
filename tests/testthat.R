# Test runner for CytoGateR (not an R package, so this is a plain script
# rather than the usual testthat::test_check() package harness).
#
# Run from the project root with:
#   Rscript tests/testthat.R
# or from an R session with the working directory set to the project root:
#   testthat::test_dir("tests/testthat")

library(testthat)

test_dir("tests/testthat")
