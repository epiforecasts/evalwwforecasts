library(testthat)
library(baselinenowcastpaper)

test_results <- test_check("baselinenowcastpaper")

if (any(as.data.frame(test_results)$warning > 0)) {
  stop("tests failed with warnings")
}
