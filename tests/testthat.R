pkg_name <- "teal.code"
library(pkg_name, character.only = TRUE)
if (requireNamespace("testthat", quietly = TRUE)) {
  testthat::test_check(pkg_name)
}
