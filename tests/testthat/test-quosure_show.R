testthat::test_that("showing an empty quosure states quosure is empty", {
  q <- new_quosure()
  testthat::expect_output(show(q), "A quosure object containing no objects")
})

testthat::test_that("showing a non-empty quosure lists its contents", {
  q <- new_quosure()
  q <- eval_code(q, quote(x <- 1))
  q <- eval_code(q, quote(y <- 2))
  testthat::expect_output(show(q), "A quosure object containing: x, y")
})
