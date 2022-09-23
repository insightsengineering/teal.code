testthat::test_that("showing an empty qenv states qenv is empty", {
  q <- new_qenv()
  testthat::expect_output(show(q), "A qenv object containing no objects")
})

testthat::test_that("showing a non-empty qenv lists its contents", {
  q <- new_qenv()
  q <- eval_code(q, quote(x <- 1))
  q <- eval_code(q, quote(y <- 2))
  testthat::expect_output(show(q), "A qenv object containing: x, y")
})
