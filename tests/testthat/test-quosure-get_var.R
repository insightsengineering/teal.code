testthat::test_that("get_var and `[[`return quosure.error if given quosure error", {
  q <- eval_code(new_quosure(), "x <- 1")
  q <- eval_code(q, "y <- w * x")

  testthat::expect_s3_class(get_var(q, "x"), "quosure.error")
  testthat::expect_s3_class(q[["x"]], "quosure.error")
})


testthat::test_that("get_var and `[[` return object from quosure environment", {
  q <- eval_code(new_quosure(), "x <- 1")
  q <- eval_code(q, "y <- 5 * x")

  testthat::expect_equal(get_var(q, "y"), 5)
  testthat::expect_equal(q[["x"]], 1)
})

testthat::test_that("get_var and `[[` throw error if object not in quosure environment", {
  q <- eval_code(new_quosure(), "x <- 1")
  q <- eval_code(q, "y <- 5 * x")

  testthat::expect_error(get_var(q, "z"), "object 'z' not found")
  testthat::expect_error(q[["w"]], "object 'w' not found")
})
