testthat::test_that("get_var and `[[`return error if object is quosure.error", {
  q <- eval_code(new_quosure(), quote(x <- 1))
  q <- eval_code(q, quote(y <- w * x))

  testthat::expect_error(get_var(q, "x"), "when evaluating Quosure code")
  testthat::expect_error(q[["x"]], "when evaluating Quosure code")
})


testthat::test_that("get_var and `[[` return object from quosure environment", {
  q <- eval_code(new_quosure(), quote(x <- 1))
  q <- eval_code(q, quote(y <- 5 * x))

  testthat::expect_equal(get_var(q, "y"), 5)
  testthat::expect_equal(q[["x"]], 1)
})

testthat::test_that("get_var and `[[` throw error if object not in quosure environment", {
  q <- eval_code(new_quosure(), quote(x <- 1))
  q <- eval_code(q, quote(y <- 5 * x))

  testthat::expect_error(get_var(q, "z"), "object 'z' not found")
  testthat::expect_error(q[["w"]], "object 'w' not found")
})
