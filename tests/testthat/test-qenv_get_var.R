testthat::test_that("get_var and `[[`return error if object is qenv.error", {
  q <- eval_code(new_qenv(), quote(x <- 1))
  q <- eval_code(q, quote(y <- w * x))

  testthat::expect_error(get_var(q, "x"), "when evaluating qenv code")
  testthat::expect_error(q[["x"]], "when evaluating qenv code")
})


testthat::test_that("get_var and `[[` return object from qenv environment", {
  q <- eval_code(new_qenv(), quote(x <- 1))
  q <- eval_code(q, quote(y <- 5 * x))

  testthat::expect_equal(get_var(q, "y"), 5)
  testthat::expect_equal(q[["x"]], 1)
})

testthat::test_that("get_var and `[[` throw error if object not in qenv environment", {
  q <- eval_code(new_qenv(), quote(x <- 1))
  q <- eval_code(q, quote(y <- 5 * x))

  testthat::expect_error(get_var(q, "z"), "object 'z' not found")
  testthat::expect_error(q[["w"]], "object 'w' not found")
})
