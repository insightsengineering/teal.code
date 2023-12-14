testthat::test_that("get_var and `[[`return error if object is qenv.error", {
  q <- eval_code(qenv(), quote(x <- 1))
  q <- eval_code(q, quote(y <- w * x))

  testthat::expect_error(get_var(q, "x"), "when evaluating qenv code")
  testthat::expect_error(q[["x"]], "when evaluating qenv code")
})

testthat::test_that("get_var and `[[` return object from qenv environment", {
  q <- eval_code(qenv(), quote(x <- 1))
  q <- eval_code(q, quote(y <- 5 * x))

  testthat::expect_equal(get_var(q, "y"), 5)
  testthat::expect_equal(q[["x"]], 1)
})

testthat::test_that("get_var and `[[` return NULL if object not in qenv environment", {
  q <- eval_code(qenv(), quote(x <- 1))
  q <- eval_code(q, quote(y <- 5 * x))

  testthat::expect_null(get_var(q, "z"))
  testthat::expect_message(get_var(q, "z"), "object 'z' not found")

  testthat::expect_null(q[["w"]])
  testthat::expect_message(q[["w"]], "object 'w' not found")
})

testthat::test_that("get_var and `[[` only returns objects from qenv, not parent environment(s)", {
  q <- qenv()

  testthat::expect_null(get_var(q, "iris"))
  testthat::expect_null(q[["iris"]])
})
