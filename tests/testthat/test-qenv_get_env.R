test_that("get_env returns environment for qenv objects", {
  q <- within(qenv(), {
    a <- 5
    b <- data.frame(x = 1:10)
  })
  testthat::expect_is(get_env(q), "environment")
  testthat::expect_equal(ls(get_env(q)), c("a", "b"))
})

test_that("get_env returns the same object for qenv.error objects", {
  q_error <- eval_code(qenv(), quote(a <- b))
  testthat::expect_equal(get_env(q_error), q_error)
})
