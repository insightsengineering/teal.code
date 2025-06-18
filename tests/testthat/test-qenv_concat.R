testthat::test_that("Concatenate two identical qenvs outputs", {
  q <- qenv()
  q1 <- eval_code(q, quote(iris1 <- iris))
  q2 <- q1

  q12 <- concat(q1, q2)

  testthat::expect_equal(q12@.xData, q1@.xData)
  testthat::expect_identical(get_code(q12), "iris1 <- iris\niris1 <- iris")
})

testthat::test_that("Concatenate two independent qenvs results in object having combined code and environments", {
  q1 <- qenv()
  q2 <- qenv()

  q1 <- eval_code(q1, quote(iris1 <- iris))
  q2 <- eval_code(q2, quote(mtcars1 <- mtcars))

  q12 <- concat(q1, q2)

  testthat::expect_equal(q12@.xData, list2env(list(iris1 = iris, mtcars1 = mtcars)))
  testthat::expect_identical(get_code(q12), "iris1 <- iris\nmtcars1 <- mtcars")
  testthat::expect_identical(names(q12@code), c(names(q1@code), names(q2@code)))
})

testthat::test_that("Concatenate qenvs results with the same variable, the RHS has priority", {
  q1 <- eval_code(qenv(), quote(a <- data.frame(1)))
  q2 <- eval_code(qenv(), quote(a <- data.frame(2)))

  q12 <- concat(q1, q2)
  testthat::expect_identical(q12[["a"]], data.frame(2))
})

testthat::test_that("Concatenate with a qenv.error object returns the qenv.error object", {
  q1 <- eval_code(qenv(), quote(x <- 1))
  error_q <- eval_code(qenv(), quote(y <- w))
  error_q2 <- eval_code(qenv(), quote(z <- w))

  testthat::expect_s3_class(concat(q1, error_q), "qenv.error")
  testthat::expect_s3_class(concat(error_q, error_q2), "qenv.error")
  testthat::expect_s3_class(concat(error_q, q1), "qenv.error")

  # if joining two qenv.error objects keep the first
  testthat::expect_equal(concat(error_q, error_q2), error_q)
})

testthat::test_that("Concatenate two independent qenvs with warnings results in object having combined warnings", {
  q1 <- eval_code(qenv(), "warning('This is warning 1')")
  q2 <- eval_code(qenv(), "warning('This is warning 2')")

  q12 <- concat(q1, q2)

  testthat::expect_identical(
    get_warnings(q12),
    paste(
      "~~~ Warnings ~~~",
      "\n> This is warning 1",
      "when running code:",
      "warning('This is warning 1')",
      "\n> This is warning 2",
      "when running code:",
      "warning('This is warning 2')",
      "\n~~~ Trace ~~~\n",
      "warning('This is warning 1')",
      "warning('This is warning 2')",
      sep = "\n"
    )
  )
})

testthat::test_that("Concatenate two independent qenvs with messages results in object having combined messages", {
  q1 <- eval_code(qenv(), "message('This is message 1')")
  q2 <- eval_code(qenv(), "message('This is message 2')")

  q12 <- concat(q1, q2)

  testthat::expect_identical(
    get_messages(q12),
    paste(
      "~~~ Messages ~~~",
      "\n> This is message 1",
      "when running code:",
      "message('This is message 1')",
      "\n> This is message 2",
      "when running code:",
      "message('This is message 2')",
      "\n~~~ Trace ~~~\n",
      "message('This is message 1')",
      "message('This is message 2')",
      sep = "\n"
    )
  )
})
