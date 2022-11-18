testthat::test_that("Concatenate two identical qenvs outputs", {
  env <- new.env()
  env$iris1 <- iris
  q1 <- new_qenv(quote(iris1 <- iris), env = env)
  q2 <- q1

  q <- concat(q1, q2)

  testthat::expect_equal(q@env, env)
  testthat::expect_identical(
    q@code,
    as.expression(list(quote(iris1 <- iris), quote(iris1 <- iris)))
  )
})

testthat::test_that("Concatenate two independent qenvs results in object having combined code and environments", {
  q1 <- new_qenv(quote(iris1 <- iris), env = list2env(list(iris1 = iris)))
  q2 <- new_qenv(quote(mtcars1 <- mtcars), env = list2env(list(mtcars1 = mtcars)))

  q <- concat(q1, q2)

  testthat::expect_equal(q@env, list2env(list(iris1 = iris, mtcars1 = mtcars)))
  testthat::expect_identical(
    q@code,
    as.expression(c(quote(iris1 <- iris), quote(mtcars1 <- mtcars)))
  )
  testthat::expect_identical(q@id, c(q1@id, q2@id))
})

testthat::test_that("Concatenate qenvs results with the same variable, the RHS has priority", {
  q1 <- new_qenv() |> eval_code(quote(a <- data.frame(1)))
  q2 <- new_qenv() |> eval_code(quote(a <- data.frame(2)))

  qenv <- concat(q1, q2)
  testthat::expect_identical(qenv[["a"]], data.frame(2))
})

testthat::test_that("Concatenate with a qenv.error object returns the qenv.error object", {
  q1 <- eval_code(new_qenv(), quote(x <- 1))
  error_q <- eval_code(new_qenv(), quote(y <- w))
  error_q2 <- eval_code(new_qenv(), quote(z <- w))

  testthat::expect_s3_class(concat(q1, error_q), "qenv.error")
  testthat::expect_s3_class(concat(error_q, error_q2), "qenv.error")
  testthat::expect_s3_class(concat(error_q, q1), "qenv.error")

  # if joining two qenv.error objects keep the first
  testthat::expect_equal(concat(error_q, error_q2), error_q)
})

testthat::test_that("Concatenate two independent qenvs with warnings results in object having combined warnings", {
  q1 <- new_qenv() %>% eval_code("warning('This is warning 1')")
  q2 <- new_qenv() %>% eval_code("warning('This is warning 2')")

  q <- concat(q1, q2)

  testthat::expect_equal(
    q@warnings,
    c(
      "This is warning 1",
      "This is warning 2"
    )
  )
})

testthat::test_that("Concatenate two independent qenvs with messages results in object having combined messages", {
  q1 <- new_qenv() %>% eval_code("message('This is message 1')")
  q2 <- new_qenv() %>% eval_code("message('This is message 2')")

  q <- concat(q1, q2)

  testthat::expect_equal(
    q@messages,
    c(
      "This is message 1\n",
      "This is message 2\n"
    )
  )
})
