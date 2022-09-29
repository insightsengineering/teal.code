testthat::test_that("eval_code evaluates the code in the qenvs environment", {
  q1 <- new_qenv(quote(iris1 <- iris), env = list2env(list(iris1 = iris)))
  q2 <- eval_code(q1, quote(b <- nrow(iris1)))
  testthat::expect_identical(get_var(q2, "b"), 150L)
})

testthat::test_that("eval_code doesn't have access to environment where it's called", {
  a <- 1L
  q1 <- new_qenv(quote(a <- 1), env = environment())
  b <- 2L
  testthat::expect_s3_class(
    eval_code(q1, quote(d <- b)),
    c("qenv.error", "try-error", "error", "condition")
  )
})

testthat::test_that("@env in qenv is always a sibling of .GlobalEnv", {
  q1 <- new_qenv()
  testthat::expect_identical(parent.env(q1@env), parent.env(.GlobalEnv))

  q2 <- eval_code(q1, quote(a <- 1L))
  testthat::expect_identical(parent.env(q2@env), parent.env(.GlobalEnv))
  q3 <- eval_code(q2, quote(b <- 2L))
  testthat::expect_identical(parent.env(q3@env), parent.env(.GlobalEnv))
})

testthat::test_that("library have to be called separately before using function from package", {
  q1 <- eval_code(new_qenv(), quote(library(checkmate)))
  # library call adds package env in the search path just over .GlobalEnv
  #   it means that q3@env before the call was a parent of .GlobalEnv but not after the call

  q2 <- eval_code(q1, quote(assert_number(1)))
  testthat::expect_identical(parent.env(q2@env), parent.env(.GlobalEnv))

  detach("package:checkmate", unload = TRUE)
  testthat::expect_s3_class(
    eval_code(
      new_qenv(),
      as.expression(c(
        quote(library(checkmate)),
        quote(assert_number(1))
      ))
    ),
    "qenv.error"
  )
})

testthat::test_that("eval_code works with character", {
  q1 <- eval_code(new_qenv(), "a <- 1")

  testthat::expect_identical(q1@code, as.expression(quote(a <- 1)))
  testthat::expect_equal(q1@env, list2env(list(a = 1)))
})

testthat::test_that("eval_code works with expression", {
  q1 <- eval_code(new_qenv(), as.expression(quote(a <- 1)))

  testthat::expect_identical(q1@code, as.expression(quote(a <- 1)))
  testthat::expect_equal(q1@env, list2env(list(a = 1)))
})

testthat::test_that("eval_code works with quoted", {
  q1 <- eval_code(new_qenv(), quote(a <- 1))

  testthat::expect_identical(q1@code, as.expression(quote(a <- 1)))
  testthat::expect_equal(q1@env, list2env(list(a = 1)))
})

testthat::test_that("eval_code works with quoted code block", {
  q1 <- eval_code(
    new_qenv(),
    quote({
      a <- 1
      b <- 2
    })
  )

  testthat::expect_equal(
    q1@code,
    as.expression(
      quote({
        a <- 1
        b <- 2
      })
    )
  )
  testthat::expect_equal(q1@env, list2env(list(a = 1, b = 2)))
})

testthat::test_that("eval_code fails with unquoted expression", {
  testthat::expect_error(eval_code(new_qenv(), a <- b), "object 'b' not found")
})

testthat::test_that("an error when calling eval_code returns a qenv.error object which has message and trace", {
  q <- eval_code(new_qenv(), quote(x <- 1))
  q <- eval_code(q, quote(y <- 2))
  q <- eval_code(q, quote(z <- w * x))
  testthat::expect_s3_class(q, "qenv.error")
  testthat::expect_equal(
    unname(q$trace),
    as.expression(
      c(
        quote(x <- 1),
        quote(y <- 2),
        quote(z <- w * x)
      )
    )
  )
  testthat::expect_equal(q$message, "object 'w' not found \n when evaluating qenv code:\n z <- w * x")
})

testthat::test_that("a warning when calling eval_code returns a qenv object which has warnings", {
  q <- eval_code(new_qenv(), quote("iris_data <- iris"))
  q <- eval_code(q, quote("p <- hist(iris_data[, 'Sepal.Length'], ff = '')"))
  testthat::expect_s4_class(q, "qenv")
  testthat::expect_equal(
    q@warnings,
    c(
      "",
      paste0(
        "\"ff\" is not a graphical parameter",
        "\"ff\" is not a graphical parameter",
        "\"ff\" is not a graphical parameter",
        "\"ff\" is not a graphical parameter"
      )
    )
  )
})

testthat::test_that("a message when calling eval_code returns a qenv object which has messages", {
  q <- eval_code(new_qenv(), quote("iris_data <- head(iris)"))
  q <- eval_code(q, quote("message('This is a message')"))
  testthat::expect_s4_class(q, "qenv")
  testthat::expect_equal(
    q@messages,
    c(
      "",
      "This is a message\n"
    )
  )
})

testthat::test_that("eval_code returns a qenv object with empty messages and warnings when none are returned", {
  q <- eval_code(new_qenv(), quote("iris_data <- head(iris)"))
  testthat::expect_s4_class(q, "qenv")
  testthat::expect_equal(q@messages, "")
  testthat::expect_equal(q@warnings, "")
})
