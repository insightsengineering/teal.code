testthat::test_that("eval_code evaluates the code in the quosures environment", {
  q1 <- new_quosure("iris1 <- iris", env = list2env(list(iris1 = iris)))
  q2 <- eval_code(q1, "b <- nrow(iris1)")
  testthat::expect_identical(get_var(q2, "b"), 150L)
})

testthat::test_that("eval_code doesn't have access to environment where it's called", {
  a <- 1L
  q1 <- new_quosure("a <- 1", env = environment())
  b <- 2L
  testthat::expect_s3_class(eval_code(q1, "d <- b"), c("quosure.error", "try-error", "error", "condition"))
})

testthat::test_that("@env in quosure is always a sibling of .GlobalEnv", {
  q1 <- new_quosure()
  testthat::expect_identical(parent.env(q1@env), parent.env(.GlobalEnv))

  q2 <- eval_code(q1, "a <- 1L")
  testthat::expect_identical(parent.env(q2@env), parent.env(.GlobalEnv))
  q3 <- eval_code(q2, "b <- 2L")
  testthat::expect_identical(parent.env(q3@env), parent.env(.GlobalEnv))
})

testthat::test_that("library have to be called separately before using function from package", {
  q1 <- eval_code(new_quosure(), "library(checkmate)")
  # library call adds package env in the search path just over .GlobalEnv
  #   it means that q3@env before the call was a parent of .GlobalEnv but not after the call

  q2 <- eval_code(q1, "assert_number(1)")
  testthat::expect_identical(parent.env(q2@env), parent.env(.GlobalEnv))

  detach("package:checkmate", unload = TRUE)
  testthat::expect_s3_class(
    eval_code(
      new_quosure(),
      as.expression(c(
        quote(library(checkmate)),
        quote(assert_number(1))
      ))
    ),
    "quosure.error"
  )
})

testthat::test_that("eval_code allows to pass comments", {
  q1 <- eval_code(new_quosure(), "a <- 1")
  q2 <- eval_code(q1, "# comment")
  q3 <- eval_code(q2, "b <- 3")

  testthat::expect_identical(q3@code, c(code = "a <- 1", code.1 = "# comment", code.2 = "b <- 3"))
  testthat::expect_equal(q3@env, list2env(list(a = 1, b = 3)))
})

testthat::test_that("eval_code works with expression", {
  q1 <- eval_code(new_quosure(), as.expression(quote(a <- 1)))

  testthat::expect_identical(q1@code, c(code = "a <- 1"))
  testthat::expect_equal(q1@env, list2env(list(a = 1)))
})

testthat::test_that("eval_code works with quoted", {
  q1 <- eval_code(new_quosure(), quote(a <- 1))

  testthat::expect_identical(q1@code, c(code = "a <- 1"))
  testthat::expect_equal(q1@env, list2env(list(a = 1)))
})

testthat::test_that("eval_code works with quoted code block", {
  q1 <- eval_code(
    new_quosure(),
    quote({
      a <- 1
      b <- 2
    })
  )

  testthat::expect_identical(q1@code, c(code = "{\n    a <- 1\n    b <- 2\n}"))
  testthat::expect_equal(q1@env, list2env(list(a = 1, b = 2)))
})

testthat::test_that("eval_code fails with unquoted expression", {
  testthat::expect_error(eval_code(new_quosure(), a <- b), "object 'b' not found")
})

testthat::test_that("each eval_code adds name to passed code", {
  q1 <- new_quosure()
  q2 <- eval_code(q1, code = "a <- 1", name = "test")
  q3 <- eval_code(q2, code = "b <- 2", name = "test2")
  testthat::expect_identical(q3@code, c(test = "a <- 1", test2 = "b <- 2"))
})

testthat::test_that("get_code make name of the code block unique if duplicated", {
  q1 <- new_quosure()
  q2 <- eval_code(q1, code = "a <- 1", name = "test")
  q3 <- eval_code(q2, code = "b <- 2", name = "test")
  testthat::expect_identical(q3@code, c(test = "a <- 1", test.1 = "b <- 2"))
})
