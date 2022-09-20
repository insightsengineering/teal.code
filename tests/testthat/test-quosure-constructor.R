testthat::test_that("constructor returns quosure if nothing is specified", {
  q <- new_quosure()
  testthat::expect_s4_class(q, "Quosure")
  testthat::expect_identical(ls(q@env), character(0))
  testthat::expect_identical(q@code, expression())
  testthat::expect_identical(q@id, integer(0))
})

testthat::test_that("parent of quosure environment is the parent of .GlobalEnv", {
  a <- 1L
  q1 <- new_quosure(quote(a <- 1), env = environment())
  q2 <- new_quosure()
  q3 <- new_quosure(code = quote(a <- 1), env = list2env(list(a = 1)))
  testthat::expect_identical(parent.env(q1@env), parent.env(.GlobalEnv))
  testthat::expect_identical(parent.env(q1@env), parent.env(q2@env))
  testthat::expect_identical(parent.env(q1@env), parent.env(q3@env))
})

testthat::test_that("provided environment is copied so the change of the environment won't affect quosure", {
  a <- 1L
  q1 <- new_quosure(quote(a <- 1), env = environment())
  b <- 2L
  testthat::expect_identical(ls(q1@env), "a")
})

testthat::test_that("new_quosure fails if code is provided but not environment", {
  testthat::expect_error(new_quosure(quote(iris1 <- iris)))
})

testthat::test_that("new_quosure works with code being character", {
  env <- new.env()
  env$iris1 <- iris
  q <- new_quosure("iris1 <- iris", env = env)
  testthat::expect_equal(q@env, env)
  testthat::expect_identical(q@code, as.expression(quote(iris1 <- iris)))
  testthat::expect_true(checkmate::test_int(q@id))
})

testthat::test_that("new_quosure works with code being expression", {
  env <- new.env()
  env$iris1 <- iris
  q <- new_quosure(as.expression(quote(iris1 <- iris)), env = env)
  testthat::expect_equal(q@env, env)
  testthat::expect_identical(q@code, as.expression(quote(iris1 <- iris)))
  testthat::expect_true(checkmate::test_int(q@id))
})

testthat::test_that("new_quosure works with code being quoted expression", {
  env <- new.env()
  env$iris1 <- iris
  q <- new_quosure(quote(iris1 <- iris), env = env)
  testthat::expect_equal(q@env, env)
  testthat::expect_identical(q@code, as.expression(quote(iris1 <- iris)))
  testthat::expect_true(checkmate::test_int(q@id))
})

testthat::test_that("code argument is ignored in new_quosure and throws a warning", {
  env <- list(iris1 = iris)
  attr(env, "code") <- quote(iris1 <- iris)
  testthat::expect_warning(new_quosure(env = env, iris1 <- iris))
})

testthat::test_that("new_quosure works with code being length > 1", {
  env <- new.env()
  env$iris1 <- iris
  env$iris1$new <- 1L
  q <- new_quosure(
    as.expression(c(quote(iris1 <- iris), quote(iris1$new <- 1L))),
    env = env
  )
  testthat::expect_identical(
    q@code,
    as.expression(c(quote(iris1 <- iris), quote(iris1$new <- 1L)))
  )
  testthat::expect_equal(q@env, env)
})

testthat::test_that("new_quosure allows to pass irreproducible env and code", {
  testthat::expect_error(new_quosure(quote(a <- 1), env = list2env(b = 0)))
})
