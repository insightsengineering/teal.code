testthat::test_that("constructor returns quosure if nothing is specified", {
  q <- new_quosure()
  testthat::expect_s4_class(q, "Quosure")
  testthat::expect_identical(ls(q@env), character(0))
  testthat::expect_identical(q@code, new_qcode())
})

testthat::test_that("new_quosure fails if code is provided but not environment", {
  testthat::expect_error(new_quosure("iris1 <- iris"))
})

testthat::test_that("new_quosure works with code being character", {
  env <- new.env()
  env$iris1 <- iris
  q <- new_quosure("iris1 <- iris", env = env)
  testthat::expect_identical(get_code(q), c(`initial code` = "iris1 <- iris"))
  testthat::expect_identical(q@env, env)
})

testthat::test_that("new_quosure works with code being expression", {
  env <- new.env()
  env$iris1 <- iris
  q <- new_quosure(parse(text = "iris1 <- iris"), env = env)
  testthat::expect_identical(get_code(q), c(`initial code` = "iris1 <- iris"))
  testthat::expect_identical(q@env, env)
})

testthat::test_that("new_quosure works with code a quoted expression", {
  env <- new.env()
  env$iris1 <- iris
  q <- new_quosure(quote("iris1 <- iris"), env = env)
  testthat::expect_identical(get_code(q), c(`initial code` = "iris1 <- iris"))
  testthat::expect_identical(q@env, env)
})

testthat::test_that("new_quosure works with code being unquoted expression", {
  env <- new.env()
  env$iris1 <- iris
  q <- new_quosure(iris1 <- iris, env = env)
  testthat::expect_identical(get_code(q), c(`initial code` = "iris1 <- iris"))
  testthat::expect_identical(q@env, env)
})

testthat::test_that("new_quosure works with env being a list", {
  list <- list(iris1 = iris)
  attr(list, "code") <- quote(iris1 <- iris)
  q <- new_quosure(env = list)
  testthat::expect_identical(get_code(q), c(`initial code` = "iris1 <- iris"))
  testthat::expect_equal(q@env, list2env(list))
})
testthat::test_that("can't initialize quosure without code attr when env is a list", {
  env <- list(iris1 = iris)
  testthat::expect_error(new_quosure(env = env))
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
  q <- new_quosure(c("iris1 <- iris", "iris1$new <- 1L"), env = env)
  testthat::expect_identical(get_code(q), c(`initial code` = "iris1 <- iris", `initial code.1` = "iris1$new <- 1L"))
  testthat::expect_identical(q@env, env)
})
