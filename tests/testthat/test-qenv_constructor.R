testthat::test_that("constructor returns qenv if nothing is specified", {
  q <- new_qenv()
  testthat::expect_s4_class(q, "qenv")
  testthat::expect_identical(ls(q@env), character(0))
  testthat::expect_identical(q@code, character())
  testthat::expect_identical(q@id, integer(0))
  testthat::expect_identical(q@warnings, character(0))
  testthat::expect_identical(q@messages, character(0))
})

testthat::test_that("parent of qenv environment is the parent of .GlobalEnv", {
  a <- 1L
  q1 <- new_qenv(quote(a <- 1), env = environment())
  q2 <- new_qenv()
  q3 <- new_qenv(code = quote(a <- 1), env = list2env(list(a = 1)))
  testthat::expect_identical(parent.env(q1@env), parent.env(.GlobalEnv))
  testthat::expect_identical(parent.env(q1@env), parent.env(q2@env))
  testthat::expect_identical(parent.env(q1@env), parent.env(q3@env))
})

testthat::test_that("provided environment is copied so the change of the environment won't affect qenv", {
  a <- 1L
  q1 <- new_qenv(quote(a <- 1), env = environment())
  b <- 2L
  testthat::expect_identical(ls(q1@env), "a")
})

testthat::test_that("new_qenv fails if code is provided but not environment", {
  testthat::expect_error(new_qenv(quote(iris1 <- iris)))
})

testthat::test_that("new_qenv works with code being character", {
  env <- new.env()
  env$iris1 <- iris
  q <- new_qenv("iris1 <- iris", env = env)
  testthat::expect_equal(q@env, env)
  testthat::expect_identical(q@code, "iris1 <- iris")
  testthat::expect_true(checkmate::test_int(q@id))
})

testthat::test_that("new_qenv works with code being expression", {
  env <- new.env()
  env$iris1 <- iris
  q <- new_qenv(as.expression(quote(iris1 <- iris)), env = env)
  testthat::expect_equal(q@env, env)
  testthat::expect_identical(q@code, "iris1 <- iris")
  testthat::expect_true(checkmate::test_int(q@id))
})

testthat::test_that("new_qenv works with code being quoted expression", {
  env <- new.env()
  env$iris1 <- iris
  q <- new_qenv(quote(iris1 <- iris), env = env)
  testthat::expect_equal(q@env, env)
  testthat::expect_identical(q@code, "iris1 <- iris")
  testthat::expect_true(checkmate::test_int(q@id))
})

testthat::test_that("new_qenv works with code being length > 1", {
  env <- new.env()
  env$iris1 <- iris
  env$iris1$new <- 1L
  q <- new_qenv(
    as.expression(c(quote(iris1 <- iris), quote(iris1$new <- 1L))),
    env = env
  )
  testthat::expect_identical(
    q@code,
    "iris1 <- iris\niris1$new <- 1"
  )
  testthat::expect_equal(q@env, env)
})

testthat::test_that("new_qenv allows to pass irreproducible env and code", {
  testthat::expect_error(new_qenv(quote(a <- 1), env = list2env(b = 0)))
})

testthat::test_that("Initializing qenv with code only creates corresponding warnings and messages slots", {
  q1 <- new_qenv(bquote(a <- 1), env = list2env(list(a = 1)))
  testthat::expect_identical(q1@warnings, "")
  testthat::expect_identical(q1@messages, "")
})
