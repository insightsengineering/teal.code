testthat::test_that("Joining two identical quosures outputs the same object", {
  env <- new.env()
  env$iris1 <- iris

  q1 <- new_quosure("iris1 <- iris", env = env)
  q2 <- q1

  q <- join(q1, q2)

  testthat::expect_identical(q@code, c(code = "iris1 <- iris"))
  testthat::expect_equal(q@env, env)
})

testthat::test_that("Joining two independent quosures results in object having combined code and environments", {
  env1 <- new.env()
  env1$iris1 <- iris

  env2 <- new.env()
  env2$mtcars1 <- mtcars

  q1 <- new_quosure(iris1 <- iris, env = env1)
  q2 <- new_quosure(mtcars1 <- mtcars, env = env2)
  q <- join(q1, q2)

  testthat::expect_identical(q@code, c(code = "iris1 <- iris", code.1 = "mtcars1 <- mtcars"))
  testthat::expect_equal(q@env, list2env(list(iris1 = iris, mtcars1 = mtcars)))
})

testthat::test_that("Joined quosure does not duplicate common code", {
  env <- new.env()
  env$iris1 <- iris
  env$mtcars1 <- mtcars

  q1 <- new_quosure(code = c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars"), env = env)
  q2 <- q1
  q2 <- eval_code(q2, mtcars2 <- mtcars)
  q <- join(q1, q2)

  testthat::expect_identical(
    q@code,
    c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars", code = "mtcars2 <- mtcars")
  )
})

testthat::test_that("Joined quosure overwrites env in x by elements of the y's environment", {
  env1 <- new.env()
  env1$iris1 <- iris
  env1$iris2 <- iris
  env2 <- new.env()
  env2$iris1 <- head(iris)

  q1 <- new_quosure(iris1 <- iris, env = env1)
  q2 <- new_quosure(iris1 <- head(iris), env = env2)
  q <- join(q1, q2)

  testthat::expect_equal(q@env, list2env(list(iris1 = head(iris), iris2 = iris)))
})

testthat::test_that("join does not duplicate code but adds only extra code", {
  env <- new.env()
  env$iris1 <- iris
  env$mtcars1 <- mtcars

  q1 <- new_quosure(code = c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars"), env = env)
  q2 <- q1
  q1 <- eval_code(q1, "iris2 <- iris")
  q2 <- eval_code(q2, "mtcars2 <- mtcars")

  q <- join(q1, q2)

  testthat::expect_identical(
    q@code,
    c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars", code = "iris2 <- iris", code.1 = "mtcars2 <- mtcars")
  )

  testthat::expect_equal(
    as.list(q@env),
    list(iris1 = iris, iris2 = iris, mtcars1 = mtcars, mtcars2 = mtcars)
  )
})

# todo: test when shared code and both has extra code and variable overwritten
testthat::test_that("Not possible to join quosures which shares some code and modify one of the shared object", {
  env <- new.env()
  env$iris1 <- iris
  env$mtcars1 <- mtcars

  q1 <- new_quosure(code = c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars"), env = env)
  q2 <- q1
  q1 <- eval_code(q1, "iris2 <- iris")
  q2 <- eval_code(q2, "mtcars1 <- head(mtcars)")

  testthat::expect_error(join(q1, q2))
})