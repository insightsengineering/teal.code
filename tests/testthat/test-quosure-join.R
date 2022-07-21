testthat::test_that("joined  - O", {
  env <- new.env()
  env$iris1 <- iris

  q1 <- new_quosure("iris1 <- iris", env = env)
  q2 <- q1

  q <- join(q1, q2)

  testthat::expect_identical(q@code, c(code = "iris1 <- iris"))
  testthat::expect_identical(q@env, env) # envs will not be identical but it's element will
})

testthat::test_that("join the same - O O Y", {
  env <- new.env()
  env$iris1 <- iris
  env$mtcars1 <- mtcars

  q1 <- new_quosure(code = c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars"), env = env)
  q2 <- q1
  q2 <- eval_code(q2, mtcars2 <- mtcars)

  testthat::expect_identical(
    q2@code, c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars", code = "mtcars2 <- mtcars")
  )

  testthat::expect_identical(
    q2@env,
    list2env(list(iris1 = iris, mtcars1 = mtcars, mtcars2 = mtcars))
  )
})

testthat::test_that("join the same - O O X Y", {
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
    c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars", code = "iris2 <- iris", code = "mtcars2 <- mtcars")
  )

  testthat::expect_identical(
    as.list(q@env),
    list(iris1 = iris, mtcars1 = mtcars, mtcars2 = mtcars)
  )
})