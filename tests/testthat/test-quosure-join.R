testthat::test_that("Joining two identical quosures outputs the same object", {
  env <- new.env()
  env$iris1 <- iris
  q1 <- new_quosure("iris1 <- iris", env = env)
  q2 <- q1

  testthat::expect_true(check_joinable(q1, q2))
  q <- join(q1, q2)

  testthat::expect_equal(q@env, env)
  testthat::expect_identical(q@code, c(`initial code` = "iris1 <- iris"))
  testthat::expect_identical(q@id, q1@id)
})

testthat::test_that("Joining two independent quosures results in object having combined code and environments", {
  q1 <- new_quosure("iris1 <- iris", env = list2env(list(iris1 = iris)))
  q2 <- new_quosure("mtcars1 <- mtcars", env = list2env(list(mtcars1 = mtcars)))

  testthat::expect_true(check_joinable(q1, q2))
  q <- join(q1, q2)

  testthat::expect_equal(q@env, list2env(list(iris1 = iris, mtcars1 = mtcars)))
  testthat::expect_identical(
    q@code,
    c(`initial code` = "iris1 <- iris", `initial code.1` = "mtcars1 <- mtcars")
  )
  testthat::expect_identical(q@id, c(q1@id, q2@id))
})

testthat::test_that("Joined quosure does not duplicate common code", {
  env <- list2env(list(
    iris1 = iris,
    mtcars1 = mtcars
  ))

  q1 <- new_quosure(code = c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars"), env = env)
  q2 <- q1
  q2 <- eval_code(q2, "mtcars2 <- mtcars")

  testthat::expect_true(check_joinable(q1, q2))
  q <- join(q1, q2)

  testthat::expect_identical(
    q@code,
    c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars", code = "mtcars2 <- mtcars")
  )
  testthat::expect_identical(q@id, c(q1@id, q2@id[3]))
})

testthat::test_that("Not able to join two Quosures if any of the shared objects changed", {
  env1 <- list2env(list(
    iris1 = iris,
    iris2 = iris
  ))
  env2 <- list2env(list(iris1 = head(iris)))

  q1 <- new_quosure("iris1 <- iris", env = env1)
  q2 <- new_quosure("iris1 <- head(iris)", env = env2)

  testthat::expect_match(check_joinable(q1, q2), "Not possible to join Quosure objects")
  testthat::expect_error(join(q1, q2), "Not possible to join Quosure objects")
})

testthat::test_that("join does not duplicate code but adds only extra code", {
  env <- list2env(list(
    iris1 = iris,
    mtcars1 = mtcars
  ))

  q1 <- new_quosure(code = c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars"), env = env)
  q2 <- q1
  q1 <- eval_code(q1, "iris2 <- iris")
  q2 <- eval_code(q2, "mtcars2 <- mtcars")

  testthat::expect_true(check_joinable(q1, q2))
  q <- join(q1, q2)

  testthat::expect_identical(
    q@code,
    c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars", code = "iris2 <- iris", code.1 = "mtcars2 <- mtcars")
  )

  testthat::expect_equal(
    as.list(q@env),
    list(iris1 = iris, iris2 = iris, mtcars1 = mtcars, mtcars2 = mtcars)
  )

  testthat::expect_identical(q@id, c(q1@id, q2@id[3]))
})

testthat::test_that("Not possible to join quosures which share some code when one of the shared object was modified", {
  env <- new.env()
  env$iris1 <- iris
  env$mtcars1 <- mtcars

  q1 <- new_quosure(code = c(iris1 = "iris1 <- iris", mtcars1 = "mtcars1 <- mtcars"), env = env)
  q2 <- q1
  q1 <- eval_code(q1, "iris2 <- iris")
  q2 <- eval_code(q2, "mtcars1 <- head(mtcars)")

  testthat::expect_error(join(q1, q2))
})

testthat::test_that("Quosure objects are mergeable if they don't share any code (identified by id)", {
  q1 <- new_quosure(code = "a1 <- 1", env = list2env(list(a1 = 1)))
  q2 <- new_quosure(code = "a1 <- 1", env = list2env(list(a1 = 1)))
  testthat::expect_true(check_joinable(q1, q2))

  cq <- join(q1, q2)
  testthat::expect_s4_class(cq, "Quosure")
  testthat::expect_equal(cq@env, list2env(list(a1 = 1)))
  testthat::expect_identical(cq@code, c(`initial code` = "a1 <- 1", `initial code.1` = "a1 <- 1"))
  testthat::expect_identical(cq@id, c(q1@id, q2@id))
})

testthat::test_that("Quosure objects are mergeable if they share common initial quosure elements", {
  q1 <- new_quosure(code = "a1 <- 1", env = list2env(list(a1 = 1)))
  q2 <- eval_code(q1, "b1 <- 2")
  q1 <- eval_code(q1, "a2 <- 3")
  testthat::expect_true(check_joinable(q1, q2))

  cq <- join(q1, q2)
  testthat::expect_s4_class(cq, "Quosure")
  testthat::expect_equal(cq@env, list2env(list(a1 = 1, b1 = 2, a2 = 3)))
  testthat::expect_identical(
    cq@code,
    c(`initial code` = "a1 <- 1", `code` = "a2 <- 3", `code.1` = "b1 <- 2")
  )
  testthat::expect_identical(cq@id, union(q1@id, q2@id))
})

testthat::test_that(
  "Quosure objects aren't mergeable if they share common quosure elements proceeded with some other code",
  {
    q1 <- new_quosure(code = "a1 <- 1", env = list2env(list(a1 = 1)))
    q2 <- new_quosure(code = "b1 <- 2", env = list2env(list(b1 = 2)))
    q_common <- new_quosure("c1 <- 3", env = list2env(list(c1 = 3)))
    q1 <- join(q1, q_common)
    q2 <- join(q2, q_common)
    testthat::expect_match(check_joinable(q1, q2), "start from index = 1")
    testthat::expect_error(join(q1, q2), "start from index = 1")
  }
)

testthat::test_that("Quosure objects are not mergable if they have multiple common streaks", {
  q_common1 <- new_quosure(code = c("c1 <- 1"), env = list2env(list(c1 = 1)))
  q_common2 <- new_quosure(code = c("c2 <- 2"), env = list2env(list(c2 = 2)))

  q1 <- eval_code(q_common1, "a1 <- 3")
  q1 <- join(q1, q_common2) # c1, a1, c2
  q2 <- join(q_common1, q_common2) # c1, c2

  testthat::expect_match(check_joinable(q1, q2), "doesn't have the same indices")
  testthat::expect_error(join(q1, q2), "doesn't have the same indices")
})


testthat::test_that("joining with a quosure.error object returns the quosure.error object", {

  q1 <- eval_code(new_quosure(), "x <- 1")
  error_q <- eval_code(new_quosure(), "y <- w")
  error_q2 <- eval_code(new_quosure(), "z <- w")

  testthat::expect_s3_class(join(q1, error_q), "quosure.error")
  testthat::expect_s3_class(join(error_q, error_q2), "quosure.error")
  testthat::expect_s3_class(join(error_q, q1), "quosure.error")

  # if joining two quosure.error objects keep the first
  testthat::expect_equal(join(error_q, error_q2), error_q)
})

