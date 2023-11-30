testthat::test_that("Joining two identical qenvs outputs the same object", {
  q1 <- eval_code(qenv(), quote(iris1 <- iris))
  q2 <- q1

  testthat::expect_true(.check_joinable(q1, q2))
  q <- join(q1, q2)

  testthat::expect_equal(q@env, q1@env)
  testthat::expect_identical(q@code, "iris1 <- iris")
  testthat::expect_identical(q@id, q1@id)
})

testthat::test_that("Joining two independent qenvs results in object having combined code and environments", {
  q1 <- eval_code(qenv(), quote(iris1 <- iris))
  q2 <- eval_code(qenv(), quote(mtcars1 <- mtcars))

  testthat::expect_true(.check_joinable(q1, q2))
  q <- join(q1, q2)

  testthat::expect_equal(q@env, list2env(list(iris1 = iris, mtcars1 = mtcars)))
  testthat::expect_identical(
    q@code,
    c("iris1 <- iris", "mtcars1 <- mtcars")
  )
  testthat::expect_identical(q@id, c(q1@id, q2@id))
})

testthat::test_that("Joined qenv does not duplicate common code", {
  env <- list2env(list(
    iris1 = iris,
    mtcars1 = mtcars
  ))

  q1 <- eval_code(qenv(), code = as.expression(c(quote(iris1 <- iris), quote(mtcars1 <- mtcars))))
  q2 <- q1
  q2 <- eval_code(q2, quote(mtcars2 <- mtcars))

  testthat::expect_true(.check_joinable(q1, q2))
  q <- join(q1, q2)

  testthat::expect_identical(
    q@code,
    c("iris1 <- iris\nmtcars1 <- mtcars", "mtcars2 <- mtcars")
  )
  testthat::expect_identical(q@id, c(q1@id, q2@id[2]))
})

testthat::test_that("Not able to join two qenvs if any of the shared objects changed", {
  q1 <- eval_code(qenv(), expression(iris1 <- iris, iris2 <- iris))
  q2 <- eval_code(qenv(), quote(iris1 <- head(iris)))

  testthat::expect_match(.check_joinable(q1, q2), "Not possible to join qenv objects")
  testthat::expect_error(join(q1, q2), "Not possible to join qenv objects")
})

testthat::test_that("join does not duplicate code but adds only extra code", {
  q1 <- eval_code(qenv(), expression(iris1 <- iris, mtcars1 <- mtcars))
  q2 <- q1
  q1 <- eval_code(q1, quote(iris2 <- iris))
  q2 <- eval_code(q2, quote(mtcars2 <- mtcars))

  testthat::expect_true(.check_joinable(q1, q2))
  q <- join(q1, q2)

  testthat::expect_identical(
    q@code,
    c("iris1 <- iris\nmtcars1 <- mtcars", "iris2 <- iris", "mtcars2 <- mtcars")
  )

  testthat::expect_equal(
    as.list(q@env),
    list(iris1 = iris, iris2 = iris, mtcars1 = mtcars, mtcars2 = mtcars)
  )

  testthat::expect_identical(q@id, c(q1@id, q2@id[2]))
})

testthat::test_that("Not possible to join qenvs which share some code when one of the shared object was modified", {
  env <- new.env()
  env$iris1 <- iris
  env$mtcars1 <- mtcars

  q1 <- eval_code(qenv(), code = expression(iris1 <- iris, mtcars1 <- mtcars))
  q2 <- q1
  q1 <- eval_code(q1, quote(iris2 <- iris))
  q2 <- eval_code(q2, quote(mtcars1 <- head(mtcars)))

  testthat::expect_error(join(q1, q2))
})

testthat::test_that("qenv objects are mergeable if they don't share any code (identified by id)", {
  q1 <- eval_code(qenv(), code = quote(a1 <- 1))
  q2 <- eval_code(qenv(), code = quote(a1 <- 1))
  testthat::expect_true(.check_joinable(q1, q2))

  cq <- join(q1, q2)
  testthat::expect_s4_class(cq, "qenv")
  testthat::expect_equal(cq@env, list2env(list(a1 = 1)))
  testthat::expect_identical(cq@code, c("a1 <- 1", "a1 <- 1"))
  testthat::expect_identical(cq@id, c(q1@id, q2@id))
})

testthat::test_that("qenv objects are mergeable if they share common initial qenv elements", {
  q1 <- eval_code(qenv(), code = quote(a1 <- 1))
  q2 <- eval_code(q1, quote(b1 <- 2))
  q1 <- eval_code(q1, quote(a2 <- 3))
  testthat::expect_true(.check_joinable(q1, q2))

  cq <- join(q1, q2)
  testthat::expect_s4_class(cq, "qenv")
  testthat::expect_equal(cq@env, list2env(list(a1 = 1, b1 = 2, a2 = 3)))
  testthat::expect_identical(
    cq@code,
    c("a1 <- 1", "a2 <- 3", "b1 <- 2")
  )
  testthat::expect_identical(cq@id, union(q1@id, q2@id))
})

testthat::test_that(
  "qenv objects aren't mergeable if they share common qenv elements proceeded with some other code",
  {
    q1 <- eval_code(qenv(), code = quote(a1 <- 1))
    q2 <- eval_code(qenv(), code = quote(b1 <- 2))
    q_common <- eval_code(qenv(), quote(c1 <- 3))
    q1 <- join(q1, q_common)
    q2 <- join(q2, q_common)
    testthat::expect_match(.check_joinable(q1, q2), "these objects cannot be joined")
    testthat::expect_error(join(q1, q2), "these objects cannot be joined")
  }
)

testthat::test_that("qenv objects are not mergable if they have multiple common streaks", {
  q_common1 <- eval_code(qenv(), code = quote(c1 <- 1))
  q_common2 <- eval_code(qenv(), code = quote(c2 <- 2))

  q1 <- eval_code(q_common1, quote(a1 <- 3))
  q1 <- join(q1, q_common2) # c1, a1, c2
  q2 <- join(q_common1, q_common2) # c1, c2

  testthat::expect_match(.check_joinable(q1, q2), "it's impossible to determine the evaluation's order")
  testthat::expect_error(join(q1, q2), "it's impossible to determine the evaluation's order")
})


testthat::test_that("joining with a qenv.error object returns the qenv.error object", {
  q1 <- eval_code(qenv(), quote(x <- 1))
  error_q <- eval_code(qenv(), quote(y <- w))
  error_q2 <- eval_code(qenv(), quote(z <- w))

  testthat::expect_s3_class(join(q1, error_q), "qenv.error")
  testthat::expect_s3_class(join(error_q, error_q2), "qenv.error")
  testthat::expect_s3_class(join(error_q, q1), "qenv.error")

  # if joining two qenv.error objects keep the first
  testthat::expect_equal(join(error_q, error_q2), error_q)
})

testthat::test_that("Joining two independent qenvs with warnings results in object having combined warnings", {
  q1 <- eval_code(qenv(), "warning('This is warning 1')")
  q2 <- eval_code(qenv(), "warning('This is warning 2')")

  testthat::expect_true(.check_joinable(q1, q2))
  q <- join(q1, q2)

  testthat::expect_equal(
    q@warnings,
    c(
      "> This is warning 1\n",
      "> This is warning 2\n"
    )
  )
})

testthat::test_that("Joining two independent qenvs with messages results in object having combined messages", {
  q1 <- eval_code(qenv(), "message('This is message 1')")
  q2 <- eval_code(qenv(), "message('This is message 2')")

  testthat::expect_true(.check_joinable(q1, q2))
  q <- join(q1, q2)

  testthat::expect_equal(
    q@messages,
    c(
      "> This is message 1\n",
      "> This is message 2\n"
    )
  )
})
