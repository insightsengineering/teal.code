# styler: off
# nolint start

# code acceptance ----
testthat::test_that("simple and compound expressions are evaluated", {
  q <- new_qenv()
  testthat::expect_no_error(
    within(q, 1 + 1)
  )
  testthat::expect_no_error(
    within(q, {
      1 + 1
    })
  )
})

# code identity ----
testthat::test_that("styling of input code does not impact evaluation results", {
  q <- new_qenv()
  q <- within(q, 1 + 1)
  q <- within(q, {1 + 1})
  q <- within(q, {
    1 + 1
  })
  q <- within(q, {
    1 +
      1
  })
  all_code <- get_code(q)
  testthat::expect_identical(
    all_code,
    paste(rep("1 + 1", 4L), collapse = "\n")
  )

  q <- new_qenv()
  q <- within(q, {1 + 1; 2 + 2})
  q <- within(q, {
    1 + 1; 2 + 2
  })
  q <- within(q, {
    1 + 1
    2 + 2
  })
  q <- within(q, {
    1 + 1;
    2 + 2
  })
  all_code <- get_code(q)
  testthat::expect_identical(
    all_code,
    paste(rep("1 + 1\n2 + 2", 4L), collapse = "\n")
  )
})


# return value ----
testthat::test_that("within.qenv renturns a `qenv` where `@env` is a deep copy of that in `data`", {
  q <- new_qenv()
  q <- within(new_qenv(), i <- iris)
  qq <- within(q, {})
  testthat::expect_equal(q@env, qq@env)
  testthat::expect_false(identical(q@env, qq@env))
})

testthat::test_that("within.qenv renturns qenv.error even if evaluation raises error", {
  q <- new_qenv()
  q <- within(q, i <- iris)
  qq <- within(q, stop("right there"))
  testthat::expect_true(
    exists("qq", inherits = FALSE)
  )
  testthat::expect_s3_class(qq, "qenv.error")
})


# injecting values ----
testthat::test_that("external values can be injected into expressions through `...`", {
  q <- new_qenv()

  external_value <- "virginica"
  q <- within(q, {
    i <- subset(iris, Species == species)
  },
  species = external_value)

  testthat::expect_identical(get_code(q), "i <- subset(iris, Species == \"virginica\")")
})

testthat::test_that("external values are not taken from calling frame", {
  q <- new_qenv()
  species <- "setosa"
  qq <- within(q, {
    i <- subset(iris, Species == species)
  })
  testthat::expect_s3_class(qq, "qenv.error")
  testthat::expect_error(get_code(qq), "object 'species' not found")

  qq <- within(q, {
    i <- subset(iris, Species == species)
  },
  species = species)
  testthat::expect_s4_class(qq, "qenv")
  testthat::expect_identical(get_code(qq), "i <- subset(iris, Species == \"setosa\")")
})

# nolint end
# styler: on
