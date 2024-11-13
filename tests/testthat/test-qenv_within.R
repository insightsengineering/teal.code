# styler: off
# nolint start

# code acceptance ----
testthat::test_that("simple and compound expressions are evaluated", {
  q <- qenv()
  testthat::expect_no_error(
    within(q, 1 + 1)
  )
  testthat::expect_no_error(
    within(q, {
      1 + 1
    })
  )
})

testthat::test_that("multiline expressions are evaluated", {
  q <- qenv()
  testthat::expect_no_error(
    within(q, a <- function(x) {
      y <- x + 1
      y + 3
    })
  )
})

# code identity ----
testthat::test_that("styling of input code does not impact evaluation results", {
  q <- qenv()
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

  q <- qenv()
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
    paste(rep(c("1 + 1", "2 + 2"), 4L), collapse = "\n")
  )
})


# return value ----
testthat::test_that("within.qenv empty call doesn't change qenv object", {
  q <- qenv()
  q <- within(qenv(), i <- iris)
  qq <- within(q, {})
  testthat::expect_identical(q, qq)
})

testthat::test_that("within.qenv renturns a `qenv` where `@.xData` is a deep copy of that in `data`", {
  q <- qenv()
  q <- within(qenv(), i <- iris)
  qq <- within(q, i)
  testthat::expect_equal(q@.xData, qq@.xData)
  testthat::expect_false(identical(q@.xData, qq@.xData))
})

testthat::test_that("within.qenv renturns qenv.error even if evaluation raises error", {
  q <- qenv()
  q <- within(q, i <- iris)
  qq <- within(q, stop("right there"))
  testthat::expect_true(
    exists("qq", inherits = FALSE)
  )
  testthat::expect_s3_class(qq, "qenv.error")
})


# injecting values ----
testthat::test_that("external values can be injected into expressions through `...`", {
  q <- qenv()

  external_value <- "virginica"
  q <- within(q, {
    i <- subset(iris, Species == species)
  },
  species = external_value)

  testthat::expect_identical(get_code(q), "i <- subset(iris, Species == \"virginica\")")
})

testthat::test_that("external values are not taken from calling frame", {
  q <- qenv()
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

testthat::test_that("within run on qenv.error returns the qenv.error as is", {
  q <- qenv()
  q <- within(q, i <- iris)
  qe <- within(q, stop("right there"))
  qee <- within(qe, m <- mtcars)

  testthat::expect_identical(qe, qee)
})
