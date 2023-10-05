# styler: off
# nolint start

# evaluation ----
## code acceptance ----
testthat::test_that("simple expressions are evaluated", {
  q <- new_qenv()
  testthat::expect_no_error(within(q, 1 + 1))
})

testthat::test_that("compound expressions are evaluated", {
  q <- new_qenv()
  testthat::expect_no_error(
    within(q, {
      1 + 1
    })
  )
  testthat::expect_no_error(
    within(q, {
      1 + 1
      2 + 2
    })
  )
  testthat::expect_no_error(
    within(q, {
      1 + 1; 2 + 2
    })
  )
  testthat::expect_no_error(
    within(q, {
      1 +
        1
    })
  )
})


## code identity ----
testthat::test_that("differently formulated expressions yield the same code", {
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
    rep("1 + 1", 4L)
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
  all_code_pairs <- lapply(seq_len(4L), function(x) all_code[((x - 1L) * 2L) + 1:2])
  testthat::expect_identical(
    all_code,
    rep(c("1 + 1", "2 + 2"), 4L)
  )
})


## injecting values ----
testthat::test_that("external values can be injected into expressions through `...`", {
  q <- new_qenv()

  q1 <- within(q, {
    i <- subset(iris, Species == "virginica")
  })

  q2 <- within(q, {
    i <- subset(iris, Species == species)
  },
  species = "virginica")

  external_value <- "virginica"
  q3 <- within(q, {
    i <- subset(iris, Species == species)
  },
  species = external_value)

  testthat::expect_identical(get_code(q1), get_code(q2))
  testthat::expect_identical(get_code(q1), get_code(q3))
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

# return value ----
testthat::test_that("within.qenv renturns a deep copy of `data`", {
  q <- new_qenv()
  q <- within(new_qenv(), i <- iris)
  qq <- within(q, {})
  testthat::expect_equal(q, qq)

  q <- new_qenv()
  q <- within(q, i <- iris)
  qq <- within(q, m <- mtcars)
  testthat::expect_failure(
    testthat::expect_equal(q, qq)
  )
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

# nolint end
# styler: on
