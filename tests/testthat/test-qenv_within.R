# styler: off
# nolint start

# code acceptance ----
test_that("simple and compound expressions are evaluated", {
  q <- qenv()
  expect_no_error(
    within(q, 1 + 1)
  )
  expect_no_error(
    within(q, {
      1 + 1
    })
  )
})

test_that("multiline expressions are evaluated", {
  q <- qenv()
  expect_no_error(
    within(q, a <- function(x) {
      y <- x + 1
      y + 3
    })
  )
})

# code identity ----
test_that("styling of input code does not impact evaluation results", {
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
  expect_identical(
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
  expect_identical(
    all_code,
    paste(rep(c("1 + 1", "2 + 2"), 4L), collapse = "\n")
  )
})


# return value ----
test_that("within.qenv empty call doesn't change qenv object", {
  q <- qenv()
  q <- within(qenv(), i <- iris)
  qq <- within(q, {})
  expect_identical(q, qq)
})

test_that("within.qenv renturns a `qenv` where `@.xData` is a deep copy of that in `data`", {
  q <- qenv()
  q <- within(qenv(), i <- iris)
  qq <- within(q, i)
  expect_equal(q@.xData, qq@.xData)
  expect_false(identical(q@.xData, qq@.xData))
})

test_that("within.qenv renturns qenv.error even if evaluation raises error", {
  q <- qenv()
  q <- within(q, i <- iris)
  qq <- within(q, stop("right there"))
  expect_true(
    exists("qq", inherits = FALSE)
  )
  expect_s3_class(qq, "qenv.error")
})


# injecting values ----
test_that("external values can be injected into expressions through `...`", {
  q <- qenv()

  external_value <- "virginica"
  q <- within(q, {
    i <- subset(iris, Species == species)
  },
  species = external_value)

  expect_identical(get_code(q), "i <- subset(iris, Species == \"virginica\")")
})

test_that("external values are not taken from calling frame", {
  q <- qenv()
  species <- "setosa"
  qq <- within(q, {
    i <- subset(iris, Species == species)
  })
  expect_s3_class(qq, "qenv.error")
  expect_error(get_code(qq), "object 'species' not found")

  qq <- within(q, {
    i <- subset(iris, Species == species)
  },
  species = species)
  expect_s4_class(qq, "qenv")
  expect_identical(get_code(qq), "i <- subset(iris, Species == \"setosa\")")
})

# nolint end
# styler: on

test_that("within run on qenv.error returns the qenv.error as is", {
  q <- qenv()
  q <- within(q, i <- iris)
  qe <- within(q, stop("right there"))
  qee <- within(qe, m <- mtcars)

  expect_identical(qe, qee)
})

describe("within run with `=`", {
  it("single expression", {
    q <- qenv()
    q <- within(q, {
      i = 1 # nolintr: assigment. styler: off.
    })
  })

  it("multiple '=' expressions", {
    q <- qenv()
    q <- within(q, {
      j = 2 # nolintr: assigment. styler: off.
      i = 1 # nolintr: assigment. styler: off.
    })
    expect_equal(q$i, 1)
  })
})

test_that("Code executed with integer shorthand (1L) is the same as original", {
  q <- within(qenv(), a <- 1L)
  expect_identical(get_code(q), "a <- 1L")
})


test_that("Chinese characters are handled properly (issue 284)", {
  q <- within(qenv(), {
    "无进展生存期 (月)"
    "总生存期 (月)"
    "缓解持续时间 (月)"
    "确认的缓解持续时间 (月)"
  })

  expect_equal(lengths(strsplit(get_code(q), split = "\n", fixed = TRUE)), 4L)
})
