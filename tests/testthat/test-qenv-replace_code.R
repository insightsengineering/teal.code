q <- new_qenv()
qq <- within(q, i <- iris)

# argument checks ----
test_that("object argument accepts qenv", {
  testthat::expect_no_error(replace_code(qq, "i <- mtcars"))
})

test_that("object argument accepts qenv.error", {
  qe <- within(q, stop())
  testthat::expect_no_error(replace_code(qe, "i <- mtcars"))
})

test_that("code argument accepts character", {
  testthat::expect_no_error(replace_code(qq, "i <- mtcars"))
})

test_that("code argument accepts language", {
  testthat::expect_no_error(replace_code(qq, quote(i <- mtcars)))
})

test_that("code argument accepts expression", {
  testthat::expect_no_error(replace_code(qq, expression(i <- mtcars)))
})

# code replacement ----
testthat::test_that("length of @code is unchanged", {
  qr <- replace_code(qq, "i <- mtcars")
  testthat::expect_identical(length(get_code(qq)), length(get_code(qr)))
})

testthat::test_that("contents of @code are changed", {
  qr <- replace_code(qq, "i <- mtcars")
  testthat::expect_failure(testthat::expect_identical(get_code(qq), get_code(qr)))
})

testthat::test_that("code passed to replace_code is placed in @code", {
  replacement <- "i <- mtcars"
  qr <- replace_code(qq, replacement)
  testthat::expect_true(replacement %in% get_code(qr))
})

testthat::test_that("code passed to replace_code is replaces last element of @code", {
  qq <- within(qq, m <- mtcars)
  replacement <- "i <- mtcars"
  qr <- replace_code(qq, replacement)
  testthat::expect_identical(replacement, get_code(qr)[length(get_code(qr))])
})

testthat::test_that("non-last elements of @code are unaffected", {
  qq <- within(qq, m <- mtcars)
  replacement <- "i <- mtcars"
  qr <- replace_code(qq, replacement)
  before <- get_code(qq)[-length(get_code(qq))]
  after <- get_code(qr)[-length(get_code(qr))]
  testthat::expect_identical(before, after)
})

# edge cases ----
testthat::test_that("no changes are made to empty @code slot", {
  qe <- replace_code(q, "i <- mtcars")
  testthat::expect_identical(get_code(qe), character(0L))
})
