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
testthat::test_that("code_replace replaces last element of the code", {
  qq <- within(qq, m <- mtcars)
  replacement <- "i <- mtcars"
  qr <- replace_code(qq, replacement)
  previous <- get_code(qq)
  current <- get_code(qr)

  previous <- head(strsplit(previous, split = "\n")[[1]], -1)
  testthat::expect_identical(current, paste0(c(previous, replacement), collapse = "\n"))
})

# edge cases ----
testthat::test_that("no changes are made to empty @code slot", {
  qe <- replace_code(q, "i <- mtcars")
  testthat::expect_identical(get_code(qe), character(0L))
})
