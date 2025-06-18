testthat::test_that("", {
  q <- qenv()
  q1 <- eval_code(q, expression(iris, mtcars))
  testthat::expect_identical(
    get_outputs(q1),
    list(iris, mtcars)
  )
})
