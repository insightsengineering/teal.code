
test_that("dev_suppress function supress printing plot on IDE", {
  expect_no_error(dev_suppress(plot(1:10)))

  initial_pdf_count <- sum(dev.list())
  dev_suppress(plot(1:10))
  final_pdf_count <- sum(dev.list())

  expect_equal(final_pdf_count, initial_pdf_count, label = "The PDF device should be closed after calling dev_suppress")
})


# lang2calls ------------------------------------------------------------------------------------------------------

testthat::test_that("format_expression returns expression/calls into characters without curly brackets", {
  expr1 <- expression({
    i <- iris
    m <- mtcars
  })
  expr2 <- expression(
    i <- iris,
    m <- mtcars
  )
  expr3 <- list(
    expression(i <- iris),
    expression(m <- mtcars)
  )
  cll1 <- quote({
    i <- iris
    m <- mtcars
  })
  cll2 <- list(
    quote(i <- iris),
    quote(m <- mtcars)
  )

  # function definition
  fundef <- quote(
    format_expression <- function(x) {
      x + x
      return(x)
    }
  )

  testthat::expect_identical(format_expression(expr1), "i <- iris\nm <- mtcars")
  testthat::expect_identical(format_expression(expr2), "i <- iris\nm <- mtcars")
  testthat::expect_identical(format_expression(expr3), "i <- iris\nm <- mtcars")
  testthat::expect_identical(format_expression(cll1), "i <- iris\nm <- mtcars")
  testthat::expect_identical(format_expression(cll2), "i <- iris\nm <- mtcars")
  testthat::expect_identical(
    format_expression(fundef), "format_expression <- function(x) {\n    x + x\n    return(x)\n}"
  )

})
