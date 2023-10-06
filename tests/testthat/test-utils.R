test_that("dev_suppress function supress printing plot on IDE", {
  expect_no_error(dev_suppress(plot(1:10)))

  initial_pdf_count <- sum(dev.list())
  dev_suppress(plot(1:10))
  final_pdf_count <- sum(dev.list())

  expect_equal(final_pdf_count, initial_pdf_count, label = "The PDF device should be closed after calling dev_suppress")
})


# lang2calls ------------------------------------------------------------------------------------------------------
testthat::test_that("lang2calls returns list of calls given a language object", {
  expr1 <- expression({
    i <- iris
    m <- mtcars
  })
  expr2 <- expression(
    i <- iris,
    m <- mtcars
  )
  call1 <- quote(
    i <- iris
  )
  call2 <- quote({
    i <- iris
    m <- mtcars
  })

  testthat::expect_true(is.list(lang2calls(expr1)) && all(vapply(lang2calls(expr1), is.call, logical(1L))))
  testthat::expect_true(is.list(lang2calls(expr2)) && all(vapply(lang2calls(expr2), is.call, logical(1L))))
  testthat::expect_true(is.list(lang2calls(call1)) && all(vapply(lang2calls(call1), is.call, logical(1L))))
  testthat::expect_true(is.list(lang2calls(call2)) && all(vapply(lang2calls(call2), is.call, logical(1L))))
})

testthat::test_that("lang2calls returns list of calls given a list of language objects", {
  exprlist <- list(
    expression(i <- iris),
    expression(
      i <- iris,
      m <- mtcars
    )
  )
  calllist <- list(
    quote(i <- iris),
    quote({
      i <- iris
      m <- mtcars
    })
  )

  testthat::expect_true(is.list(lang2calls(exprlist)) && all(vapply(lang2calls(exprlist), is.call, logical(1L))))
  testthat::expect_true(is.list(lang2calls(calllist)) && all(vapply(lang2calls(calllist), is.call, logical(1L))))
})

testthat::test_that("lang2calls returns atomics and symbols as is", {
  testthat::expect_identical(lang2calls("x"), "x")
  testthat::expect_identical(lang2calls(as.symbol("x")), as.symbol("x"))

  testthat::skip(message = "unexplained behavior")
  testthat::expect_identical(lang2calls(list("x")), "x")
  testthat::expect_identical(lang2calls(list(as.symbol("x"))), as.symbol("x"))
})


testthat::test_that(
  "format_expression turns expression/calls or lists thereof into character strings without curly brackets",
  {
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
  }
)
