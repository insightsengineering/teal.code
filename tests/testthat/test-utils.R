test_that("dev_suppress function supress printing plot on IDE", {
  expect_no_error(dev_suppress(plot(1:10)))

  initial_pdf_count <- sum(dev.list())
  dev_suppress(plot(1:10))
  final_pdf_count <- sum(dev.list())

  expect_equal(final_pdf_count, initial_pdf_count, label = "The PDF device should be closed after calling dev_suppress")
})


# lang2calls ------------------------------------------------------------------------------------------------------
test_that("lang2calls returns list of calls given a language object", {
  expr1 <- expression(
    i <- iris
  )
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

  expect_true(is.list(lang2calls(expr1)) && all(vapply(lang2calls(expr1), is.call, logical(1L))))
  expect_true(is.list(lang2calls(expr2)) && all(vapply(lang2calls(expr2), is.call, logical(1L))))
  expect_true(is.list(lang2calls(call1)) && all(vapply(lang2calls(call1), is.call, logical(1L))))
  expect_true(is.list(lang2calls(call2)) && all(vapply(lang2calls(call2), is.call, logical(1L))))
})

test_that("lang2calls returns list of calls given a list of language objects", {
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

  expect_true(is.list(lang2calls(exprlist)) && all(vapply(lang2calls(exprlist), is.call, logical(1L))))
  expect_true(is.list(lang2calls(calllist)) && all(vapply(lang2calls(calllist), is.call, logical(1L))))
})

test_that("lang2calls returns atomics and symbols wrapped in list", {
  expect_identical(lang2calls("x"), list("x"))
  expect_identical(lang2calls(as.symbol("x")), list(as.symbol("x")))

  expect_identical(lang2calls(list("x")), list("x"))
  expect_identical(lang2calls(list(as.symbol("x"))), list(as.symbol("x")))
})
