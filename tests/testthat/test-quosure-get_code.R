testthat::test_that("get_code returns code (character by default) of Quosure object", {
  q <- new_quosure(list2env(list(x = 1)), code = quote(x <- 1))
  q <- eval_code(q, quote(y <- x))
  testthat::expect_equal(get_code(q), c("x <- 1", "y <- x"))
})

testthat::test_that("get_code returns code elements being code-blocks as character(1)", {
  q <- new_quosure(list2env(list(x = 1)), code = quote(x <- 1))
  q <- eval_code(
    q,
    quote({
      y <- x
      z <- 5
    })
  )
  testthat::expect_equal(get_code(q), c("x <- 1", "{\n    y <- x\n    z <- 5\n}"))
})

testthat::test_that("get_code returns code (unparsed) of Quosure object if deparse = FALSE", {
  q <- new_quosure(list2env(list(x = 1)), code = quote(x <- 1))
  q <- eval_code(q, quote(y <- x))
  testthat::expect_equal(get_code(q, deparse = FALSE), q@code)
})

testthat::test_that("get_code called with quosure.error returns error with trace in error message", {
  q1 <- new_quosure(list2env(list(x = 1)), code = quote(x <- 1))
  q2 <- eval_code(q1, quote(y <- x))
  q3 <- eval_code(q2, quote(w <- v))

  code <- tryCatch(
    get_code(q3),
    error = function(e) e
  )
  testthat::expect_equal(class(code), c("validation", "try-error", "simpleError", "error", "condition"))
  testthat::expect_equal(
    code$message,
    "object 'v' not found \n when evaluating Quosure code:\n w <- v\n\ntrace: \n x <- 1\n y <- x\n w <- v\n"
  )
})
