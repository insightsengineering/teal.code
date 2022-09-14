testthat::test_that("get_code returns code of Quosure object", {
  q <- new_quosure(list2env(list(x = 1)), code = "x <- 1")
  q <- eval_code(q, "y <- x", name = "next_code")
  testthat::expect_equal(get_code(q), c("initial code" = "x <- 1", "next_code" = "y <- x"))
})

testthat::test_that("get_code called with quosure.error returns error with trace in error message", {
  q <- new_quosure(list2env(list(x = 1)), code = "x <- 1")
  q <- eval_code(q, "y <- x")
  q <- eval_code(q, "w <- v")

  code <- tryCatch(
    get_code(q),
    error = function(e) e
  )
  testthat::expect_equal(class(code), c("validation", "try-error", "simpleError", "error", "condition"))
  testthat::expect_equal(
    code$message,
    "object 'v' not found \n when evaluating Quosure code:\n w <- v\n\ntrace: \n x <- 1\n y <- x\n w <- v\n"
  )
})
