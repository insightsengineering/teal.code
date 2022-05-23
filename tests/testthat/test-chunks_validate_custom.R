testthat::test_that("chunks_validate_custom", {
  x <- chunks_new()
  x$push(quote(a <- 1))
  chunks_safe_eval(x)
  testthat::expect_null(chunks_validate_custom(quote(a == 1), chunks = x))
  testthat::expect_null(chunks_validate_custom(quote(c(a == 1, a == 1)), chunks = x))
  testthat::expect_error(
    chunks_validate_custom(quote(c(a == 1, a == 2)), chunks = x),
    "c\\(a == 1, a == 2\\) is not all TRUE",
    class = "shiny.silent.error"
  )
  testthat::expect_error(
    chunks_validate_custom(quote(c(TRUE, FALSE, NA)), chunks = x),
    "x returned NA value\\(s\\) when evaluated in chunks environment"
  )
  testthat::expect_error(
    chunks_validate_custom(quote({
      NULL
    }), chunks = x), # nolint
    "x did not return a logical value when evaluated in chunks environment, but NULL instead"
  )
  testthat::expect_error(
    chunks_validate_custom(quote(1 + 1), chunks = x),
    "x did not return a logical value when evaluated in chunks environment, but double instead"
  )
  testthat::expect_error(
    chunks_validate_custom(quote(a == 2), chunks = x),
    "a == 2 is not all TRUE",
    class = "shiny.silent.error"
  )
  testthat::expect_error(
    chunks_validate_custom(quote(a == 2), msg = "custome msg", chunks = x),
    "custome msg",
    class = "shiny.silent.error"
  )
  testthat::expect_error(
    chunks_validate_custom(quote(a), chunks = x),
    "x did not return a logical value when evaluated in chunks environment, but double instead"
  )
  testthat::expect_error(
    chunks_validate_custom(1, chunks = x),
    "x is not an expression"
  )
  testthat::expect_error(
    chunks_validate_custom(quote({
      TRUE
    }), msg = c("msg1", "msg2"), chunks = x), # nolint
    "msg is not a character or its length is more than 1"
  )
  testthat::expect_error(
    chunks_validate_custom(quote({
      TRUE
    }), chunks = 1), # nolint
    "chunks is not a chunks object"
  )
})
