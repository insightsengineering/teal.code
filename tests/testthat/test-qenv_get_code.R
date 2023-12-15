testthat::test_that("get_code returns code (character by default) of qenv object", {
  q <- qenv() |>
    eval_code(quote(x <- 1)) |>
    eval_code(quote(y <- x))
  testthat::expect_equal(get_code(q), paste(c("x <- 1", "y <- x"), collapse = "\n"))
})

testthat::test_that("get_code returns code elements being code-blocks as character(1)", {
  q <- qenv()
  q <- eval_code(q, quote(x <- 1))
  q <- eval_code(
    q,
    quote({
      y <- x
      z <- 5
    })
  )
  testthat::expect_equal(get_code(q), paste(c("x <- 1", "y <- x\nz <- 5"), collapse = "\n"))
})

testthat::test_that("get_code returns expression of qenv object if deparse = FALSE", {
  q <- qenv()
  q <- eval_code(q, quote(x <- 1))
  q <- eval_code(q, quote(y <- x))
  testthat::expect_equivalent(
    toString(get_code(q, deparse = FALSE)),
    toString(parse(text = paste(c("{", q@code, "}"), collapse = "\n"), keep.source = TRUE))
  )
})

testthat::test_that("get_code called with qenv.error returns error with trace in error message", {
  q1 <- qenv()
  q1 <- eval_code(q1, quote(x <- 1))
  q2 <- eval_code(q1, quote(y <- x))
  q3 <- eval_code(q2, quote(w <- v))

  code <- tryCatch(
    get_code(q3),
    error = function(e) e
  )
  testthat::expect_equal(class(code), c("validation", "try-error", "simpleError", "error", "condition"))
  testthat::expect_equal(
    code$message,
    "object 'v' not found \n when evaluating qenv code:\nw <- v\n\ntrace: \n x <- 1\n y <- x\n w <- v\n"
  )
})
