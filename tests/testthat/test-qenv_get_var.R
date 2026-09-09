test_that("`$` and `[[` return error if object is qenv.error", {
  q <- eval_code(qenv(), quote(x <- 1))
  q <- eval_code(q, quote(y <- w * x))

  expect_error(q[["x"]], "when evaluating qenv code")
  expect_error(q$x, "when evaluating qenv code")
})

test_that("`$` and `[[` return object from qenv environment", {
  q <- eval_code(qenv(), quote(x <- 1))
  q <- eval_code(q, quote(y <- 5 * x))

  expect_equal(q[["x"]], 1)
  expect_equal(q$x, 1)
})


test_that("`$` and `[[` return NULL if object not in qenv environment", {
  q <- eval_code(qenv(), quote(x <- 1))
  q <- eval_code(q, quote(y <- 5 * x))

  expect_null(q[["w"]])
  expect_null(q$w)
})

test_that("`$` and `[[` only returns objects from qenv, not parent environment(s)", {
  q <- qenv()

  new_env <- new.env(parent = parent.env(q))
  new_env$an_object <- 2

  expect_null(q[["an_object"]])
  expect_null(q$an_object)
})

test_that("`$` and `[[` only returns objects from qenv, not .GlobalEnv", {
  if (is.null(.GlobalEnv)) {
    withr::defer(rm("an_object", envir = .GlobalEnv))
  } else {
    old_object <- .GlobalEnv$an_object
    withr::defer(.GlobalEnv$an_object <- old_object) # nolint: object_name.
  }
  .GlobalEnv$an_object <- iris # nolint: object_name.

  q <- qenv()
  expect_null(q[["iris"]])
  expect_null(q$iris)
})
