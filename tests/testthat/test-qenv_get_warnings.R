testthat::test_that("get_warnings accepts a qenv object and returns character", {
  q <- eval_code(qenv(), quote(warning("This is a warning!")))
  testthat::expect_identical(
    get_warnings(q),
    paste0(
      "~~~ Warnings ~~~\n\n> This is a warning!\nwhen running code:\nwarning(\"This is a warning!\")\n\n",
      "~~~ Trace ~~~\n\nwarning(\"This is a warning!\")"
    )
  )
})

testthat::test_that("get_warnings accepts a qenv.error object and returns NULL", {
  q <- eval_code(qenv(), quote(error("This is a error!")))
  testthat::expect_null(get_warnings(q))
})

testthat::test_that("get_warnings accepts a NULL object and returns NULL", {
  testthat::expect_null(get_warnings(NULL))
})

testthat::test_that("get_warnings accepts a qenv object with no warning and returns NULL", {
  q <- eval_code(qenv(), quote("x <- 1"))
  testthat::expect_null(get_warnings(q))
})

testthat::test_that("get_warnings accepts a qenv object with 2 warnings", {
  q <- qenv()
  q <- eval_code(q, quote(warning("This is a warning 1!")))
  q <- eval_code(q, quote(warning("This is a warning 2!")))
  testthat::expect_identical(
    get_warnings(q),
    paste0(
      "~~~ Warnings ~~~\n\n> This is a warning 1!\nwhen running code:\nwarning(\"This is a warning 1!\")",
      "\n\n> This is a warning 2!\nwhen running code:\nwarning(\"This is a warning 2!\")\n\n",
      "~~~ Trace ~~~\n\nwarning(\"This is a warning 1!\")\nwarning(\"This is a warning 2!\")"
    )
  )
})

testthat::test_that("get_warnings accepts a qenv object with a single eval_code returning 2 warnings", {
  q <- eval_code(qenv(), quote({
    warning("This is a warning 1!")
    warning("This is a warning 2!")
  }))
  testthat::expect_identical(
    get_warnings(q),
    paste(
      c(
        "~~~ Warnings ~~~\n",
        "> This is a warning 1!",
        "when running code:",
        "warning(\"This is a warning 1!\")\n",
        "> This is a warning 2!",
        "when running code:",
        "warning(\"This is a warning 2!\")\n",
        "~~~ Trace ~~~\n",
        "warning(\"This is a warning 1!\")",
        "warning(\"This is a warning 2!\")"
      ),
      collapse = "\n"
    )
  )
})

testthat::test_that("get_warnings accepts a qenv object with 1 warning eval_code and 1 no warning eval_code", {
  q <- qenv()
  q <- eval_code(q, quote("x <- 1"))
  q <- eval_code(q, quote(warning("This is a warning 2!")))
  testthat::expect_identical(
    get_warnings(q),
    paste0(
      "~~~ Warnings ~~~\n\n> This is a warning 2!\nwhen running code:\nwarning(\"This is a warning 2!\")\n\n",
      "~~~ Trace ~~~\n\nx <- 1\nwarning(\"This is a warning 2!\")"
    )
  )
})
