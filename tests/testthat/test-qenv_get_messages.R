testthat::test_that("get_messages accepts a qenv object and returns character", {
  q <- eval_code(qenv(), quote(message("This is a message!")))
  testthat::expect_identical(
    get_messages(q),
    paste0(
      "~~~ Messages ~~~\n\n> This is a message!\nwhen running code:\nmessage(\"This is a message!\")\n\n",
      "~~~ Trace ~~~\n\nmessage(\"This is a message!\")"
    )
  )
})

testthat::test_that("get_messages accepts a qenv.error object and returns NULL", {
  q <- eval_code(qenv(), quote(error("This is a error!")))
  testthat::expect_null(get_messages(q))
})

testthat::test_that("get_messages accepts a NULL object and returns NULL", {
  testthat::expect_null(get_messages(NULL))
})

testthat::test_that("get_messages accepts a qenv object with no message and returns NULL", {
  q <- eval_code(qenv(), quote("x <- 1"))
  testthat::expect_null(get_messages(q))
})

testthat::test_that("get_messages accepts a qenv object with 2 messages", {
  q <- qenv()
  q <- eval_code(q, quote(message("This is a message 1!")))
  q <- eval_code(q, quote(message("This is a message 2!")))
  testthat::expect_identical(
    get_messages(q),
    paste0(
      "~~~ Messages ~~~\n\n> This is a message 1!\nwhen running code:\nmessage(\"This is a message 1!\")",
      "\n\n> This is a message 2!\nwhen running code:\nmessage(\"This is a message 2!\")\n\n",
      "~~~ Trace ~~~\n\nmessage(\"This is a message 1!\")\nmessage(\"This is a message 2!\")"
    )
  )
})

testthat::test_that("get_messages accepts a qenv object with a single eval_code returning 2 messages", {
  q <- eval_code(qenv(), quote({
    message("This is a message 1!")
    message("This is a message 2!")
  }))
  testthat::expect_identical(
    get_messages(q),
    paste(
      c(
        "~~~ Messages ~~~\n",
        "> This is a message 1!",
        "when running code:",
        "message(\"This is a message 1!\")\n",
        "> This is a message 2!",
        "when running code:",
        "message(\"This is a message 2!\")\n",
        "~~~ Trace ~~~\n",
        "message(\"This is a message 1!\")",
        "message(\"This is a message 2!\")"
      ),
      collapse = "\n"
    )
  )
})

testthat::test_that("get_messages accepts a qenv object with 1 message eval_code and 1 no message eval_code", {
  q <- qenv()
  q <- eval_code(q, quote("x <- 1"))
  q <- eval_code(q, quote(message("This is a message 2!")))
  testthat::expect_identical(
    get_messages(q),
    paste0(
      "~~~ Messages ~~~\n\n> This is a message 2!\nwhen running code:\nmessage(\"This is a message 2!\")\n\n",
      "~~~ Trace ~~~\n\nx <- 1\nmessage(\"This is a message 2!\")"
    )
  )
})
