testthat::test_that("get_warnings accepts a qenv object and returns character", {
  q <- new_qenv() %>% eval_code(bquote(warning("This is a warning!")))
  testthat::expect_identical(
    get_warnings(q),
    paste0(
      "Warnings: \n> This is a warning! \nWhen running code:\n warning(\"This is a warning!\")\n\n",
      "Trace:\nwarning(\"This is a warning!\")"
    )
  )
})

testthat::test_that("get_warnings accepts a qenv.error object and returns NULL", {
  q <- new_qenv() %>% eval_code(bquote(error("This is a error!")))
  testthat::expect_null(get_warnings(q))
})

testthat::test_that("get_warnings accepts a qenv object with no warning and returns NULL", {
  q <- new_qenv() %>% eval_code(bquote("x <- 1"))
  testthat::expect_null(get_warnings(q))
})

testthat::test_that("get_warnings accepts a qenv object with 2 warnings", {
  q <- new_qenv() %>% eval_code(bquote(warning("This is a warning 1!"))) %>% eval_code(bquote(warning("This is a warning 2!")))
  testthat::expect_identical(
    get_warnings(q),
    paste0(
      "Warnings: \n> This is a warning 1! \nWhen running code:\n warning(\"This is a warning 1!\")",
      " \n> This is a warning 2! \nWhen running code:\n warning(\"This is a warning 2!\")\n\n",
      "Trace:\nwarning(\"This is a warning 1!\")\nwarning(\"This is a warning 2!\")"
    )
  )
})

testthat::test_that("get_warnings accepts a qenv object with a single eval_code returning 2 warnings", {
  q <- new_qenv() %>% eval_code(bquote({
    warning("This is a warning 1!")
    warning("This is a warning 2!")
  }))
  testthat::expect_identical(
    get_warnings(q),
    paste0(
      "Warnings: \n> This is a warning 1!This is a warning 2! \nWhen running code:\n {\n    ",
      "warning(\"This is a warning 1!\")\n    warning(\"This is a warning 2!\")\n}\n\nTrace:\n{\n",
      "    warning(\"This is a warning 1!\")\n    warning(\"This is a warning 2!\")\n}"
    )
  )
})

testthat::test_that("get_warnings accepts a qenv object with 1 warning eval_code and 1 no warning eval_code", {
  q <- new_qenv() %>% eval_code(bquote("x <- 1")) %>% eval_code(bquote(warning("This is a warning 2!")))
  testthat::expect_identical(
    get_warnings(q),
    paste0(
      "Warnings: \n> This is a warning 2! \nWhen running code:\n warning(\"This is a warning 2!\")\n\n",
      "Trace:\nx <- 1\nwarning(\"This is a warning 2!\")"
    )
  )
})
