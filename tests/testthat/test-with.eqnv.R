# styler: off
# nolint start

# creation ----
testthat::test_that("quenv is created empty with attributes as empty lists", {
  testthat::expect_no_error(q <- quenv())
  testthat::expect_s3_class(q, "quenv")
  testthat::expect_identical(
    attributes(q),
    list(
      code = list(),
      errors = list(),
      warnings = list(),
      messages = list(),
      class = c("quenv", "environment")
    )
  )
})


# evaluation ----
## code acceptance ----
# internal functions .prepare_code and .eval_one are tested by running `with`
testthat::test_that("simple expressions passed `expr` are evaluated", {
  q <- quenv()
  testthat::expect_no_error(with(q, 1 + 1))
  testthat::expect_no_error(with(q, iris))
})

testthat::test_that("compound expressions passed to `expr` are evaluated", {
  q <- quenv()
  testthat::expect_no_error(
    with(q, {
      1 + 1
    })
  )
  testthat::expect_no_error(
    with(q, {
      1 + 1
      2 + 2
    })
  )
  testthat::expect_no_error(
    with(q, {
      1 + 1; 2 + 2
    })
  )
  testthat::expect_no_error(
    with(q, {
      1 +
        1
    })
  )
})

testthat::test_that("sipmle expressions as literal strings passed to `text` are evaluated", {
  q <- quenv()
  testthat::expect_no_error(with(q, text = "1 + 1"))
})

testthat::test_that("compound expressions as literal strings passed to `text` are evaluated", {
  q <- quenv()
  testthat::expect_no_error(
    with(q, text = "{
      1 + 1
    }")
  )
  testthat::expect_no_error(
    with(q, text = "{
      1 + 1
      2 + 2
    }")
  )
  testthat::expect_no_error(
    with(q, text = "{
      1 + 1; 2 + 2
    }")
  )
  testthat::expect_no_error(
    with(q, text = "{
      1 +
        1
    }")
  )
})

testthat::test_that("simple expressions as character vectors passed to `text` are evaluated", {
  q <- quenv()
  expressions <- c(
    "1 + 1",
    "1 + 1
       2 + 2",
    "1 + 1; 2 + 2",
    "1 +
     1"
  )
  testthat::expect_no_error(
    with(q, text = expressions)
  )
})

testthat::test_that("compound expressions as character vectors passed to `text` are evaluated", {
  q <- quenv()
  expressions <- c(
    "{1 + 1}",
    "{1 + 1
      2 + 2}",
    "{
       1 + 1
       2 + 2
     }",
    "{
      1 +
       1
     }"
  )
  testthat::expect_no_error(
    with(q, text = expressions)
  )
})

testthat::test_that("sipmle expressions from file passed to `text` are evaluated", {
  q <- quenv()
  expressions <- c(
    "1 + 1",
    "1 + 1
       2 + 2",
    "1 + 1; 2 + 2",
    "1 +
     1"
  )
  file <- tempfile()
  writeLines(expressions, file)
  testthat::expect_no_error(
    with(q, text = readLines(file))
  )
  unlink(file)
})

testthat::test_that("compound expressions from file passed to `text` are evaluated", {
  q <- quenv()
  expressions <- c(
    "{1 + 1}",
    "{1 + 1
      2 + 2}",
    "{
       1 + 1
       2 + 2
     }",
    "{
      1 +
       1
     }"
  )
  file <- tempfile()
  writeLines(expressions, file)
  testthat::expect_no_error(
    with(q, text = readLines(file))
  )
  unlink(file)
})

testthat::test_that("characters passed to `expr` raise errors", {
  q <- quenv()
  testthat::expect_error(with(q, "1 + 1"), "character vector passed to \"expr\":.+use the \"text\" argument instead")
})

testthat::test_that("character-only compound expressions passed `expr` are ignored", {
  q <- quenv()
  with(q, {"1 + 1"})
  testthat::expect_identical(attributes(q), attributes(quenv()))
})


# variable assignment ----
testthat::test_that("direct assignment to quenv is forbidden", {
  q <- quenv()
  testthat::expect_error(q$i <- iris, regexp = "Direct assignment is forbidden")
  testthat::expect_error(q[["i"]] <- iris, regexp = "Direct assignment is forbidden")
  testthat::expect_no_error(with(q, i <- iris))
})


# variable access ----
testthat::test_that("variables in quenv can be accessed", {
  q <- quenv()
  with(q, i <- iris)
  testthat::expect_no_error(q$i)
  testthat::expect_no_error(q[["i"]])
  testthat::expect_identical(q$i, iris)
  testthat::expect_identical(q[["i"]], iris)
  testthat::expect_error(q["i"], "Use.+to access variables.")
})


# extracting conditions ----
testthat::test_that("get_conditions extracts requested conditions as lists of strings", {
  q <- quenv()
  testthat::expect_error({
    with(q, {
      i <- iris
      m <- mtcars
      mm <- m[m$cyl == 4, ]
      message("this is a message")
      warning("this is a warning")
      stop("this is an error")
    })
  })

  testthat::expect_identical(
    get_conditions(q, "messages"),
    list(
      "this is a message"
    )
  )
  testthat::expect_identical(
    get_conditions(q, "warnings"),
    list(
      "this is a warning"
    )
  )
  testthat::expect_identical(
    get_conditions(q, "errors"),
    list(
      "this is an error"
    )
  )
  testthat::expect_identical(
    get_conditions(q, "all"),
    list(
      errors = list(
        "this is an error"
      ),
      warnings = list(
        "this is a warning"
      ),
      messages = list(
        "this is a message"
      )
    )
  )
})


# extracting code ----
testthat::test_that("get_code_quenv extracts code identical to the evaluated one", {
  q <- quenv()
  with(q, {
    i <- iris
    m <- mtcars
    mm <- m[m$cyl == 4, ]
  })

  testthat::expect_identical(
    get_code_quenv(q),
    list(
      quote(i <- iris),
      quote(m <- mtcars),
      quote(mm <- m[m$cyl == 4, ])
    )
  )
})

testthat::test_that("get_code_quenv juxtaposes expressions with their respective conditions", {
  q <- quenv()
  testthat::expect_error({
    with(q, {
      i <- iris
      m <- mtcars
      mm <- m[m$cyl == 4, ]
      message("this is a message")
      warning("this is a warning")
      stop("this is an error")
    })
  })

  summary <- get_code_quenv(q, include_messages = TRUE)
  testthat::expect_s3_class(summary, "data.frame")
  testthat::expect_named(summary, c("code", "error", "warning", "message"))
  lapply(summary, testthat::expect_type, type = "character")
  testthat::expect_identical(
    summary[["code"]],
    c(
      "i <- iris",
      "m <- mtcars",
      "mm <- m[m$cyl == 4, ]",
      "message(\"this is a message\")",
      "warning(\"this is a warning\")",
      "stop(\"this is an error\")"
    )
  )
  testthat::expect_identical(
    summary[["error"]],
    c("", "", "", "", "", "this is an error")
  )
  testthat::expect_identical(
    summary[["warning"]],
    c("", "", "", "", "this is a warning", "")
  )
  testthat::expect_identical(
    summary[["message"]],
    c("", "", "", "this is a message", "", "")
  )
})


# evaluation, ctd. ----
## code identity ----
testthat::test_that("code passed as expression or character is evaluated as identical", {
  q1 <- quenv()
  with(q1, 1 + 1)
  with(q1, {
    1 + 1
  })
  with(q1, {
    1 + 1
    2 + 2
  })
  with(q1, {
    1 + 1; 2 + 2
  })
  with(q1, {
    1 +
      1
  })
  with(q1, {
    if (1 + 1) {
      "> 0"
    } else {
      "== 0"
    }
  })

  q2 <- quenv()
  with(q2, text = "1 + 1")
  with(q2, text = "{
    1 + 1
  }")
  with(q2, text = "{
    1 + 1
    2 + 2
  }")
  with(q2, text = "{
    1 + 1; 2 + 2
  }")
  with(q2, text = "{
    1 +
      1
  }")
  with(q2, text = "{
    if (1 + 1) {
      \"> 0\"
    } else {
      \"== 0\"
    }
  }")

  expressions <- c(
    "1 + 1",
    "{
      1 + 1
     }",
    "{
      1 + 1
      2 + 2
     }",
    "{
      1 + 1; 2 + 2
     }",
    "{
      1 +
       1
     }",
    "{
       if (1 + 1) {
         \"> 0\"
       } else {
         \"== 0\"
       }
     }"
  )
  q3 <- quenv()
  with(q3, text = expressions)
  testthat::expect_identical(
    get_code_quenv(q1),
    get_code_quenv(q2)
  )
  testthat::expect_identical(
    get_code_quenv(q2),
    get_code_quenv(q3)
  )
})

testthat::test_that("differently formulated expressions yield the same code", {
  q <- quenv()
  with(q, 1 + 1)
  with(q, {1 + 1})
  with(q, {
    1 + 1
  })
  with(q, {
    1 +
      1
  })
  all_code <- get_code_quenv(q)
  testthat::expect_identical(
    all_code,
    rep(list(quote(1 + 1)), 4L)
  )

  q <- quenv()
  with(q, {1 + 1; 2 + 2})
  with(q, {
    1 + 1; 2 + 2
  })
  with(q, {
    1 + 1
    2 + 2
  })
  with(q, {
    1 + 1;
    2 + 2
  })
  all_code <- get_code_quenv(q)
  all_code_pairs <- lapply(seq_len(4L), function(x) all_code[((x - 1L) * 2L) + 1:2])
  testthat::expect_identical(
    all_code,
    rep(list(quote(1 + 1), quote(2 + 2)), 4L)
  )
})

## injecting values ----
testthat::test_that("external values can be injected into native expressions through `...`", {
  q <- quenv()

  with(q, {
    i <- subset(iris, Species == "setosa")
  })

  testthat::expect_error(
    with(q, {
      ii <- subset(iris, Species == species)
    }),
    "Evaluation failed"
  )
  testthat::expect_identical(
    get_conditions(q, "errors"),
    list(
      "object 'species' not found"
    )
  )

  with(q, {
    iii <- subset(iris, Species == species)
  },
  species = "virginica")

  external_value <- "versicolor"
  with(q, {
    iiii <- subset(iris, Species == species)
  },
  species = external_value)

  testthat::expect_identical(
    get_code_quenv(q),
    list(
      quote(i <- subset(iris, Species == "setosa")),
      quote(ii <- subset(iris, Species == species)),
      quote(iii <- subset(iris, Species == "virginica")),
      quote(iiii <- subset(iris, Species == "versicolor"))
    )
  )
})

testthat::test_that("external values can be injected into (literal) character expressions through `...`", {
  q <- quenv()

  with(q, text = "i <- subset(iris, Species == \"setosa\")")

  testthat::expect_error(
    with(q, text = "ii <- subset(iris, Species == species)"),
    "Evaluation failed"
  )
  testthat::expect_identical(
    get_conditions(q, "errors"),
    list(
      "object 'species' not found"
    )
  )

  with(q, text = "iii <- subset(iris, Species == species)", species = "virginica")

  external_value <- "versicolor"
  with(q, text = "iiii <- subset(iris, Species == species)", species = external_value)

  testthat::expect_identical(
    get_code_quenv(q),
    list(
      quote(i <- subset(iris, Species == "setosa")),
      quote(ii <- subset(iris, Species == species)),
      quote(iii <- subset(iris, Species == "virginica")),
      quote(iiii <- subset(iris, Species == "versicolor"))
    )
  )
})

testthat::test_that("external values can be injected into (value) character expressions through `...`", {
  q <- quenv()

  expression <- "i <- subset(iris, Species == \"setosa\")"
  with(q, text = expression)

  expression <- "ii <- subset(iris, Species == species)"
  testthat::expect_error(
    with(q, text = expression),
    "Evaluation failed"
  )
  testthat::expect_identical(
    get_conditions(q, "errors"),
    list(
      "object 'species' not found"
    )
  )

  expression <- "iii <- subset(iris, Species == species)"
  with(q, text = expression, species = "virginica")

  expression <- "iiii <- subset(iris, Species == species)"
  external_value <- "versicolor"
  with(q, text = expression, species = external_value)

  testthat::expect_identical(
    get_code_quenv(q),
    list(
      quote(i <- subset(iris, Species == "setosa")),
      quote(ii <- subset(iris, Species == species)),
      quote(iii <- subset(iris, Species == "virginica")),
      quote(iiii <- subset(iris, Species == "versicolor"))
    )
  )
})


# format ----
# no tests for format method yet

# within ----
testthat::test_that("within.quenv renturns a deep copy of `data`", {
  q <- quenv()
  with(q, i <- iris)
  qq <- within(q, text = "")
  testthat::expect_equal(q, qq)

  q <- quenv()
  with(q, i <- iris)
  qq <- within(q, m <- mtcars)
  testthat::expect_failure(
    testthat::expect_equal(q, qq)
  )
})

testthat::test_that("within.quenv renturns even if evaluation raises error", {
  q <- quenv()
  with(q, i <- iris)
  try(qq <- within(q, stop("right there")))
  testthat::expect_true(
    exists("qq", mode = "environment", inherits = FALSE)
  )
})

# nolint end
# styler: on
