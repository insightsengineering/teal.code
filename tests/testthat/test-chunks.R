rm(list = ls())

# chunks_comment ====
testthat::test_that("chunk comment init", {
  testthat::expect_silent(chunk_comment$new("Comment"))
  testthat::expect_error(chunk_comment$new(c("A", "B")))
  testthat::expect_silent(chunk_comment$new())
})

testthat::test_that("chunk comment methods", {
  comment <- chunk_comment$new("This is a comment")
  testthat::expect_equal(
    comment$get_rcode(),
    "# This is a comment"
  )
  testthat::expect_silent(comment$eval())

  # Multiline comments
  comment <- chunk_comment$new("This is the first line\nThis is the second line")
  testthat::expect_equal(
    comment$get_rcode(),
    "# This is the first line\n# This is the second line"
  )

  # get_eval_info
  comment <- chunk_comment$new("This is a comment")
  testthat::expect_equal(
    comment$get_eval_info(),
    list(
      code = comment$get_rcode(),
      eval_info = list(
        flag = FALSE
      ),
      message_info = list(
        flag = FALSE,
        msg = NULL
      ),
      warning_info = list(
        flag = FALSE,
        msg = NULL
      ),
      error_info = list(
        flag = FALSE,
        msg = NULL
      )
    )
  )
})

# chunk ====
# * init ====
testthat::test_that("chunk init", {
  testthat::expect_silent(
    chunk1 <- chunk$new(
      expression = quote(
        apply(data, 2, function(x) x * y)
      )
    )
  ) # expect_silent
  testthat::expect_error(
    chunk1 <- chunk$new(
      expression = NULL
    )
  ) # expect_error
  testthat::expect_silent(
    chunk1 <- chunk$new(
      expression = quote(
        print(5)
      )
    )
  ) # expect_silent
})

# * eval ====
testthat::test_that("chunk eval", {
  testthat::expect_silent(
    chunk1 <- chunk$new(
      expression = quote({
        x <- 5
        x
      })
    )
  ) # expect_silent
  testthat::expect_equal(
    chunk1$eval(),
    5,
    info = "Single chunk evaluation without vars failed."
  )

  testthat::expect_silent({
    y <- 5
    data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
  }) # expect_silent
  testthat::expect_silent({
    chunk1 <- chunk$new(
      expression = quote({
        int_result <- apply(data, 2, function(x) x * y)
        int_result
      })
    )
  })

  testthat::expect_equal(
    chunk1$eval(),
    as.matrix(data.frame(x = c(5, 10, 15, 20, 25, 30), y = c(5, 5, 5, 5, 5, 5))),
    info = "Single chunk evaluation failed."
  )

  # Check that set_vars function is not available anymore
  testthat::expect_error(
    chunk1$set_vars(list(
      data = data.frame(x = c(1, 2), y = c(2, 2)),
      y = 5
    ))
  )
  testthat::expect_error(
    chunk1$set_expression(expression = quote(x <- 2))
  )

  testthat::expect_silent({
    chunk1 <- chunk$new(
      expression = quote({
        int_result <- data
        int_result[1, 1] <- int_result[1, 1] + y
        int_result
      })
    )
  })
  testthat::expect_equal(
    object = {
      data <- data.frame(x = c(1, 2), y = c(2, 2))
      y <- 5
      chunk1$eval()
    },
    data.frame(x = c(6, 2), y = c(2, 2)),
    info = "Single chunk evaluation with changed vars failed."
  )
})

# * state methods ====
testthat::test_that("chunk validation functions", {
  chunk_error <- chunk$new(quote(
    stop("Error")
  ))
  chunk_warning <- chunk$new(quote(
    warning("Warning")
  ))
  chunk_correct <- chunk$new(quote(
    x <- "correct"
  ))

  chunk_error$eval()
  chunk_warning$eval()
  chunk_correct$eval()

  testthat::expect_equal(chunk_error$is_ok(), FALSE)
  testthat::expect_equal(chunk_error$is_warnings(), FALSE)

  testthat::expect_equal(chunk_warning$is_ok(), TRUE)
  testthat::expect_equal(chunk_warning$is_warnings(), TRUE)

  testthat::expect_equal(chunk_correct$is_ok(), TRUE)
  testthat::expect_equal(chunk_correct$is_warnings(), FALSE)

  testthat::expect_false(chunk_correct$is_messages())
  chunk1 <- chunk$new(quote(
    message("Message")
  ))
  testthat::expect_false(chunk1$is_messages())
  chunk1$eval()
  testthat::expect_true(chunk1$is_messages())

  chunk1 <- chunk$new(quote(x <- "Test"))
  testthat::expect_false(chunk1$is_evaluated())
  chunk1$eval()
  testthat::expect_true(chunk1$is_evaluated())
})

# * get_rcode ====
testthat::test_that("chunk get_rcode", {
  testthat::expect_silent(
    chunk1 <- chunk$new(
      expression = quote(
        apply(data, 2, function(x) x * y)
      )
    )
  ) # expect_silent


  compare_string <- paste0(
    "apply(data, 2, function(x) x * y)"
  )
  testthat::expect_equal(
    object = {
      data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
      y <- 5
      chunk1$get_rcode()
    },
    compare_string,
    info = "Single chunk code generation failed."
  )

  testthat::expect_silent({
    td <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
    y <- 5
    chunk1 <- chunk$new(
      expression = substitute(apply(td, 2, function(x) x * y), list(y = y))
    )
  }) # expect_silent

  testthat::expect_equal(
    object = {
      chunk1$get_rcode()
    },
    "apply(td, 2, function(x) x * 5)",
    info = "Single chunk code generation with data failed."
  )
  testthat::expect_equal(
    chunk1$get_rcode(envir = list(td = data.frame(x = c(1, 2)))),
    "apply(td, 2, function(x) x * 5)",
    info = "Single chunk code generation with data and filtering failed."
  )

  testthat::expect_silent({
    rm(list = "td")
    chunk1$eval()
  })

  testthat::expect_match(
    chunk1$info_msg(),
    regexp = "The following errors"
  )
})

# * info_msg ====
testthat::test_that("chunk info_msg", {
  chunk1 <- chunk$new(quote(warning("A warning")))

  testthat::expect_match(
    chunk1$info_msg(),
    "Chunk not evaluated yet"
  )

  chunk1$eval()

  testthat::expect_match(
    chunk1$info_msg(),
    "The following warning"
  )

  chunk1 <- chunk$new(quote(stop("An error")))
  chunk1$eval()
  testthat::expect_match(
    chunk1$info_msg(),
    regexp = "The following errors"
  )

  chunk1 <- chunk$new(quote({
    warning("Warning no 1")
    warning("Warning no 2")
  }))
  chunk1$eval()
  testthat::expect_match(
    chunk1$info_msg(),
    regexp = ".*Warning no 1.*Warning no 2.*"
  )

  chunk1 <- chunk$new(quote(
    x <- "No error"
  ))
  chunk1$eval()
  testthat::expect_match(
    chunk1$info_msg(),
    "Everything went well!\n\nwhen evaluating the following code:\nx <- \"No error\"\n"
  )

  chunk1 <- chunk$new(quote({
    message("Message")
    warning("Warning")
  }))
  chunk1$eval()
  testthat::expect_match(
    chunk1$info_msg(),
    ".*Message.*Warning.*"
  )

  # Multiline
  chunk1 <- chunk$new(quote({
    message("First line message\nSecond line message")
  }))
  chunk1$eval()
  testthat::expect_equal(
    chunk1$info_msg(),
    paste0(
      "The following message(s) were outputted:\nFirst line message\nSecond line message\n\n",
      "Everything went well!\n\nwhen evaluating the following code:\n",
      "message(\"First line message\\nSecond line message\")\n"
    )
  )
})

# * get_warnings, get_messages ====
testthat::test_that("chunk get_warnings, get_messages", {
  chunk1 <- chunk$new(quote({
    message("Message")
    warning("Warning")
    stop("Error")
  }))

  # One line
  chunk1$eval()
  testthat::expect_match(
    chunk1$get_messages(),
    "Message"
  )
  testthat::expect_match(
    chunk1$get_warnings(),
    "Warning"
  )

  # Multi-line
  chunk1 <- chunk$new(quote({
    message("First line of message\nSecond line of message")
    warning("First line of warning\nSecond line of warning")
    stop("Error")
  }))
  chunk1$eval()
  testthat::expect_equal(
    chunk1$get_messages(),
    "First line of message\nSecond line of message"
  )
  testthat::expect_equal(
    chunk1$get_warnings(),
    "First line of warning\nSecond line of warning"
  )
})

# * info ====
testthat::test_that("chunk info", {
  testthat::expect_silent(
    chunk1 <- chunk$new(
      expression = quote(
        apply(data, 2, function(x) x * y)
      )
    )
  ) # expect_silent

  testthat::expect_equal(
    chunk1$info(),
    list(
      expression = quote(
        apply(data, 2, function(x) x * y)
      ),
      is_evaluated = FALSE,
      is_error = FALSE,
      is_warning = FALSE,
      is_message = FALSE,
      eval_msg = character(0)
    )
  )

  testthat::expect_silent({
    data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
    y <- 5
    chunk1$eval()
  })
  testthat::expect_equal(
    chunk1$info(),
    list(
      expression = quote(
        apply(data, 2, function(x) x * y)
      ),
      is_evaluated = TRUE,
      is_error = FALSE,
      is_warning = FALSE,
      is_message = FALSE,
      eval_msg = character(0)
    )
  )
})

# chunks ====
testthat::test_that("chunks eval with random ID", {
  testthat::expect_silent({
    rm(list = ls())
    data <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
    y <- 5
    chunks1 <- chunks$new()
  })
  testthat::expect_silent(
    chunks1$reset(envir = environment())
  )

  testthat::expect_equal(
    ls(chunks1$info()$envir),
    c("chunks1", "data", "y")
  )

  testthat::expect_silent(
    chunks1$push(id = "test1", x = quote(apply(data, 2, function(x) x * y)))
  )
  produced_value <- chunks1$eval()
  names(produced_value) <- NULL
  testthat::expect_equal(
    produced_value,
    as.matrix(data.frame(x = c(5, 10, 15, 20, 25, 30), y = c(5, 5, 5, 5, 5, 5))),
    info = "Single chunk evaluation inside chunks list failed."
  )

  testthat::expect_error(
    chunks1$set_env_var("y", 4),
    "attempt"
  )


  y <- 4
  data <- data

  testthat::expect_silent(
    chunks1$reset(envir = environment())
  )
  testthat::expect_silent(
    chunks1$push(id = "test2", x = quote(apply(data, 2, function(x) x * y)))
  )

  produced_value <- chunks1$eval()
  names(produced_value) <- NULL

  testthat::expect_equal(
    produced_value,
    as.matrix(data.frame(x = c(4, 8, 12, 16, 20, 24), y = c(4, 4, 4, 4, 4, 4))),
    info = "Single chunk evaluation inside chunks list failed. y = 4."
  )

  testthat::expect_silent({
    rm(list = ls())
    td <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
    y <- 4
    chunks1 <- chunks$new()
    chunks1$reset()
  })
  testthat::expect_silent(
    chunks1$push(id = "test3", x = quote(apply(td, 2, function(x) x * y)))
  )
  produced_value <- chunks1$eval()
  names(produced_value) <- NULL
  testthat::expect_equal(
    produced_value,
    as.matrix(data.frame(x = c(4, 8, 12, 16, 20, 24), y = c(4, 4, 4, 4, 4, 4))),
    info = "Single chunk evaluation inside chunks list failed. dataset given."
  )

  testthat::expect_true(
    length(chunks1$info()$remaining) == 0
  )

  testthat::expect_true(
    !is.na(chunks1$info()$id)
  )
})

testthat::test_that("chunks eval with given ID", {
  testthat::expect_silent({
    rm(list = ls())
    chunks1 <- chunks$new()
  })

  testthat::expect_silent({
    dataset <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
    y <- 4
    chunks1$reset()
  })

  testthat::expect_silent(
    chunks1$push(id = "tbl", x = quote(apply(dataset, 2, function(x) x * y)))
  )

  testthat::expect_silent(
    chunks1$eval()
  )

  # do not let people re-execute if already executed.
  testthat::expect_warning(
    chunks1$eval(),
    regexp = "already",
    info = "Single chunk evaluation inside chunks list should fail if already evaluated."
  )

  testthat::expect_silent({
    dataset <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
    y <- 4
    chunks1$reset()
  })

  testthat::expect_silent(
    chunks1$push(id = "tbl", x = quote(apply(dataset, 2, function(x) x * y)))
  )

  testthat::expect_equal(
    chunks1$eval(),
    as.matrix(data.frame(x = c(4, 8, 12, 16, 20, 24), y = c(4, 4, 4, 4, 4, 4))),
    info = "Single chunk evaluation inside chunks list should work with ID if it is the very last one."
  )

  testthat::expect_silent(
    chunks1$push(id = "tbl1", x = quote(apply(dataset, 2, function(x) x * y)))
  )


  testthat::expect_silent(
    chunks1$push(id = "y3", x = bquote(y <- 3))
  )
  testthat::expect_silent(
    chunks1$push(id = "tbl3", x = quote(apply(dataset, 2, function(x) x * y)))
  )
  testthat::expect_warning(
    chunks1$push(id = "tbl3", x = quote(apply(dataset, 2, function(x) x * y))),
    "push"
  )
  testthat::expect_equal(
    chunks1$eval(),
    as.matrix(data.frame(x = c(3, 6, 9, 12, 15, 18), y = c(3, 3, 3, 3, 3, 3))),
  )

  testthat::expect_equal(
    chunks1$get("y"),
    3
  )
})

# * get_rcode ====
testthat::test_that("chunks get_rcode", {
  testthat::expect_silent({
    rm(list = ls())
    chunks1 <- chunks$new()
  })
  testthat::expect_silent({
    dataset <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
    y <- 5
    chunks1$reset()
  })

  testthat::expect_equal(chunks1$get_rcode(), character())

  testthat::expect_silent(
    chunks1$push(id = "tbl", x = quote(apply(dataset, 2, function(x) x * y)))
  )

  testthat::expect_equal(
    unname(chunks1$.__enclos_env__$private$get_rcode_id("tbl")),
    "apply(dataset, 2, function(x) x * y)",
    info = "Single chunk code generation unfiltered failed inside chunks."
  )

  testthat::expect_equal(
    chunks1$get_rcode(),
    c("tbl" = "apply(dataset, 2, function(x) x * y)"),
    info = "Multi list single code generation filtered fails."
  )

  testthat::expect_silent(
    chunks1$push(
      id = "tbl1",
      x = substitute(
        apply(dataset, 2, function(x) x * y),
        list(y = 4)
      )
    )
  )
  testthat::expect_equal(
    chunks1$get_rcode(),
    c(
      "tbl" = "apply(dataset, 2, function(x) x * y)",
      "tbl1" = "apply(dataset, 2, function(x) x * 4)"
    ),
    info = "Multi list single code generation fails."
  )

  testthat::expect_equal(
    chunks1$get_rcode(chunk_ids = "tbl"),
    c("tbl" = "apply(dataset, 2, function(x) x * y)")
  )
})


testthat::test_that("chunks_eval", {
  testthat::expect_silent({
    rm(list = ls())
    dataset <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
    y <- 5
    chunks1 <- chunks$new(envir = environment())
  })
  testthat::expect_silent(
    chunks_push(
      id = "tbl",
      expression = quote(apply(dataset, 2, function(x) x * y)),
      chunks = chunks1
    )
  )
  testthat::expect_error(
    chunks_eval("tbl")
  )
  testthat::expect_equal(
    chunks_eval(chunks = chunks1),
    as.matrix(data.frame(x = c(5, 10, 15, 20, 25, 30), y = c(5, 5, 5, 5, 5, 5)))
  )
})
testthat::test_that("get_code_chunk", {
  testthat::expect_silent({
    rm(list = ls())
    dataset <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
    y <- 5
    chunks1 <- chunks$new(envir = environment())
  })
  testthat::expect_silent(
    chunks_push(
      id = "tbl",
      expression = quote(apply(dataset, 2, function(x) x * y)),
      chunks = chunks1
    )
  )
  testthat::expect_error(
    get_code_chunk("tbl")
  )
  testthat::expect_equal(
    unname(chunks_get_rcode(chunks = chunks1)),
    "apply(dataset, 2, function(x) x * y)"
  )
})

testthat::test_that("get_code_chunk curly braces", {
  testthat::expect_silent({
    rm(list = ls())
    dataset <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
    y <- 5
    chunks1 <- chunks$new(envir = environment())
  })
  testthat::expect_silent(
    chunks_push(
      id = "tbl",
      expression = bquote({
        a <- 2
        apply(dataset, 2, function(x) x * y)
      }),
      chunks = chunks1
    )
  )
  testthat::expect_equal(
    unname(chunks_get_rcode(chunks = chunks1)),
    "a <- 2\napply(dataset, 2, function(x) x * y)"
  )
})

testthat::test_that("init_chunks", {
  session <- new.env()
  session$userData <- new.env() # nolint
  session$ns <- function(x) {
    if (length(x) == 0) {
      "id"
    } else {
      paste0("id", x, sep = "-")
    }
  } # nolint
  init_chunks(session = session)
  testthat::expect_true(
    "chunks" %in% class(session$userData[[session$ns(character(0))]]$chunks)
  )
})

testthat::test_that("set chunk environment", {
  rm(list = ls())
  my_chunks <- chunks$new(envir = environment())
  testthat::expect_equal(
    ls(my_chunks$info()$envir),
    character(0)
  )

  rm(list = ls())
  a <- 3
  my_chunks <- chunks$new(envir = environment())
  testthat::expect_equal(
    ls(my_chunks$info()$envir),
    "a"
  )

  # chunk env in my_chunks
  rm(list = ls())
  a <- 3
  my_chunks <- chunks$new()
  testthat::expect_silent(
    chunks_reset(chunks = my_chunks)
  )
  testthat::expect_equal(
    ls(my_chunks$info()$envir),
    c("a", "my_chunks")
  )

  testthat::expect_equal(
    chunks_get_var(chunks = my_chunks, "a"),
    3
  )

  # chunk env in session
  testthat::expect_silent({
    rm(list = ls())
    session <- new.env()
    session$userData <- new.env() # nolint
    session$ns <- function(x) {
      if (length(x) == 0) {
        "id"
      } else {
        paste0("id", x, sep = "-")
      }
    } # nolint
    init_chunks(session = session)
  })

  a <- 4
  testthat::expect_silent({
    chunks_reset(chunks = session$userData[[session$ns(character(0))]]$chunks)
  })

  testthat::expect_equal(
    chunks_get_var(chunks = session$userData[[session$ns(character(0))]]$chunks, "a"),
    4
  )
})


testthat::test_that("deep clone method", {
  var1 <- 1
  var2 <- 2
  var3 <- new.env()

  var3$var1 <- 10
  var3$var2 <- 20
  var3$var3 <- new.env()

  var3$var3$var1 <- 100
  var3$var3$var2 <- 200
  var3$var3$var3 <- new.env()

  var3$var3$var3$var <- "test"

  chunks1 <- chunks$new(envir = environment())

  testthat::expect_silent({
    chunks2 <- chunks1$clone(deep = TRUE)
  })
  testthat::expect_false(identical(chunks1, chunks2))

  # all should be equal
  testthat::expect_equal(chunks1$get("var1"), chunks2$get("var1"))
  testthat::expect_equal(chunks1$get("var2"), chunks2$get("var2"))
  testthat::expect_equal(chunks1$get("var3"), chunks2$get("var3"))

  # values are identical (i.e. deep copy of 1 still points to the same memory address of 1)
  # envirs should not be identical
  testthat::expect_identical(chunks1$get("var1"), chunks2$get("var1"))
  testthat::expect_identical(chunks1$get("var2"), chunks2$get("var2"))
  # testthat::expect_identical(chunks1$get("var3"), chunks2$get("var3")) ---> should be FALSE: please see below

  # expect not identical
  chunks1_var3 <- chunks1$get("var3")
  chunks2_var3 <- chunks2$get("var3")
  testthat::expect_equal(chunks1_var3, chunks2_var3)
  testthat::expect_false(identical(chunks1_var3, chunks2_var3))

  # check nested envir
  chunks1_var3_var3 <- chunks1_var3$var3
  chunks2_var3_var3 <- chunks2_var3$var3
  testthat::expect_equal(chunks1_var3_var3, chunks2_var3_var3)
  testthat::expect_false(identical(chunks1_var3_var3, chunks2_var3_var3))

  # check double nested envir
  chunks1_var3_var3_var3 <- chunks1_var3$var3$var3
  chunks2_var3_var3_var3 <- chunks2_var3$var3$var3
  testthat::expect_equal(chunks1_var3_var3_var3, chunks2_var3_var3_var3)
  testthat::expect_false(identical(chunks1_var3_var3_var3, chunks2_var3_var3_var3))
})


testthat::test_that("lhs and rhs", {
  testthat::expect_silent({
    rm(list = ls())
    dataset <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
    y <- 5
    chunks1 <- chunks$new(envir = environment())
  })
  testthat::expect_silent(
    chunks_push(
      id = "one",
      expression = bquote(x <- y),
      chunks = chunks1
    )
  )
  testthat::expect_silent(
    chunks_push(
      id = "two",
      expression = bquote(x * y),
      chunks = chunks1
    )
  )
})



testthat::test_that("chunk pipe", {
  rm(list = ls())
  session <- new.env()
  session$userData <- new.env() # nolint
  session$ns <- function(x) {
    if (length(x) == 0) {
      "id"
    } else {
      paste0("id", x, sep = "-")
    }
  } # nolint
  init_chunks(session = session)
  form <- "from"
  testthat::expect_equal(
    ls(session$userData[[session$ns(character(0))]]$chunks$info()$envir),
    character(0)
  )
  chunks_reset(chunks = session$userData[[session$ns(character(0))]]$chunks)
  testthat::expect_silent({
    form_two %<chunk% paste(form, "to")
  })
  testthat::expect_equal(
    form_two$eval(chunks = session$userData[[session$ns(character(0))]]$chunks),
    c("from to")
  )
  testthat::expect_equal(
    unname(chunks_get_rcode(chunks = session$userData[[session$ns(character(0))]]$chunks)),
    "paste(form, \"to\")"
  )
})

# * push_chunks ====
testthat::test_that("chunks push_chunks", {
  testthat::expect_silent({
    chunks_object <- chunks$new()
    chunks_object$push(bquote(x <- 1))
  })

  testthat::expect_silent({
    chunks_object2 <- chunks$new()
    chunks_object2$push(bquote(y <- 1))
  })

  testthat::expect_error(
    chunks_object$push(chunks_object2)
  )

  testthat::expect_silent(
    chunks_object$push_chunks(chunks_object2)
  )

  testthat::expect_equal(
    c("x <- 1", "y <- 1"),
    unname(chunks_object$get_rcode())
  )

  testthat::expect_silent(
    chunks_object$eval()
  )

  testthat::expect_equal(
    chunks_object$get("x"),
    1
  )

  testthat::expect_equal(
    chunks_object$get("y"),
    1
  )

  # comments always flagged as evaluated
  stack <- chunks$new()
  stack$push("This is a comment")
  testthat::expect_warning(
    stack$eval(),
    "All chunks were already evaluated"
  )
  testthat::expect_match(
    stack$get_rcode(),
    "# This is a comment"
  )

  testthat::expect_silent({
    chunks_object <- chunks$new()
    chunks_object$push(bquote(x <- 1))
  })

  testthat::expect_silent({
    chunks_object2 <- chunks$new()
    chunks_object2$push(bquote(y <- 1))
  })

  testthat::expect_silent({
    chunks_push_chunks(x = chunks_object2, chunks = chunks_object)
    chunks_eval(chunks = chunks_object)
  })

  testthat::expect_equal(
    chunks_get_var("x", chunks_object),
    1
  )


  # * chunks1 evaluated ====
  testthat::expect_silent({
    chunks_object <- chunks$new()
    chunks_object$push(bquote(x <- 1))
    chunks_object$eval()
  })

  testthat::expect_warning({
    stack <- chunks$new()
    stack$eval()
  })

  testthat::expect_warning({
    stack <- chunks$new()
    stack$push(bquote(stop("Error")))
    stack$eval()
    stack$push(bquote(a <- "Will the pandemic ever end?"))
    stack$eval()
  })

  testthat::expect_silent({
    chunks_object2 <- chunks$new()
    chunks_object2$push(bquote(y <- 1))
  })

  testthat::expect_silent(
    chunks_object$push_chunks(chunks_object2)
  )

  testthat::expect_equal(
    chunks_object$get("x"),
    1
  )

  testthat::expect_equal(
    suppressWarnings(chunks_object$get("y")),
    NULL
  )

  testthat::expect_silent(
    chunks_object$eval()
  )

  testthat::expect_equal(
    chunks_object$get("y"),
    1
  )

  # * chunks2 evaluated, run uneval ====
  testthat::expect_silent({
    chunks_object <- chunks$new()
    chunks_object$push(bquote(x <- 1))
  })

  testthat::expect_silent({
    chunks_object2 <- chunks$new()
    chunks_object2$push(bquote(y <- 1))
    chunks_object2$eval()
  })

  testthat::expect_silent(
    chunks_object$push_chunks(chunks_object2)
  )

  testthat::expect_equal(
    chunks_object$info()$is_evaluated[2],
    FALSE
  )

  testthat::expect_equal(
    chunks_object$info()$latest_result,
    NULL
  )

  # * chunks2 evaluated, expect silent ====
  testthat::expect_silent({
    chunks_object$eval()
  })

  testthat::expect_silent(
    chunks_object$push_chunks(chunks_object2)
  )

  testthat::expect_equal(
    chunks_object$get("y"),
    1
  )
  testthat::expect_equal(
    chunks_object$get("x"),
    1
  )

  # * environment conflict ====
  testthat::expect_silent({
    chunks_object <- chunks$new()
    chunks_object$push(bquote(x <- 1))
    chunks_object$eval()
  })

  testthat::expect_silent({
    chunks_object2 <- chunks$new()
    chunks_object2$push(bquote(x <- 2))
    chunks_object2$eval()
  })

  testthat::expect_error(
    object = {
      chunks_object$push_chunks(chunks_object2)
    },
    "- x"
  )


  # * environment conflict overwrite ====
  testthat::expect_silent({
    chunks_object <- chunks$new()
    chunks_object$push(bquote(x <- 1))
    chunks_object$eval()
  })

  testthat::expect_silent({
    chunks_object2 <- chunks$new()
    chunks_object2$push(bquote(x <- 2))
    chunks_object2$eval()
  })

  testthat::expect_silent({
    chunks_object$push_chunks(chunks_object2, overwrite = TRUE)
  })

  testthat::expect_equal(
    chunks_object$get("x"),
    2
  )
})

# * chunks_uneval ====
testthat::test_that("chunks uneval", {
  testthat::expect_silent({
    chunks_object <- chunks$new()
    chunks_object$push(bquote(x <- 1))
    chunks_object$push(bquote(a <- 2))
    chunks_object$push(bquote(res <- a + x))
    chunks_object$eval()
  })

  testthat::expect_silent({
    chunks_object$uneval()
  })

  testthat::expect_equal(
    suppressWarnings(chunks_object$get("x")),
    NULL
  )

  testthat::expect_silent(chunks_object$eval())
  testthat::expect_equal(
    chunks_object$get("res"),
    3
  )

  # * uneval from env ====
  testthat::expect_silent({
    a <- 2
    chunks_object <- chunks$new()
    chunks_object$reset()
    chunks_object$push(bquote(res <- a + 1))
    chunks_object$eval()
  })
  testthat::expect_equal(
    chunks_object$get("res"),
    3
  )

  testthat::expect_silent({
    a <- 1
    chunks_object$uneval()
  })

  testthat::expect_equal(
    chunks_object$.__enclos_env__$private$envir$a,
    2
  )

  testthat::expect_silent(chunks_object$eval())

  testthat::expect_equal(
    chunks_object$get("res"),
    3
  )

  chunks_uneval(chunks = chunks_object)

  testthat::expect_silent(chunks_object$eval())

  testthat::expect_equal(
    chunks_object$get("res"),
    3
  )

  # * uneval from overwrite env ====
  testthat::expect_silent({
    a <- 2
    chunks_object <- chunks$new()
    chunks_object$reset()
    chunks_object$push(bquote(res <- a + 1))
    chunks_object$eval()
  })

  testthat::expect_silent({
    a <- 1
    chunks_object$uneval(overwrite = TRUE)
  })

  testthat::expect_equal(
    chunks_object$.__enclos_env__$private$any_remaining(),
    TRUE
  )

  testthat::expect_silent(chunks_object$eval())

  testthat::expect_equal(
    chunks_object$get("res"),
    2
  )
})

# * chunks_push_merge_data ====
testthat::test_that("chunks chunks_push_merge_data", {
  testthat::expect_silent({
    chunks_object <- chunks$new()
    chunks_object$push(bquote(x <- 1))
    chunks_object$push(bquote(a <- 2))
    chunks_object$push(bquote(res <- a + x))
    chunks_object$eval()
  })

  testthat::expect_silent({
    chunks_object2 <- chunks$new()
    chunks_object2$push(bquote(y <- 1))
  })

  testthat::expect_silent({
    chunks_push_data_merge(
      x = list(chunks = chunks_object2),
      chunks = chunks_object
    )
  })

  testthat::expect_equal(
    unname(chunks_object$get_rcode()),
    c("x <- 1", "a <- 2", "res <- a + x", "y <- 1")
  )
})

# * get_warnings, get_errors, get_messages ====
testthat::test_that("chunks warning messages", {
  stack <- chunks$new()

  # Empty chunks
  testthat::expect_equal(
    stack$get_warnings(),
    character(0)
  )

  # Warnings
  stack$push(bquote(
    x <- "Test"
  ))
  stack$push(bquote(
    warning("First warning")
  ))
  stack$push(bquote(
    warning("Second warning")
  ))
  stack$eval()
  testthat::expect_equal(
    stack$get_warnings(),
    list(
      chunk_1 = character(0),
      chunk_2 = "First warning",
      chunk_3 = "Second warning"
    )
  )

  testthat::expect_equal(
    stack$get_warnings("chunk_2"),
    list(chunk_2 = "First warning")
  )

  testthat::expect_equal(
    stack$get_warnings("chunk_1"),
    list(chunk_1 = character(0))
  )

  testthat::expect_equal(
    chunks_warnings(chunks = stack),
    list(
      chunk_1 = character(0),
      chunk_2 = "First warning",
      chunk_3 = "Second warning"
    )
  )

  testthat::expect_equal(
    stack$get_warnings(),
    chunks_warnings(chunks = stack)
  )

  # Messages
  stack <- chunks$new()
  testthat::expect_equal(stack$get_messages(), character(0))
  stack$push(bquote({
    message("First message")
  }))
  stack$push(bquote({
    message("Second message")
  }))
  testthat::expect_equal(
    stack$get_messages(),
    list(
      chunk_1 = character(0),
      chunk_2 = character(0)
    )
  )
  stack$eval()
  testthat::expect_equal(
    stack$get_messages(),
    list(
      chunk_1 = "First message",
      chunk_2 = "Second message"
    )
  )

  testthat::expect_equal(
    chunks_messages(chunks = stack),
    list(
      chunk_1 = "First message",
      chunk_2 = "Second message"
    )
  )

  testthat::expect_equal(
    stack$get_messages(),
    chunks_messages(chunks = stack)
  )
})

# * eval_info() ====
testthat::test_that("chunks$eval_info() testing", {
  stacks <- chunks$new()
  stacks$push("This is a comment")
  testthat::expect_warning(stacks$eval(), "All chunks were already evaluated")

  # Default behaviour - comments not shown
  testthat::expect_equal(length(stacks$eval_info()), 0)

  # Show eval_info by id
  stacks$push(id = "test_chunk", x = bquote(x <- "test"))
  stacks$eval()
  testthat::expect_equal(length(stacks$eval_info()), 1)
  testthat::expect_equal(length(stacks$eval_info("test_chunk")), 1)
})

# * validate_ methods ====
testthat::test_that("chunks$validate_is", {
  stacks <- chunks$new()
  testthat::expect_error(stacks$validate_is(8))
  testthat::expect_error(stacks$validate_is(c("test1", "test2")))
  testthat::expect_error(stacks$validate_is(var = "test", class = 9))
  testthat::expect_error(stacks$validate_is(var = "test", class = c("t1", "t2")))
  testthat::expect_error(stacks$validate_is(var = "test", class = "character", msg = 9))
  testthat::expect_error(stacks$validate_is(var = "test", class = "character", msg = c("t1", "t2")))
  testthat::expect_warning(
    testthat::expect_error(stacks$validate_is(var = "test", class = "character", msg = "Test message"))
  )

  stacks$push(x = bquote({
    test <- "this is a test character"
  }))
  stacks$eval()

  testthat::expect_silent(stacks$validate_is(var = "test", class = "character", msg = "Test message"))
  testthat::expect_silent(stacks$validate_is(var = "test", class = "character"))
  testthat::expect_error(
    stacks$validate_is(var = "test", class = "numeric"),
    regexp = "Variable 'test' is of class character instead of numeric.",
    class = "shiny.silent.error"
  )
  testthat::expect_error(
    stacks$validate_is(var = "test", class = "numeric", msg = "Test message"),
    regexp = "Test message",
    class = "shiny.silent.error"
  )
})

testthat::test_that("chunks$validate_all", {
  stacks <- chunks$new()
  testthat::expect_warning(testthat::expect_error(stacks$validate_all(var = "test", class = "character")))

  stacks$push(x = bquote({
    test <- "this is a test character"
  }))
  stacks$eval()
  testthat::expect_silent(stacks$validate_all(var = "test", class = "character"))

  stacks$push(x = bquote({
    x <- ("How was you morning?")
  }))
  testthat::expect_error(stacks$validate_all(var = "test", class = "character"))

  stacks$push(x = bquote({
    stop("This is an error")
  }))
  stacks$eval()
  testthat::expect_error(stacks$validate_all(var = "test", class = "character"))
})

# * reactive_summary ====
testthat::test_that("chunks$get_reactive_summary", {
  stacks <- chunks$new()

  stacks$push(quote(message("message")))
  stacks$eval()
  testthat::expect_equal(
    isolate(stacks$get_reactive_summary()$errors),
    c(FALSE)
  )
  testthat::expect_equal(
    isolate(stacks$get_reactive_summary()$warnings),
    c(FALSE)
  )
  testthat::expect_equal(
    isolate(stacks$get_reactive_summary()$msgs),
    c(TRUE)
  )

  stacks$push(quote(warning("warning")))
  stacks$eval()
  testthat::expect_equal(
    isolate(stacks$get_reactive_summary()$errors),
    c(FALSE, FALSE)
  )
  testthat::expect_equal(
    isolate(stacks$get_reactive_summary()$warnings),
    c(FALSE, TRUE)
  )
  testthat::expect_equal(
    isolate(stacks$get_reactive_summary()$msgs),
    c(TRUE, FALSE)
  )

  stacks$push(quote(stop("error")))
  stacks$eval()
  testthat::expect_equal(
    isolate(stacks$get_reactive_summary()$errors),
    c(FALSE, FALSE, TRUE)
  )
  testthat::expect_equal(
    isolate(stacks$get_reactive_summary()$warnings),
    c(FALSE, TRUE, FALSE)
  )
  testthat::expect_equal(
    isolate(stacks$get_reactive_summary()$msgs),
    c(TRUE, FALSE, FALSE)
  )

  stacks$reset()
  testthat::expect_equal(
    isolate(stacks$get_reactive_summary()$errors),
    logical(0)
  )
  testthat::expect_equal(
    isolate(stacks$get_reactive_summary()$warnings),
    logical(0)
  )
  testthat::expect_equal(
    isolate(stacks$get_reactive_summary()$msgs),
    logical(0)
  )
})

# Helper function ====
# * chunks_push ====
testthat::test_that("chunks_push", {
  testthat::expect_warning({
    rm(list = "session")
    rm(list = ls())
    chunks1 <- chunks$new()
  })
  testthat::expect_silent({
    dataset <- data.frame(x = c(1, 2, 3, 4, 5, 6), y = c(1, 1, 1, 1, 1, 1))
    y <- 4
    chunks1$reset()
  })
  testthat::expect_error({
    chunks_push(
      id = "tbl",
      expression = quote(apply(dataset, 2, function(x) x * y))
    )
  })

  testthat::expect_silent(
    chunks_push(
      id = "tbl",
      expression = quote(apply(dataset, 2, function(x) x * y)),
      chunks = chunks1
    )
  )

  # name
  testthat::expect_silent(
    chunks_push(
      expression = as.name("y"),
      chunks = chunks1
    )
  )

  # expression
  testthat::expect_silent(
    chunks_push(
      expression = substitute(x <- 2),
      chunks = chunks1
    )
  )

  testthat::expect_silent(
    chunks_push(
      expression = substitute({
        x1 <- 2
        x2 <- 2
      }), # nolint
      chunks = chunks1
    )
  )

  testthat::expect_error(chunks_push(
    expression = quote(x <- 2),
    chunks = "test_class"
  ), regexp = "Provided chunks are not of class chunks.")
})

# * chunks_push_chunks ====
testthat::test_that("chunks_push_chunks", {
  stacks <- chunks$new()
  testthat::expect_error(
    chunks_push_chunks(x = stacks, chunks = "test_class"),
    regexp = "Provided chunks are not of class chunks."
  )
})

# * chunks_push_data_merge ====
testthat::test_that("chunks_push_data_merge", {
  testthat::expect_error(
    object = {
      names <- function(x) {
        return(c("chunks"))
      }
      chunks_push_data_merge("test", chunks = "wrong class")
    },
    regexp = "Provided chunks are not of class chunks."
  )
})

# * chunks_push_comment ====
testthat::test_that("chunks_push_comment", {
  stacks <- chunks$new()
  testthat::expect_error(chunks_push_comment(comment = c("test1", "test2"), chunks = stacks))
  testthat::expect_error(
    chunks_push_comment(comment = "Test", chunks = "test_class"),
    regexp = "Provided chunks are not of class chunks."
  )
  testthat::expect_equal(
    chunks_push_comment(comment = "Test comment", chunks = stacks),
    NULL
  )

  testthat::expect_equal(
    stacks$get_rcode(),
    c("chunk_1" = "# Test comment")
  )
})

# * chunks_push_new_line ====
testthat::test_that("chunks_push_new_line", {
  stacks <- chunks$new()
  testthat::expect_error(
    chunks_push_new_line(chunks = "test_class"),
    regexp = "Provided chunks are not of class chunks."
  )

  testthat::expect_silent(chunks_push_new_line(chunks = stacks))
  testthat::expect_equal(
    stacks$get_rcode(),
    c("chunk_1" = " ")
  )
})

# * chunks_eval ====
testthat::test_that("chunks_eval", {
  testthat::expect_error(chunks_eval(chunks = "wrong class"), regexp = "Provided chunks are not of class chunks.")
})

# * chunks_uneval ====
testthat::test_that("chunks_uneval", {
  testthat::expect_error(chunks_uneval(chunks = "wrong class"), regexp = "Provided chunks are not of class chunks.")
})

# * chunks_get_rcode ====
testthat::test_that("chunks_get_rcode", {
  testthat::expect_error(chunks_get_rcode(chunks = "wrong class"),
    regexp = "Provided chunks are not of class chunks."
  )
})

# * chunks_warnings ====
testthat::test_that("chunks_warnings", {
  testthat::expect_error(chunks_warnings(chunks = "wrong class"),
    regexp = "Provided chunks are not of class chunks."
  )
})

# * chunks_messages ====
testthat::test_that("chunks_messages", {
  testthat::expect_error(chunks_messages(chunks = "wrong class"),
    regexp = "Provided chunks are not of class chunks."
  )
})

# * overwrite_chunks ====
testthat::test_that("overwrite_chunks", {
  session <- new.env()
  session$userData <- new.env() # nolint
  session$ns <- function(x) {
    if (length(x) == 0) {
      "id"
    } else {
      paste0("id", x, sep = "-")
    }
  } # nolint

  a <- 2
  test_chunks <- chunks$new()

  testthat::expect_error(overwrite_chunks(x = test_chunks))

  init_chunks(session = session)

  overwrite_chunks(x = test_chunks, session = session)
  isolate({
    testthat::expect_equal(
      session$userData[[session$ns(character(0))]]$chunks,
      test_chunks
    )
  })

  testthat::expect_error(
    overwrite_chunks(x = NULL, session = session),
    regexp = "No chunks object provided for 'overwrite_chunks' in argument 'x'."
  )

  session$userData$id$chunks <- NULL
  testthat::expect_error(
    overwrite_chunks(x = test_chunks, session = session),
    regexp = "You cannot overwrite chunks without initializing them by 'init_chunks\\(\\)'"
  )
})

# * chunks_is_ok ====
testthat::test_that("chunks_is_ok", {
  testthat::expect_silent({
    stacks <- chunks$new()
    stacks$push(bquote({
      x <- "test"
    }))
    stacks$eval()
  })
  testthat::expect_equal(chunks_is_ok(chunks = stacks), stacks$is_ok())

  testthat::expect_error(chunks_is_ok(chunks = "Wrong class"),
    regexp = "Provided chunks are not of class chunks."
  )
})

# * chunks_get_eval_msg ====
testthat::test_that("chunks_get_eval_msg", {
  stacks <- chunks$new()
  stacks$push(bquote({
    stop("Test error")
  }))
  stacks$eval()
  testthat::expect_equal(chunks_get_eval_msg(chunks = stacks), stacks$get_eval_msg())

  testthat::expect_error(chunks_get_eval_msg(chunks = "Wrong class"),
    regexp = "Provided chunks are not of class chunks."
  )
})

# * chunks_validate_ methods ====
testthat::test_that("chunks_validate_ methods", {
  stacks <- chunks$new()

  stacks$push(x = bquote({
    test <- "this is a test character"
  }))
  stacks$eval()

  testthat::expect_silent(chunks_validate_is_ok(chunks = stacks))

  stacks$push(x = bquote({
    stop("Test error")
  }))
  stacks$eval()

  testthat::expect_error(
    chunks_validate_is_ok(chunks = stacks),
    regexp = "Test error",
    class = "shiny.silent.error"
  )
  testthat::expect_error(
    chunks_validate_is_ok(chunks = stacks, msg = "Custom error message"),
    regexp = "Custom error message",
    class = "shiny.silent.error"
  )
})

testthat::test_that("chunks_validate_is", {
  stacks <- chunks$new()
  stacks$push(x = bquote({
    test <- "this is a test character"
  }))
  stacks$eval()

  testthat::expect_error(chunks_validate_is(var = "test", class = "character", chunks = "test_class"))
  testthat::expect_silent(chunks_validate_is(var = "test", class = "character", chunks = stacks))
  testthat::expect_error(chunks_validate_is(var = "test", class = "numeric", chunks = stacks))
})

testthat::test_that("chunks_validate_all", {
  stacks <- chunks$new()
  testthat::expect_error(chunks_validate_all(var = "test", class = "character"))
  testthat::expect_warning(
    testthat::expect_error(chunks_validate_all(var = "test", class = "character", chunks = stacks))
  )

  stacks$push(x = bquote({
    test <- "this is a test character"
  }))
  stacks$eval()
  testthat::expect_silent(chunks_validate_all(var = "test", class = "character", chunks = stacks))

  stacks$push(x = bquote({
    x <- ("How was you morning?")
  }))
  testthat::expect_error(chunks_validate_all(var = "test", class = "character", chunks = stacks))

  stacks$push(x = bquote({
    stop("This is an error")
  }))
  stacks$eval()
  testthat::expect_error(chunks_validate_all(var = "test", class = "character", chunks = stacks))
})


testthat::test_that("chunks_deep_clone", {

  # check validation
  testthat::expect_error(chunks_deep_clone(list()), "Assertion on 'chunks' failed")
  # note chunk not chunks here
  testthat::expect_error(chunks_deep_clone(chunk$new(expression(y <- 1))), "Assertion on 'chunks' failed")

  x_chunk <- chunks$new()
  chunks_push(chunks = x_chunk, expression = expression(y <- 1))

  # A copy of x_chunk which does not share the same environment
  x_chunk_copy <- chunks_deep_clone(x_chunk)

  # Add expression only into x_chunk
  chunks_push(chunks = x_chunk, expression = expression(y <- 2 * y))

  # Evaluate x_chunk
  testthat::expect_equal(chunks_safe_eval(x_chunk), 2)
  # Evaluate x_chunk_copy
  testthat::expect_equal(chunks_safe_eval(x_chunk_copy), 1)
})
