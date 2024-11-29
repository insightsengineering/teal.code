pasten <<- function(...) paste(..., collapse = "\n")

testthat::test_that("get_code returns code (character(1) by default) of qenv object", {
  q <- qenv()
  q <- eval_code(q, quote(x <- 1))
  q <- eval_code(q, quote(y <- x))
  testthat::expect_equal(get_code(q), pasten(c("x <- 1", "y <- x")))
})

testthat::test_that("get_code handles code elements being code-blocks", {
  q <- qenv()
  q <- eval_code(q, quote(x <- 1))
  q <- eval_code(
    q,
    quote({
      y <- x
      z <- 5
    })
  )
  testthat::expect_equal(get_code(q), pasten(c("x <- 1", "y <- x", "z <- 5")))
})

testthat::test_that("get_code returns expression of qenv object if deparse = FALSE", {
  q <- qenv()
  q <- eval_code(q, quote(x <- 1))
  q <- eval_code(q, quote(y <- x))
  testthat::expect_equivalent(
    toString(get_code(q, deparse = FALSE)),
    "{\n    x <- 1\n    y <- x\n}"
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

testthat::test_that("get_code formatted returns code asis but replaces `;` with `\n`", {
  code <- "
    # header comment after white space

    a <- 1L; b <- 2 #inline comment


    c <- 3
    # closing comment
    "
  q <- eval_code(qenv(), code)
  testthat::expect_equal(get_code(q), gsub(";", "\n", code))
})

# names parameter -------------------------------------------------------------------------------------------------
testthat::describe("get_code for specific names", {
  testthat::it("warns if empty @code slot", {
    testthat::expect_warning(
      testthat::expect_identical(
        get_code(qenv(), names = "a"),
        ""
      ),
      "not found in code"
    )
  })

  testthat::it("handles the code without symbols on rhs", {
    code <- c(
      "1 + 1",
      "a <- 5",
      "501"
    )

    testthat::expect_identical(
      get_code(eval_code(qenv(), code), names = "a"),
      "a <- 5"
    )
  })

  testthat::it("handles the code included in curly brackets", {
    code <- "{1 + 1;a <- 5}"

    testthat::skip("SHOULD THIS BE FIXED? it gives the whole code {1 + 1;a <- 5}")
    testthat::expect_identical(
      get_code(eval_code(qenv(), code), names = "a"),
      "a <- 5"
    )
  })

  testthat::it("handles the code of length > 1 when at least one is enclosed in curly brackets", {
    code <- c("{a<-5}", "1+1")
    q <- eval_code(eval_code(qenv(), code[1]), code[2])

    testthat::expect_identical(
      get_code(q, names = "a"),
      "{a<-5}"
    )
  })

  testthat::it("extracts the code of a binding from character vector containing simple code", {
    code <- c(
      "a <- 1",
      "b <- 2"
    )
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "a"),
      "a <- 1"
    )
    testthat::expect_identical(
      get_code(q, names = "b"),
      "b <- 2"
    )
  })

  testthat::it("extracts the code without downstream usage", {
    code <- c(
      "a <- 1",
      "head(a)"
    )
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "a"),
      "a <- 1"
    )
  })

  testthat::it("works for names of length > 1", {
    code <- c(
      "a <- 1",
      "b <- 2"
    )
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = c("a", "b")),
      pasten(code)
    )
  })

  testthat::it("warns if binding doesn't exist in code", {
    code <- c("a <- 1")
    q <- eval_code(qenv(), code)
    testthat::expect_warning(
      get_code(q, names = "c"),
      "Object\\(s\\) not found in code: c"
    )
  })

  testthat::it("does not fall into a loop", {
    code <- c(
      "a <- 1",
      "b <- a",
      "c <- b",
      "a <- c"
    )
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "a"),
      pasten(code)
    )
    testthat::expect_identical(
      get_code(q, names = "b"),
      pasten(code[1:2])
    )
    testthat::expect_identical(
      get_code(q, names = "c"),
      pasten(code[1:3])
    )
  })

  testthat::it("extracts code of a parent binding but only those evaluated before coocurence", {
    code <- c(
      "a <- 1",
      "b <- a",
      "a <- 2"
    )
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "b"),
      pasten(c("a <- 1", "b <- a"))
    )
  })

  testthat::it("extracts the code of a parent binding if used as an arg in a function call", {
    code <- c(
      "a <- 1",
      "b <- identity(x = a)",
      "a <- 2"
    )
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "b"),
      pasten(c("a <- 1", "b <- identity(x = a)"))
    )
  })

  testthat::it("extracts the code when using <<-", {
    code <- c(
      "a <- 1",
      "b <- a",
      "b <<- b + 2"
    )
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "b"),
      pasten(c("a <- 1", "b <- a", "b <<- b + 2"))
    )
  })

  testthat::it("detects every assign calls even if not evaluated, if there is only one assignment in this line", {
    code <- c(
      "a <- 1",
      "b <- 2",
      "eval(expression({b <- b + 2}))"
    )
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "b"),
      pasten(code[2:3])
    )
  })

  testthat::it("returns result of length 1 for non-empty input and deparse = FALSE", {
    q1 <- qenv()
    q1 <- within(q1, {
      a <- 1
      b <- a^5
      c <- list(x = 2)
    })

    testthat::expect_length(get_code(q1, deparse = FALSE), 1)
  })

  testthat::it("detects calls associated with object if calls are separated by ;", {
    code <- c("a <- 1;b <- 2;a <- a + 1")
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "a"),
      "a <- 1\na <- a + 1"
    )
  })

  testthat::it("does not break if code uses quote()", {
    code <- c(
      "expr <- quote(x <- x + 1)",
      "x <- 0",
      "eval(expr)"
    )
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "x"),
      code[2]
    )
  })

  testthat::it("does not break if object is used in a function on lhs", {
    code <- c(
      "data(iris)",
      "iris2 <- iris",
      "names(iris) <- letters[1:5]"
    )
    q <- eval_code(qenv(), code = code)
    testthat::expect_identical(
      get_code(q, names = "iris"),
      pasten(code[c(1, 3)])
    )
  })

  testthat::it(
    "does not break if object is used in a function on lhs and influencers are both on lhs and rhs",
    {
      code <- c(
        "x <- 5",
        "y <- length(x)",
        "names(x)[y] <- y"
      )
      q <- eval_code(qenv(), code = code)
      testthat::expect_identical(
        get_code(q, names = "x"),
        pasten(code)
      )
    }
  )
})


# assign ----------------------------------------------------------------------------------------------------------

testthat::test_that("extracts the code for assign() where \"x\" is a literal string", {
  code <- c(
    "a <- 1",
    "assign('b', 5)",
    "assign(value = 7, x = 'c')",
    "assign(value = 15, x = \"d\")",
    "b <- b + 2",
    "c <- b",
    "d <- d * 2"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    pasten(code[c(2, 5)])
  )
  testthat::expect_identical(
    get_code(q, names = "c"),
    pasten(code[c(2, 3, 5, 6)])
  )
  testthat::expect_identical(
    get_code(q, names = "d"),
    pasten(c("assign(value = 15, x = \"d\")", "d <- d * 2"))
  )
})

testthat::test_that("extracts the code for assign() where \"x\" is variable", {
  testthat::skip("We will not resolve this, as this requires code evaluation.")
  code <- c(
    "x <- \"a\"",
    "assign(x, 5)",
    "b <- a"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    pasten(code)
  )
})

testthat::test_that("works for assign() detection no matter how many parametrers were provided in assignq()", {
  code <- c(
    "x <- 1",
    "assign(\"x\", 0, envir = environment())",
    "assign(inherits = FALSE, immediate = TRUE, \"z\", 5, envir = environment())",
    "y <- x + z",
    "y <- x"
  )

  q <- eval_code(qenv(), code)

  testthat::expect_identical(
    get_code(q, names = "y"),
    pasten(code)
  )
})

testthat::test_that("detects function usage of the assignment operator", {
  code <- c(
    "x <- 1",
    "`<-`(y,x)"
  )
  code2 <- "`<-`(y, `<-`(x, 2))"

  q <- eval_code(qenv(), code)
  q2 <- eval_code(qenv(), code2)

  testthat::expect_identical(
    get_code(q, names = "y"),
    pasten(code)
  )
  testthat::expect_identical(
    get_code(q2, names = "y"),
    pasten(code2)
  )
})


# @linksto ---------------------------------------------------------------------------------------------------------

testthat::test_that("get_code does not break if @linksto is put in the last line", {
  # In some cases R parses comment as a separate expression so the comment is not
  # directly associated with this line of code. This situation occurs when `eval` is in the last
  # line of the code. Other cases are not known but are highly probable.
  code <- c(
    "expr <- quote(x <- x + 1)",
    "x <- 0",
    "eval(expr) #@linksto x"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "x"),
    pasten(code)
  )
})

testthat::test_that("@linksto makes a line being returned for an affected binding", {
  code <-
    "a <- 1 # @linksto b
  b <- 2"
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    pasten(c("a <- 1 # @linksto b", "  b <- 2"))
  )
})

testthat::test_that(
  "@linksto returns the line for an affected binding
  even if the object did not exist in the same iteration of eval_code",
  {
    code <- c(
      "a <- 1 # @linksto b",
      "b <- 2"
    )
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "b"),
      pasten(code)
    )
  }
)

testthat::test_that(
  "lines affecting parent evaluated after co-occurrence are not included in output when using @linksto",
  {
    code <- c(
      "a <- 1",
      "b <- 2 # @linksto a",
      "a <- a + 1",
      "b <- b + 1"
    )
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "a"),
      pasten(code[1:3])
    )
    testthat::expect_identical(
      get_code(q, names = "b"),
      pasten(code[c(2, 4)])
    )
  }
)

testthat::test_that(
  "@linksto gets extracted if it's a side-effect on a dependent object (even of a dependent object)",
  {
    code <- c(
      "iris[1:5, ] -> iris2",
      "iris_head <- head(iris) # @linksto iris3",
      "iris3 <- iris_head[1, ] # @linksto iris2",
      "classes <- lapply(iris2, class)"
    )
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "classes"),
      pasten(code)
    )
  }
)

# functions -------------------------------------------------------------------------------------------------------

testthat::test_that("ignores occurrence in a function definition", {
  code <- c(
    "b <- 2",
    "foo <- function(b) { b <- b + 2 }"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    code[1]
  )
  testthat::expect_identical(
    get_code(q, names = "foo"),
    code[2]
  )
})

testthat::test_that("ignores occurrence in a function definition that has function in it", {
  code <- c(
    "b <- 2",
    "foo <- function(b) { function(c) {b <- c + 2 }}"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    code[1]
  )
  testthat::expect_identical(
    get_code(q, names = "foo"),
    code[2]
  )
})

testthat::test_that("ignores occurrence in a function definition if there is multiple function definitions", {
  code <- c(
    "b <- 2",
    "foo <- function(b) { function(c) {b <- c + 2 }}",
    "b <- b + 1",
    "bar <- function(b) print(b)"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    pasten(code[c(1, 3)])
  )
  testthat::expect_identical(
    get_code(q, names = "foo"),
    code[2]
  )
})

testthat::test_that("ignores occurrence in a function definition in lapply", {
  code <- c(
    "a <- list(a = 1, b = 2, c = 3)",
    "b <- lapply(a, FUN = function(x) { x <- x + 1 })",
    "b <- Filter(function(x) x > 2, b)",
    "x <- 1",
    "identity(x)"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "x"),
    "x <- 1"
  )
})

testthat::test_that("does not ignore occurrence in function body if object exsits in env", {
  skip("This is not urgent and can be ommitted with @linksto tag.")
  code <- c(
    "a <- list(a = 1, b = 2, c = 3)",
    "p <- 5", # This is not extracted, even though is used in the next line.
    "b <- lapply(a, FUN = function(x) { x <- x + p })",
    "b <- Filter(function(x) x > 2, b)"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    code
  )
})

testthat::test_that("ignores occurrence in function definition without { curly brackets", {
  code <- c(
    "b <- 2",
    "foo <- function(b) b <- b + 2"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "foo"),
    code[2]
  )
  testthat::expect_identical(
    get_code(q, names = "b"),
    code[1]
  )
})

testthat::test_that("detects occurrence of the function object", {
  code <- c(
    "a <- 1",
    "b <- 2",
    "foo <- function(b) { b <- b + 2 }",
    "b <- foo(a)"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    pasten(code)
  )
})

testthat::test_that("detects occurrence of a function definition when a formal is named the same as a function", {
  code <- c(
    "x <- 1",
    "foo <- function(foo = 1) 'text'",
    "a <- foo(x)"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "a"),
    pasten(code)
  )
})

testthat::test_that("detects occurrence of a function definition with a @linksto usage", {
  code <- c(
    "
        foo <- function() {
          env <- parent.frame()
          env$x <- 0
        }",
    "foo() # @linksto x",
    "y <- x"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "x"),
    pasten(code[1:2])
  )
})


# for loop --------------------------------------------------------------------------------------------------------

testthat::test_that("objects in for loop are extracted if passed as one character", {
  code <- "
    some_other_dataset <- mtcars
    original_dataset <- iris[, 1:4]
    count <- 1
    for (x in colnames(original_dataset)) {
      original_dataset[, x] <- original_dataset[, x] * 2
      count <- count + 1
    }
    output <- rlang::list2(x = original_dataset)
  "
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "output"),
    gsub("\n    some_other_dataset <- mtcars\n", "", code, fixed = TRUE)
  )
})

testthat::test_that("objects in for loop are extracted if passed as separate calls", {
  q <- within(qenv(), {
    a <- 1
    b <- 2
  }) |> within({
    for (x in c(1, 2)) {
      b <- a
      b <- b + a + 1
      b + 3 -> b # nolint: assignment.
    }
  })

  testthat::expect_setequal(
    strsplit(get_code(q, names = "b"), "\n")[[1]],
    c(
      "a <- 1",
      "b <- 2",
      "for (x in c(1, 2)) {",
      "    b <- a",
      "    b <- b + a + 1",
      "    b <- b + 3", # ORDER IS CHANGED IN HERE, but we can live with it
      "}"
    )
  )
})

# $ ---------------------------------------------------------------------------------------------------------------

testthat::test_that("understands $ usage and do not treat rhs of $ as objects (only lhs)", {
  code <- c(
    "x <- data.frame(a = 1:3)",
    "a <- data.frame(y = 1:3)",
    "a$x <- a$y",
    "a$x <- a$x + 2",
    "a$x <- x$a"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "x"),
    code[1]
  )
  testthat::expect_identical(
    get_code(q, names = "a"),
    pasten(code)
  )
})

testthat::test_that("detects cooccurrence properly even if all objects are on lhs", {
  code <- c(
    "a <- 1",
    "b <- list(c = 2)",
    "b[[a]] <- 3"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    pasten(code)
  )
})


# @ ---------------------------------------------------------------------------------------------------------------

testthat::test_that("understands @ usage and do not treat rhs of @ as objects (only lhs)", {
  code <- c(
    "setClass('aclass', slots = c(a = 'numeric', x = 'numeric', y = 'numeric')) # @linksto a x",
    "x <- new('aclass', a = 1:3, x = 1:3, y = 1:3)",
    "a <- new('aclass', a = 1:3, x = 1:3, y = 1:3)",
    "a@x <- a@y",
    "a@x <- a@x + 2",
    "a@x <- x@a"
  )
  q <- qenv()
  code_split <- as.list(split_code(paste(code, collapse = "\n")))

  dependency <-
    lapply(
      code_split,
      function(current_code) {
        parsed_code <- parse(text = current_code, keep.source = TRUE)
        extract_dependency(parsed_code)
      }
    )

  for (i in seq_along(code_split)) {
    attr(code_split[[i]], "dependency") <- dependency[[i]]
  }

  q@code <- code_split
  testthat::expect_identical(
    get_code(q, names = "x"),
    pasten(code[1:2])
  )
  testthat::expect_identical(
    get_code(q, names = "a"),
    pasten(code)
  )
})



# libraries -------------------------------------------------------------------------------------------------------

testthat::test_that("library() and require() are always returned", {
  code <- c(
    "set.seed(1)",
    "require(dplyr)",
    "library(lifecycle)",
    "x <- 5",
    "y <- 6"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "x"),
    pasten(code[c(2, 3, 4)])
  )
})


# data() ----------------------------------------------------------------------------------------------------------

testthat::test_that("data() call is returned when data name is provided as is", {
  code <- c(
    "set.seed(1)",
    "require(dplyr)",
    "library(lifecycle)",
    "data(iris, envir = environment())",
    "x <- iris"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "x"),
    pasten(code[-1])
  )
})

testthat::test_that("data() call is returned when data name is provided as a character", {
  code <- c(
    "set.seed(1)",
    "require(dplyr)",
    "library(lifecycle)",
    "data('mtcars')",
    "z <- mtcars"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "z"),
    pasten(code[-1])
  )
})


testthat::describe("Backticked symbol", {
  testthat::it("code can be retrieved with get_code", {
    td <- within(
      qenv(),
      {
        `%cbind%` <- function(lhs, rhs) cbind(lhs, rhs) # nolint: object_name.
        iris_ds <- iris %cbind% data.frame(new_col = "new column")
      }
    )

    testthat::expect_identical(
      get_code(td, names = "%cbind%"),
      "`%cbind%` <- function(lhs, rhs) cbind(lhs, rhs)"
    )
  })

  testthat::it("code can be retrieved with get_code", {
    td <- within(
      qenv(),
      {
        `%cbind%` <- function(lhs, rhs) cbind(lhs, rhs) # nolint: object_name.
        iris_ds <- iris %cbind% data.frame(new_col = "new column")
      }
    )

    testthat::expect_identical(
      get_code(td, names = "`%cbind%`"),
      "`%cbind%` <- function(lhs, rhs) cbind(lhs, rhs)"
    )
  })

  testthat::it("starting with underscore is detected in code dependency", {
    td <- within(
      qenv(),
      {
        `_add_column_` <- function(lhs, rhs) cbind(lhs, rhs) # nolint: object_name.
        iris_ds <- `_add_column_`(iris, data.frame(new_col = "new column"))
      }
    )

    testthat::expect_identical(
      get_code(td, names = "iris_ds"),
      paste(
        c(
          "`_add_column_` <- function(lhs, rhs) cbind(lhs, rhs)",
          "iris_ds <- `_add_column_`(iris, data.frame(new_col = \"new column\"))"
        ),
        collapse = "\n"
      )
    )
  })

  testthat::it("with space character is detected in code dependency", {
    td <- within(
      qenv(),
      {
        `add column` <- function(lhs, rhs) cbind(lhs, rhs) # nolint: object_name.
        iris_ds <- `add column`(iris, data.frame(new_col = "new column"))
      }
    )

    testthat::expect_identical(
      get_code(td, names = "iris_ds"),
      paste(
        c(
          "`add column` <- function(lhs, rhs) cbind(lhs, rhs)",
          "iris_ds <- `add column`(iris, data.frame(new_col = \"new column\"))"
        ),
        collapse = "\n"
      )
    )
  })

  testthat::it("without special characters is cleaned and detected in code dependency", {
    td <- within(
      qenv(),
      {
        `add_column` <- function(lhs, rhs) cbind(lhs, rhs)
        iris_ds <- `add_column`(iris, data.frame(new_col = "new column"))
      }
    )

    testthat::expect_identical(
      get_code(td, names = "iris_ds"),
      paste(
        c(
          "add_column <- function(lhs, rhs) cbind(lhs, rhs)",
          "iris_ds <- add_column(iris, data.frame(new_col = \"new column\"))"
        ),
        collapse = "\n"
      )
    )
  })

  testthat::it("with non-native pipe used as function is detected code dependency", {
    td <- within(
      qenv(),
      {
        `%add_column%` <- function(lhs, rhs) cbind(lhs, rhs)
        iris_ds <- `%add_column%`(iris, data.frame(new_col = "new column"))
      }
    )

    # Note that the original code is changed to use the non-native pipe operator
    # correctly.
    testthat::expect_identical(
      get_code(td, names = "iris_ds"),
      paste(
        c(
          "`%add_column%` <- function(lhs, rhs) cbind(lhs, rhs)",
          "iris_ds <- iris %add_column% data.frame(new_col = \"new column\")"
        ),
        collapse = "\n"
      )
    )
  })

  testthat::it("with non-native pipe is detected code dependency", {
    td <- within(
      qenv(),
      {
        `%add_column%` <- function(lhs, rhs) cbind(lhs, rhs)
        iris_ds <- iris %add_column% data.frame(new_col = "new column")
      }
    )

    # Note that the original code is changed to use the non-native pipe operator
    # correctly.
    testthat::expect_identical(
      get_code(td, names = "iris_ds"),
      paste(
        c(
          "`%add_column%` <- function(lhs, rhs) cbind(lhs, rhs)",
          "iris_ds <- iris %add_column% data.frame(new_col = \"new column\")"
        ),
        collapse = "\n"
      )
    )
  })
})


# missing objects -------------------------------------------------------------------------------------------------

testthat::test_that("get_code raises warning for missing names", {
  q <- eval_code(qenv(), code = c("a<-1;b<-2"))
  testthat::expect_warning(
    testthat::expect_equal(get_code(q, names = "c"), ""),
    " not found in code: c"
  )
})

# comments and white spaces --------------------------
testthat::test_that("comments are preserved in the output code", {
  # If comment is on top, it gets moved to the first call.
  # Any other comment gets moved to the call above.
  # Comments get pasted if there are two assigned to the same call.
  code <- "
    # initial comment
    a <- 1 # A comment
    b <- 2 # inline comment
    c <- 3 # C comment
    # inbetween comment
    d <- 4
    # finishing comment
  "

  q <- eval_code(qenv(), code)
  testthat::expect_identical(get_code(q), code)
})

testthat::test_that("original formatting and comments are preserved when expression has a srcref", {
  code <- "# comment
    a <- 1\n

    # comment
    \n
  "
  expr <- parse(text = code, keep.source = TRUE)
  testthat::expect_identical(get_code(eval_code(qenv(), expr)), code)
})
