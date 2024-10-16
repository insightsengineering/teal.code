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


# names parameter -------------------------------------------------------------------------------------------------

testthat::test_that("handles empty @code slot", {
  testthat::expect_identical(
    get_code(qenv(), names = "a"),
    character(0)
  )
  testthat::expect_identical(
    get_code(eval_code(qenv(), code = ""), names = "a"),
    ""
  )
})

testthat::test_that("handles the code without symbols on rhs", {
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

testthat::test_that("handles the code included in curly brackets", {
  code <- "{1 + 1;a <- 5}"

  testthat::expect_identical(
    get_code(eval_code(qenv(), code), names = "a"),
    "a <- 5"
  )
})

testthat::test_that("handles the code of length > 1 when at least one is enclosed in curly brackets", {
  code <- c("{a<-5}", "1+1")
  q <- eval_code(eval_code(qenv(), code[1]), code[2])

  testthat::expect_identical(
    get_code(q, names = "a"),
    "a <- 5"
  )
})


testthat::test_that("extracts the code of a binding from character vector containing simple code", {
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

testthat::test_that("extracts the code without downstream usage", {
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

testthat::test_that("works for datanames of length > 1", {
  code <- c(
    "a <- 1",
    "b <- 2"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = c("a", "b")),
    paste(code, collapse = "\n")
  )
})

testthat::test_that("warns if binding doesn't exist in code", {
  code <- c("a <- 1")
  q <- eval_code(qenv(), code)
  testthat::expect_warning(
    get_code(q, names = "c"),
    "Object\\(s\\) not found in code: c"
  )
})

testthat::test_that("does not fall into a loop", {
  code <- c(
    "a <- 1",
    "b <- a",
    "c <- b",
    "a <- c"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "a"),
    paste(code, collapse = "\n")
  )
  testthat::expect_identical(
    get_code(q, names = "b"),
    paste(code[1:2], collapse = "\n")
  )
  testthat::expect_identical(
    get_code(q, names = "c"),
    paste(code[1:3], collapse = "\n")
  )
})


testthat::test_that("extracts code of a parent binding but only those evaluated before coocurence", {
  code <- c(
    "a <- 1",
    "b <- a",
    "a <- 2"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    paste("a <- 1", "b <- a", sep = "\n")
  )
})

testthat::test_that("extracts the code of a parent binding if used as an arg in a function call", {
  code <- c(
    "a <- 1",
    "b <- identity(x = a)",
    "a <- 2"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    paste("a <- 1", "b <- identity(x = a)", sep = "\n")
  )
})

testthat::test_that("extracts the code when using <<-", {
  code <- c(
    "a <- 1",
    "b <- a",
    "b <<- b + 2"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    paste("a <- 1", "b <- a", "b <<- b + 2", sep = "\n")
  )
})

testthat::test_that("detects every assign calls even if not evaluated, if there is only one assignment in this line", {
  code <- c(
    "a <- 1",
    "b <- 2",
    "eval(expression({b <- b + 2}))"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    paste("b <- 2", "eval(expression({\n    b <- b + 2\n}))", sep = "\n")
  )
})

testthat::test_that("returns result of length 1 for non-empty input", {
  q1 <- qenv()
  q1 <- within(q1, {
    a <- 1
    b <- a^5
    c <- list(x = 2)
  })

  testthat::expect_length(get_code(q1, deparse = FALSE), 1)
  testthat::expect_length(get_code(q1, deparse = TRUE), 1)
})

testthat::test_that("does not break if code is separated by ;", {
  code <- c(
    "a <- 1;a <- a + 1"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "a"),
    gsub(";", "\n", code, fixed = TRUE)
  )
})

testthat::test_that("does not break if code uses quote()", {
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

testthat::test_that("does not break if object is used in a function on lhs", {
  code <- c(
    "data(iris)",
    "iris2 <- iris",
    "names(iris) <- letters[1:5]"
  )
  q <- eval_code(qenv(), code = code)
  testthat::expect_identical(
    get_code(q, names = "iris"),
    paste(code[c(1, 3)], collapse = "\n")
  )
})

testthat::test_that(
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
      paste(code, collapse = "\n")
    )
  }
)

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
    paste("assign(\"b\", 5)", "b <- b + 2", sep = "\n")
  )
  testthat::expect_identical(
    get_code(q, names = "c"),
    paste(
      "assign(\"b\", 5)",
      "assign(value = 7, x = \"c\")",
      "b <- b + 2",
      "c <- b",
      sep = "\n"
    )
  )
  testthat::expect_identical(
    get_code(q, names = "d"),
    paste("assign(value = 15, x = \"d\")", "d <- d * 2", sep = "\n")
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
    paste(code, collapse = "\n")
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
    paste(code, collapse = "\n")
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
    paste(c(code[1], "y <- x"), collapse = "\n")
  )
  testthat::expect_identical(
    get_code(q2, names = "y"),
    "y <- x <- 2"
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
    paste(gsub(" #@linksto x", "", code, fixed = TRUE), collapse = "\n")
  )
})

testthat::test_that("@linksto makes a line being returned for an affected binding", {
  code <- "
  a <- 1 # @linksto b
  b <- 2
  "
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "b"),
    paste("a <- 1", "b <- 2", sep = "\n")
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
      paste("a <- 1", "b <- 2", sep = "\n")
    )
  }
)

testthat::test_that(
  "lines affecting parent evaluated after co-occurrence are not included in output when using @linksto",
  {
    code <- c(
      "a <- 1 ",
      "b <- 2 # @linksto a",
      "a <- a + 1",
      "b <- b + 1"
    )
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "a"),
      paste("a <- 1", "b <- 2", "a <- a + 1", sep = "\n")
    )
    testthat::expect_identical(
      get_code(q, names = "b"),
      paste("b <- 2", "b <- b + 1", sep = "\n")
    )
  }
)

testthat::test_that(
  "@linksto gets extracted if it's a side-effect on a dependent object (even of a dependent object)",
  {
    code <- "
      iris[1:5, ] -> iris2
      iris_head <- head(iris) # @linksto iris3
      iris3 <- iris_head[1, ] # @linksto iris2
      classes <- lapply(iris2, class)
    "
    q <- eval_code(qenv(), code)
    testthat::expect_identical(
      get_code(q, names = "classes"),
      paste("iris2 <- iris[1:5, ]",
            "iris_head <- head(iris)",
            "iris3 <- iris_head[1, ]",
            "classes <- lapply(iris2, class)",
            sep = "\n"
      )
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
    "b <- 2"
  )
  testthat::expect_identical(
    get_code(q, names = "foo"),
    "foo <- function(b) {\n    b <- b + 2\n}"
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
    "b <- 2"
  )
  testthat::expect_identical(
    get_code(q, names = "foo"),
    "foo <- function(b) {\n    function(c) {\n        b <- c + 2\n    }\n}"
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
    "b <- 2\nb <- b + 1"
  )
  testthat::expect_identical(
    get_code(q, names = "foo"),
    "foo <- function(b) {\n    function(c) {\n        b <- c + 2\n    }\n}"
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
    paste(code, sep = "\n")
  )
})

testthat::test_that("ignores occurrence in function definition without { curly brackets", {
  code <- c(
    "b <- 2",
    "foo <- function(b) b <- b + 2 "
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "foo"),
    "foo <- function(b) b <- b + 2"
  )
  testthat::expect_identical(
    get_code(q, names = "b"),
    "b <- 2"
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
    paste("a <- 1", "b <- 2", "foo <- function(b) {\n    b <- b + 2\n}", "b <- foo(a)", sep = "\n")
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
    paste("x <- 1", "foo <- function(foo = 1) \"text\"", "a <- foo(x)", sep = "\n")
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
    "foo <- function() {\n    env <- parent.frame()\n    env$x <- 0\n}\nfoo()"
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
    "x <- data.frame(a = 1:3)"
  )
  testthat::expect_identical(
    get_code(q, names = "a"),
    paste("x <- data.frame(a = 1:3)",
          "a <- data.frame(y = 1:3)",
          "a$x <- a$y",
          "a$x <- a$x + 2",
          "a$x <- x$a",
          sep = "\n"
    )
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
    paste(code, collapse = "\n")
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
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "x"),
    paste(
      'setClass("aclass", slots = c(a = "numeric", x = "numeric", y = "numeric"))',
      'x <- new("aclass", a = 1:3, x = 1:3, y = 1:3)',
      sep = "\n"
    )
  )
  testthat::expect_identical(
    get_code(q, names = "a"),
    paste(
      'setClass("aclass", slots = c(a = "numeric", x = "numeric", y = "numeric"))',
      'x <- new("aclass", a = 1:3, x = 1:3, y = 1:3)',
      'a <- new("aclass", a = 1:3, x = 1:3, y = 1:3)',
      "a@x <- a@y",
      "a@x <- a@x + 2",
      "a@x <- x@a",
      sep = "\n"
    )
  )
})



# libraries -------------------------------------------------------------------------------------------------------

testthat::test_that("library() and require() are always returned", {
  code <- c(
    "set.seed(1)",
    "library(random.cdisc.data)",
    "require(dplyr)",
    "library(MultiAssayExperiment)",
    "x <- 5",
    "y <- 6"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "x"),
    paste(
      "library(random.cdisc.data)",
      "require(dplyr)",
      "library(MultiAssayExperiment)",
      "x <- 5",
      sep = "\n"
    )
  )
})


# data() ----------------------------------------------------------------------------------------------------------

testthat::test_that("data() call is returned when data name is provided as is", {
  code <- c(
    "set.seed(1)",
    "library(random.cdisc.data)",
    "require(dplyr)",
    "library(MultiAssayExperiment)",
    "data(miniACC, envir = environment())",
    "x <- miniACC"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "x"),
    paste(
      "library(random.cdisc.data)",
      "require(dplyr)",
      "library(MultiAssayExperiment)",
      "data(miniACC, envir = environment())",
      "x <- miniACC",
      sep = "\n"
    )
  )
})

testthat::test_that("data() call is returned when data name is provided as a character", {
  code <- c(
    "set.seed(1)",
    "library(random.cdisc.data)",
    "require(dplyr)",
    "library(MultiAssayExperiment)",
    "data('mtcars')",
    "z <- mtcars"
  )
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q, names = "z"),
    paste(
      "library(random.cdisc.data)",
      "require(dplyr)",
      "library(MultiAssayExperiment)",
      "data(\"mtcars\")",
      "z <- mtcars",
      sep = "\n"
    )
  )
})
