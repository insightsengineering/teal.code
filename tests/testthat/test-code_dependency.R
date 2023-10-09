testthat::test_that("get_code_dependency extract code of a binding from a simple code put in a character", {
  q <- c(
    "a <- 1",
    "b <- 2"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "a"),
    "a <- 1"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "b"),
    "b <- 2"
  )
})

testthat::test_that("get_code_dependency warns if binding doesn't exist in a code", {
  q <- c(
    "a <- 1",
    "b <- 2"
  )
  testthat::expect_warning(
    get_code_dependency(q, names = "c")
  )
})


testthat::test_that(
  "get_code_dependency extracts code of a parent binding but only those evaluated before coocurence",
  {
    q <- c(
      "a <- 1",
      "b <- a",
      "a <- 2"
    )
    testthat::expect_identical(
      get_code_dependency(q, names = "b"),
      c("a <- 1", "b <- a")
    )
  }
)

testthat::test_that("get_code_dependency extracts code of a parent binding if used as an arg in fun call", {
  q <- c(
    "a <- 1",
    "b <- identity(x = a)",
    "a <- 2"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "b"),
    c("a <- 1", "b <- identity(x = a)")
  )
})

testthat::test_that("get_code_dependency is possible to output the code for multiple objects", {
  q <- c(
    "a <- 1",
    "b <- 2",
    "c <- 3"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = c("a", "b")),
    c("a <- 1", "b <- 2")
  )
})

testthat::test_that("get_code_dependency can't extract the code when no assign operator", {
  q <- c(
    "a <- 1",
    "assign('b', 5)",
    "b <- b + 2"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "b"),
    "b <- b + 2"
  )
})

testthat::test_that("@linksto tag indicate affected object if object is assigned anywhere in a code", {
  q <- c(
    "a <- 1",
    "assign('b', 5) # @linksto b",
    "b <- b + 2"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "b"),
    c("assign(\"b\", 5)", "b <- b + 2")
  )
})


testthat::test_that(
  "get_code_dependency can extract the code when function creates an object which is used only on rhs",
  {
    q <- c(
      "data(iris)",
      "iris2 <- head(iris)"
    )
    testthat::expect_identical(
      get_code_dependency(q, names = "iris2"),
      c("data(iris)", "iris2 <- head(iris)")
    )
  }
)

testthat::test_that("get_code_dependency can extract the code when using <<-", {
  q <- c(
    "a <- 1",
    "b <- a",
    "b <<- b + 2"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "b"),
    c("a <- 1", "b <- a", "b <<- b + 2")
  )
})

testthat::test_that("get_code_dependency detects every assign calls even if not evaluated", {
  q <- c(
    "a <- 1",
    "b <- 2",
    "eval(expression({b <- b + 2}))"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "b"),
    c("b <- 2", "eval(expression({\n    b <- b + 2\n}))")
  )
})


# @linksto ---------------------------------------------------------------------------------------------------------


testthat::test_that("@linksto cause to return this line for affected binding", {
  q <- "
  a <- 1 # @linksto b
  b <- 2
  "

  testthat::expect_identical(
    get_code_dependency(q, names = "b"),
    c("a <- 1", "b <- 2")
  )
})

testthat::test_that(
  "@linksto returns this line for affected binding
  even if object is not specificed/created in the same eval_code",
  {
    q <- c(
      "a <- 1 # @linksto b",
      "b <- 2"
    )
    testthat::expect_identical(
      get_code_dependency(q, names = "b"),
      c("a <- 1", "b <- 2")
    )
  }
)

testthat::test_that(
  "@linksto returns this line for affected binding
  if object is not specificed in the same element of code",
  {
    q <- c(
      "a <- 1 ",
      "b <- 2 # @linksto a"
    )
    testthat::expect_identical(
      get_code_dependency(q, names = "a"),
      c("a <- 1", "b <- 2")
    )
  }
)

testthat::test_that(
  "lines affecting parent evaluated after co-occurrence are not included in get_code_dependency output",
  {
    q <- c(
      "a <- 1",
      "b <- a",
      "a <- 3"
    )
    testthat::expect_identical(
      get_code_dependency(q, names = "b"),
      c("a <- 1", "b <- a")
    )
  }
)

testthat::test_that(
  "lines affecting parent evaluated after co-occurrence are not included in get_code_dependency output
  when using @linksto",
  {
    q <- c(
      "a <- 1 ",
      "b <- 2 # @linksto a",
      "a <- a + 1",
      "b <- b + 1"
    )
    testthat::expect_identical(
      get_code_dependency(q, names = "a"),
      c("a <- 1", "b <- 2", "a <- a + 1")
    )
    testthat::expect_identical(
      get_code_dependency(q, names = "b"),
      c("b <- 2", "b <- b + 1")
    )
  }
)

testthat::test_that(
  "@linksto gets extracted if it's a side-effect on a dependent object",
  {
    q <- "
      iris[1:5, ] -> iris2
      iris_head <- head(iris) # @linksto iris2
      classes <- lapply(iris2, class)
    "
    testthat::expect_identical(
      get_code_dependency(q, names = "classes"),
      c("iris2 <- iris[1:5, ]", "iris_head <- head(iris)", "classes <- lapply(iris2, class)")
    )
  }
)

testthat::test_that(
  "@linksto gets extracted if it's a side-effect on a dependent object of a dependent object",
  {
    q <- "
      iris[1:5, ] -> iris2
      iris_head <- head(iris) # @linksto iris3
      iris3 <- iris_head[1, ] # @linksto iris2
      classes <- lapply(iris2, class)
    "
    testthat::expect_identical(
      get_code_dependency(q, names = "classes"),
      c("iris2 <- iris[1:5, ]", "iris_head <- head(iris)", "iris3 <- iris_head[1, ]", "classes <- lapply(iris2, class)")
    )
  }
)

# functions -------------------------------------------------------------------------------------------------------

testthat::test_that("get_code_dependency ignores occurrence in function definition", {
  q <- c(
    "b <- 2",
    "foo <- function(b) { b <- b + 2 }"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "b"),
    "b <- 2"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "foo"),
    "foo <- function(b) {\n    b <- b + 2\n}"
  )
})

testthat::test_that("get_code_dependency ignores occurrence in function definition without { curly brackets", {
  q <- c(
    "b <- 2",
    "foo <- function(b) b <- b + 2 "
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "foo"),
    "foo <- function(b) b <- b + 2"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "b"),
    "b <- 2"
  )
})

testthat::test_that("get_code_dependency ignores effect of the object which occurs in a function definition", {
  q <- c(
    "b <- 2",
    "foo <- function(b) { b <- b + 2 }"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "b"),
    c("b <- 2")
  )
})

testthat::test_that("get_code_dependency detects occurrence of the function object", {
  q <- c(
    "a <- 1",
    "b <- 2",
    "foo <- function(b) { b <- b + 2 }",
    "b <- foo(a)"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "b"),
    c("a <- 1", "b <- 2", "foo <- function(b) {\n    b <- b + 2\n}", "b <- foo(a)")
  )
})

testthat::test_that(
  "Can't detect occurrence of function definition when a formal is named the same as a function",
  {
    q <- c(
      "x <- 1",
      "foo <- function(foo = 1) 'text'",
      "a <- foo(x)"
    )
    testthat::expect_identical(
      get_code_dependency(q, names = "a"),
      c("x <- 1", "foo <- function(foo = 1) \"text\"", "a <- foo(x)")
    )
  }
)

# $ ---------------------------------------------------------------------------------------------------------------

testthat::test_that("get_code_dependency understands $ usage and do not treat rhs of $ as objects (only lhs)", {
  q <- c(
    "x <- data.frame(a = 1:3)",
    "a <- data.frame(y = 1:3)",
    "a$x <- a$y",
    "a$x <- a$x + 2",
    "a$x <- x$a"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "x"),
    c("x <- data.frame(a = 1:3)")
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "a"),
    c("x <- data.frame(a = 1:3)", "a <- data.frame(y = 1:3)", "a$x <- a$y", "a$x <- a$x + 2", "a$x <- x$a")
  )
})

testthat::test_that("get_code_dependency detects cooccurrence properly even if all objects are on rhs", {
  q <- c(
    "a <- 1",
    "b <- list(c = 2)",
    "b[[a]] <- 3"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "b"),
    c("a <- 1", "b <- list(c = 2)", "b[[a]] <- 3")
  )
})


# @ ---------------------------------------------------------------------------------------------------------------

testthat::test_that("get_code_dependency understands @ usage and do not treat rhs of @ as objects (only lhs)", {
  q <- c(
    "setClass('aclass', slots = c(a = 'numeric', x = 'numeric', y = 'numeric')) # @linksto a x",
    "x <- new('aclass', a = 1:3, x = 1:3, y = 1:3)",
    "a <- new('aclass', a = 1:3, x = 1:3, y = 1:3)",
    "a@x <- a@y",
    "a@x <- a@x + 2",
    "a@x <- x@a"
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "x"),
    c(
      'setClass("aclass", slots = c(a = "numeric", x = "numeric", y = "numeric"))',
      'x <- new("aclass", a = 1:3, x = 1:3, y = 1:3)'
    )
  )
  testthat::expect_identical(
    get_code_dependency(q, names = "a"),
    c(
      'setClass("aclass", slots = c(a = "numeric", x = "numeric", y = "numeric"))',
      'x <- new("aclass", a = 1:3, x = 1:3, y = 1:3)',
      'a <- new("aclass", a = 1:3, x = 1:3, y = 1:3)',
      "a@x <- a@y",
      "a@x <- a@x + 2",
      "a@x <- x@a"
    )
  )
})
