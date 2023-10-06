testthat::test_that("get_code extract code of a binding from a simple code put in a character", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- 2")

  testthat::expect_identical(
    get_code(q, names = "a"),
    "a <- 1"
  )
  testthat::expect_identical(
    get_code(q, names = "b"),
    "b <- 2"
  )
})

testthat::test_that("get_code extracts the code of a binding from a code put in an expression", {
  q <- new_qenv()
  q <- eval_code(q, expression(a <- 1))
  q <- eval_code(q, expression(b <- 2))


  testthat::expect_identical(
    get_code(q, names = "a"),
    "a <- 1"
  )
})

testthat::test_that("get_code extracts the code of a binding from a code put in a language", {
  q <- new_qenv()
  q <- eval_code(q, expression(a <- 1))
  q <- eval_code(q, quote(b <- 2))

  testthat::expect_identical(
    get_code(q, names = "b"),
    "b <- 2"
  )
})

testthat::test_that("get_code warns if binding doesn't exist in a code", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- 2")

  testthat::expect_warning(
    get_code(q, names = "c")
  )
})


testthat::test_that("get_code extracts code of a parent binding but only those evaluated before coocurence", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- a")
  q <- eval_code(q, "a <- 2")

  testthat::expect_identical(
    get_code(q, names = "b"),
    c("a <- 1", "b <- a")
  )
})

testthat::test_that("get_code extracts code of a parent binding if used as an arg in fun call", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- identity(x = a)")
  q <- eval_code(q, "a <- 2")

  testthat::expect_identical(
    get_code(q, names = "b"),
    c("a <- 1", "b <- identity(x = a)")
  )
})

testthat::test_that("get_code is possible to output the code for multiple objects", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- 2")
  q <- eval_code(q, "c <- 3")

  testthat::expect_identical(
    get_code(q, names = c("a", "b")),
    c("a <- 1", "b <- 2")
  )
})

testthat::test_that("get_code can't extract the code when no assign operator", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "assign('b', 5)")
  q <- eval_code(q, "b <- b + 2")
  testthat::expect_identical(
    get_code(q, names = "b"),
    "b <- b + 2"
  )
})

testthat::test_that("@linksto tag indicate affected object if object is assigned anywhere in a code", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "assign('b', 5) # @linksto b")
  q <- eval_code(q, "b <- b + 2")
  testthat::expect_identical(
    get_code(q, names = "b"),
    c("assign(\"b\", 5)", "b <- b + 2")
  )
})


testthat::test_that("get_code can extract the code when function creates an object which is used only on rhs", {
  q <- new_qenv()
  q <- eval_code(q, "data(iris)")
  q <- eval_code(q, "iris2 <- head(iris)")
  testthat::expect_identical(
    get_code(q, names = "iris2"),
    c("data(iris)", "iris2 <- head(iris)")
  )
})

testthat::test_that("get_code can extract the code when using <<-", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- a")
  q <- eval_code(q, "b <<- b + 2")
  testthat::expect_identical(
    get_code(q, names = "b"),
    c("a <- 1", "b <- a", "b <<- b + 2")
  )
})
rm(list = "b", envir = .GlobalEnv)

testthat::test_that("get_code detects every assign calls even if not evaluated", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- 2")
  q <- eval_code(q, "eval(expression({b <- b + 2}))")
  testthat::expect_identical(
    get_code(q, names = "b"),
    c("b <- 2", "eval(expression({\n    b <- b + 2\n}))")
  )
})


# @linksto ---------------------------------------------------------------------------------------------------------


testthat::test_that("@linksto cause to return this line for affected binding", {
  q <- new_qenv()
  q <- eval_code(
    q,
    "
  a <- 1 # @linksto b
  b <- 2
  "
  )

  testthat::expect_identical(
    get_code(q, names = "b"),
    c("a <- 1", "b <- 2")
  )
})

testthat::test_that(
  "@linksto returns this line for affected binding
  even if object is not specificed/created in the same eval_code",
  {
    q <- new_qenv()
    q <- eval_code(q, "a <- 1 # @linksto b")
    q <- eval_code(q, "b <- 2")

    testthat::expect_identical(
      get_code(q, names = "b"),
      c("a <- 1", "b <- 2")
    )
  }
)

testthat::test_that(
  "@linksto returns this line for affected binding
  if object is not specificed in the same eval_code
  but it existed already in the qenv@env",
  {
    q <- new_qenv()
    q <- eval_code(q, "a <- 1 ")
    q <- eval_code(q, "b <- 2 # @linksto a")

    testthat::expect_identical(
      get_code(q, names = "a"),
      c("a <- 1", "b <- 2")
    )
  }
)


testthat::test_that(
  "lines affecting parent evaluated after co-occurrence are not included in get_code output",
  {
    q <- new_qenv()
    q <- eval_code(q, "a <- 1")
    q <- eval_code(q, "b <- a")
    q <- eval_code(q, "a <- 3")

    testthat::expect_identical(
      get_code(q, names = "b"),
      c("a <- 1", "b <- a")
    )
  }
)

testthat::test_that(
  "lines affecting parent evaluated after co-occurrence are not included in get_code output when using @linksto",
  {
    q <- new_qenv()
    q <- eval_code(q, "a <- 1 ")
    q <- eval_code(q, "b <- 2 # @linksto a")
    q <- eval_code(q, "a <- a + 1")
    q <- eval_code(q, "b <- b + 1")

    testthat::expect_identical(
      get_code(q, names = "a"),
      c("a <- 1", "b <- 2", "a <- a + 1")
    )
    testthat::expect_identical(
      get_code(q, names = "b"),
      c("b <- 2", "b <- b + 1")
    )
  }
)

testthat::test_that(
  "@linksto gets extracted if it's a side-effect on a dependent object",
  {
    q <- new_qenv()
    q <- eval_code(q,
      code = "
      iris[1:5, ] -> iris2
      iris_head <- head(iris) # @linksto iris2
      classes <- lapply(iris2, class)
    "
    )

    testthat::expect_identical(
      get_code(q, names = "classes"),
      c("iris2 <- iris[1:5, ]", "iris_head <- head(iris)", "classes <- lapply(iris2, class)")
    )
  }
)

testthat::test_that(
  "@linksto gets extracted if it's a side-effect on a dependent object of a dependent object",
  {
    q <- new_qenv()
    q <- eval_code(q,
      code = "
      iris[1:5, ] -> iris2
      iris_head <- head(iris) # @linksto iris3
      iris3 <- iris_head[1, ] # @linksto iris2
      classes <- lapply(iris2, class)
    "
    )

    testthat::expect_identical(
      get_code(q, names = "classes"),
      c("iris2 <- iris[1:5, ]", "iris_head <- head(iris)", "iris3 <- iris_head[1, ]", "classes <- lapply(iris2, class)")
    )
  }
)

testthat::test_that(
  "get_code returns the same class when names is specified and when not",
  {
    q <- eval_code(new_qenv(), "a <- 1")
    testthat::expect_identical(
      get_code(q, names = "a"),
      get_code(q)
    )
  }
)

testthat::test_that(
  "get_code returns single lines for code put in {} inside expressions",
  {
    q <- new_qenv()
    q <- eval_code(q, expression({
      a <- 1
      b <- 2
    }))

    testthat::expect_identical(
      get_code(q, names = "a"),
      "a <- 1"
    )
  }
)



# functions -------------------------------------------------------------------------------------------------------

testthat::test_that("get_code ignores occurrence in function definition", {
  q <- new_qenv()
  q <- eval_code(q, "b <- 2")
  q <- eval_code(q, "foo <- function(b) { b <- b + 2 }")

  testthat::expect_identical(
    get_code(q, names = "foo"),
    "foo <- function(b) {\n    b <- b + 2\n}"
  )
})


testthat::test_that("get_code ignores effect of the object which occurs in a function definition", {
  q <- new_qenv()
  q <- eval_code(q, "b <- 2")
  q <- eval_code(q, "foo <- function(b) { b <- b + 2 }")

  testthat::expect_identical(
    get_code(q, names = "b"),
    c("b <- 2")
  )
})

testthat::test_that("get_code detects occurrence of the function object", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- 2")
  q <- eval_code(q, "foo <- function(b) { b <- b + 2 }")
  q <- eval_code(q, "b <- foo(a)")

  testthat::expect_identical(
    get_code(q, names = "b"),
    c("a <- 1", "b <- 2", "foo <- function(b) {\n    b <- b + 2\n}", "b <- foo(a)")
  )
})

testthat::test_that(
  "Can't detect occurrence of function definition when a formal is named the same as a function",
  {
    testthat::skip("This does not return foo definition YET!")
    q <- new_qenv()
    q <- eval_code(q, "x <- 1")
    q <- eval_code(q, "foo <- function(foo = 1) 'text'")
    q <- eval_code(q, "a <- foo(x)")

    testthat::expect_identical(
      get_code(q, names = "a"),
      c("x <- 1", "foo <- function(foo = 1) 'text'", "a <- foo(x)")
    )
  }
)

# $ ---------------------------------------------------------------------------------------------------------------


testthat::test_that("get_code understands $ usage and do not treat rhs of $ as objects (only lhs)", {
  q <- new_qenv()
  q <- eval_code(q, "x <- data.frame(a = 1:3)")
  q <- eval_code(q, "a <- data.frame(y = 1:3)")
  q <- eval_code(q, "a$x <- a$y")
  q <- eval_code(q, "a$x <- a$x + 2")
  q <- eval_code(q, "a$x <- x$a")

  testthat::expect_identical(
    get_code(q, names = "x"),
    c("x <- data.frame(a = 1:3)")
  )
  testthat::expect_identical(
    get_code(q, names = "a"),
    c("x <- data.frame(a = 1:3)", "a <- data.frame(y = 1:3)", "a$x <- a$y", "a$x <- a$x + 2", "a$x <- x$a")
  )
})

testthat::test_that("get_code detects cooccurrence properly even if all objects are on rhs", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- list(c = 2)")
  q <- eval_code(q, "b[[a]] <- 3")

  testthat::expect_identical(
    get_code(q, names = "b"),
    c("a <- 1", "b <- list(c = 2)", "b[[a]] <- 3")
  )
})


# @ ---------------------------------------------------------------------------------------------------------------

testthat::test_that("get_code understands @ usage and do not treat rhs of @ as objects (only lhs)", {
  q <- new_qenv()
  q <- eval_code(q, "setClass('aclass', representation(a = 'numeric', x = 'numeric', y = 'numeric')) # @linksto a x")
  q <- eval_code(q, "x <- new('aclass', a = 1:3, x = 1:3, y = 1:3)")
  q <- eval_code(q, "a <- new('aclass', a = 1:3, x = 1:3, y = 1:3)")
  q <- eval_code(q, "a@x <- a@y")
  q <- eval_code(q, "a@x <- a@x + 2")
  q <- eval_code(q, "a@x <- x@a")

  testthat::expect_identical(
    get_code(q, names = "x"),
    c(
      'setClass("aclass", representation(a = "numeric", x = "numeric", y = "numeric"))',
      'x <- new("aclass", a = 1:3, x = 1:3, y = 1:3)'
    )
  )
  testthat::expect_identical(
    get_code(q, names = "a"),
    c(
      'setClass("aclass", representation(a = "numeric", x = "numeric", y = "numeric"))',
      'x <- new("aclass", a = 1:3, x = 1:3, y = 1:3)',
      'a <- new("aclass", a = 1:3, x = 1:3, y = 1:3)',
      "a@x <- a@y",
      "a@x <- a@x + 2",
      "a@x <- x@a"
    )
  )
})
