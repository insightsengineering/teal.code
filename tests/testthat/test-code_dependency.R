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

testthat::test_that("@effect tag indicate affected object if object is assigned anywhere in a code", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "assign('b', 5) # @effect b")
  q <- eval_code(q, "b <- b + 2")
  testthat::expect_identical(
    get_code(q, names = "b"),
    c("assign(\"b\", 5)", "b <- b + 2")
  )
})


testthat::test_that("get_code can't extract the code when function creates an object which is used only on rhs", {
  skip("Does not work yet!")
  q <- new_qenv()
  q <- eval_code(q, "data(iris) # @effect iris")
  q <- eval_code(q, "iris2 <- head(iris)")
  testthat::expect_identical(
    get_code(q, names = "iris2"),
    "iris2 <- head(iris)"
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

testthat::test_that("get_code extracts the code when using eval with object", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- 2")
  q <- eval_code(q, "eval(expression({b <- b + 2}))")
  testthat::expect_identical(
    get_code(q, names = "b"),
    c("b <- 2", "eval(expression({", "  b <- b + 2", "}))")
  )
})


# @effect ---------------------------------------------------------------------------------------------------------


testthat::test_that("@effect cause to return this line for affected binding", {
  q <- new_qenv()
  q <- eval_code(
    q,
    "
  a <- 1 # @effect b
  b <- 2
  "
  )

  testthat::expect_identical(
    get_code(q, names = "b"),
    c("a <- 1", "b <- 2")
  )
})

testthat::test_that(
  "@effect returns this line for affected binding
  even if object is not specificed/created in the same eval_code",
  {
    q <- new_qenv()
    q <- eval_code(q, "a <- 1 # @effect b")
    q <- eval_code(q, "b <- 2")

    testthat::expect_identical(
      get_code(q, names = "b"),
      c("a <- 1", "b <- 2")
    )
  }
)

testthat::test_that(
  "@effect returns this line for affected binding
  if object is not specificed in the same eval_code
  but it existed already in the qenv@env",
  {
    q <- new_qenv()
    q <- eval_code(q, "a <- 1 ")
    q <- eval_code(q, "b <- 2 # @effect a")

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
  "lines affecting parent evaluated after co-occurrence are not included in get_code output when using @effect",
  {
    skip("This needs to be fixed!")
    q <- new_qenv()
    q <- eval_code(q, "a <- 1 ")
    q <- eval_code(q, "b <- 2 # @effect a")
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
  "@effect gets extracted if it's a side-effect on a dependent object",
  {
    q <- new_qenv()
    q <- eval_code(q,
                   code = "
      iris[1:5, ] -> iris2
      iris_head <- head(iris) # @effect iris2
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
  "@effect gets extracted if it's a side-effect on a dependent object of a dependent object",
  {
    q <- new_qenv()
    q <- eval_code(q,
                   code = "
      iris[1:5, ] -> iris2
      iris_head <- head(iris) # @effect iris3
      iris3 <- iris_head[1, ] # @effect iris2
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
