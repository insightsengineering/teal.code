testthat::test_that("get_code extract code of a binding from a simple code put in a character", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- 2")

  testthat::expect_identical(
    get_code(q, deparse = FALSE, names = "a"),
    "a <- 1"
  )
  testthat::expect_identical(
    get_code(q, deparse = FALSE, names = "b"),
    "b <- 2"
  )
})

testthat::test_that("get_code does not extract code of a binding from a code put in an expression", {
  q <- new_qenv()
  q <- eval_code(q, expression(a <- 1))


  testthat::expect_identical(
    suppressMessages(get_code(q, deparse = FALSE, names = "a")),
    NULL
  )
  testthat::expect_message(
    get_code(q, deparse = FALSE, names = "a"),
    "Code dependency is supported only for the code provided as a character in "
  )
})

testthat::test_that("get_code does not extract code of a binding from a code put in a language", {
  q <- new_qenv()
  q <- eval_code(q, quote(b <- 2))

  testthat::expect_identical(
    suppressMessages(get_code(q, deparse = FALSE, names = "b")),
    NULL
  )
  testthat::expect_message(
    get_code(q, deparse = FALSE, names = "b"),
    "Code dependency is supported only for the code provided as a character in "
  )
})

testthat::test_that("get_code warns if binding doesn't exist in a code", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- 2")

  testthat::expect_warning(
    get_code(q, deparse = FALSE, names = "c")
  )
})


testthat::test_that("get_code extract code of a parent binding but only those evaluated before coocurence", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- a")
  q <- eval_code(q, "a <- 2")

  testthat::expect_identical(
    get_code(q, deparse = FALSE, names = "b"),
    c("a <- 1", "b <- a")
  )
})

testthat::test_that("get_code extract code of a parent binding if used in a function", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- identity(x = a)")
  q <- eval_code(q, "a <- 2")

  testthat::expect_identical(
    get_code(q, deparse = FALSE, names = "b"),
    c("a <- 1", "b <- identity(x = a)")
  )
})

testthat::test_that("get_code is possible to output the code for multiple objects", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- 2")
  q <- eval_code(q, "c <- 3")

  testthat::expect_identical(
    get_code(q, deparse = FALSE, names = c("a", "b")),
    c("a <- 1", "b <- 2")
  )
})

testthat::test_that("get_code can't extract the code when using assign", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "assign('b', 5)")
  q <- eval_code(q, "b <- b + 2")
  testthat::expect_identical(
    get_code(q, deparse = FALSE, names = "b"),
    "b <- b + 2"
  )
})

testthat::test_that("get_code can extract the code when using <<-", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- a")
  q <- eval_code(q, "b <<- b + 2")
  testthat::expect_identical(
    get_code(q, deparse = FALSE, names = "b"),
    c("a <- 1", "b <- a", "b <<- b + 2")
  )
})

testthat::test_that("get_code extracts the code when using eval with object", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- 2")
  q <- eval_code(q, "eval(expression({b <- b + 2}))")
  testthat::expect_identical(
    get_code(q, deparse = FALSE, names = "b"),
    c("b <- 2", "eval(expression({\n    b <- b + 2\n}))")
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
    get_code(q, deparse = FALSE, names = "b"),
    c("a <- 1", "b <- 2")
  )
})

testthat::test_that(
  "@effect does not return this line for affected binding
  if object is not specificed in the same eval_code",
  {
    q <- new_qenv()
    q <- eval_code(q, "a <- 1 # @effect b") # IT WOULD BE GREAT IF IT DID
    q <- eval_code(q, "b <- 2")

    testthat::expect_identical(
      get_code(q, deparse = FALSE, names = "b"),
      c("b <- 2")
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
      get_code(q, deparse = FALSE, names = "a"),
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
      get_code(q, deparse = FALSE, names = "b"),
      c("a <- 1", "b <- a")
    )
  }
)

testthat::test_that(
  "lines affecting parent evaluated after co-occurrence are not included in get_code output when using @effect",
  {
    q <- new_qenv()
    q <- eval_code(q, "a <- 1 ")
    q <- eval_code(q, "b <- 2 # @effect a")
    q <- eval_code(q, "a <- a + 1")
    q <- eval_code(q, "b <- b + 1")

    testthat::expect_identical(
      get_code(q, deparse = FALSE, names = "a"),
      c("a <- 1", "b <- 2", "a <- a + 1")
    )
    testthat::expect_identical(
      get_code(q, deparse = FALSE, names = "b"),
      c("b <- 2", "b <- b + 1")
    )
  }
)

testthat::test_that(
  "@effect gets extracted if it's a side-effect on a dependent object",
  {
    skip("Does not extract second line yet.")
    q <- new_qenv()
    q <- eval_code(q,
      code = "
      iris[1:5, ] -> iris2
      iris_head <- head(iris) # @effect iris2
      classes <- lapply(iris2, class)
    "
    )

    testthat::expect_identical(
      get_code(q, deparse = FALSE, names = "classes"),
      "iris2 <- iris[1:5, ]", "iris_head <- head(iris)", "classes <- lapply(iris2, class)"
    )
  }
)
