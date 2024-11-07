testthat::test_that("eval_code evaluates the code in the qenvs environment", {
  q <- qenv()
  q1 <- eval_code(q, quote(iris1 <- iris))
  q2 <- eval_code(q1, quote(b <- nrow(iris1)))
  testthat::expect_identical(get_var(q2, "b"), 150L)
})

testthat::test_that("eval_code doesn't have access to environment where it's called", {
  q <- qenv()
  q1 <- eval_code(q, quote(a <- 1))
  b <- 2L
  testthat::expect_s3_class(
    eval_code(q1, quote(d <- b)),
    c("qenv.error", "try-error", "error", "condition")
  )
})

testthat::test_that("@env in qenv is always a sibling of .GlobalEnv", {
  q1 <- qenv()
  testthat::expect_identical(parent.env(q1@env), parent.env(.GlobalEnv))

  q2 <- eval_code(q1, quote(a <- 1L))
  testthat::expect_identical(parent.env(q2@env), parent.env(.GlobalEnv))
  q3 <- eval_code(q2, quote(b <- 2L))
  testthat::expect_identical(parent.env(q3@env), parent.env(.GlobalEnv))
})

testthat::test_that("getting object from the package namespace works even if library in the same call", {
  testthat::expect_s4_class(
    eval_code(
      qenv(),
      as.expression(c(
        quote(library(checkmate)),
        quote(assert_number(1))
      ))
    ),
    "qenv"
  )
})

testthat::test_that("eval_code works with character", {
  q1 <- eval_code(qenv(), "a <- 1")

  testthat::expect_identical(unlist(q1@code), "a <- 1")
  testthat::expect_equal(q1@env, list2env(list(a = 1)))
})

testthat::test_that("eval_code works with expression", {
  q1 <- eval_code(qenv(), as.expression(quote(a <- 1)))

  testthat::expect_identical(unlist(q1@code), "a <- 1")
  testthat::expect_equal(q1@env, list2env(list(a = 1)))
})

testthat::test_that("eval_code works with quoted", {
  q1 <- eval_code(qenv(), quote(a <- 1))

  testthat::expect_identical(unlist(q1@code), "a <- 1")
  testthat::expect_equal(q1@env, list2env(list(a = 1)))
})

testthat::test_that("eval_code works with quoted code block", {
  q1 <- eval_code(
    qenv(),
    quote({
      a <- 1
      b <- 2
    })
  )

  testthat::expect_equal(
    unlist(q1@code),
    c("a <- 1\n", "b <- 2")
  )
  testthat::expect_equal(q1@env, list2env(list(a = 1, b = 2)))
})

testthat::test_that("eval_code fails with unquoted expression", {
  testthat::expect_error(eval_code(qenv(), a <- b), "object 'b' not found")
})

testthat::test_that("an error when calling eval_code returns a qenv.error object which has message and trace", {
  q <- eval_code(qenv(), quote(x <- 1))
  q <- eval_code(q, quote(y <- 2))
  q <- eval_code(q, quote(z <- w * x))
  testthat::expect_s3_class(q, "qenv.error")
  testthat::expect_equal(
    unname(q$trace),
    c("x <- 1", "y <- 2", "z <- w * x")
  )
  testthat::expect_equal(q$message, "object 'w' not found \n when evaluating qenv code:\nz <- w * x")
})

testthat::test_that(
  "a warning when calling eval_code returns a qenv object which has warnings as attributes of code",
  {
    q <- eval_code(qenv(), quote("iris_data <- iris"))
    q <- eval_code(q, quote("p <- hist(iris_data[, 'Sepal.Length'], ff = '')"))
    testthat::expect_s4_class(q, "qenv")
    testthat::expect_equal(
      lapply(q@code, attr, "warning"),
      list(NULL, "> \"ff\" is not a graphical parameter\n")
    )
  }
)

testthat::test_that(
  "eval_code associates warnings with call by adding attribute to code element",
  {
    q <- eval_code(qenv(), c("x <- 1", "y <- 1", "warning('warn1')"))
    testthat::expect_equal(
      lapply(q@code, attr, "warning"),
      list(NULL, NULL, "> warn1\n")
    )
  }
)


testthat::test_that(
  "eval_code associates messages with call by adding attribute to code element",
  {
    q <- eval_code(qenv(), quote("iris_data <- head(iris)"))
    q <- eval_code(q, quote("message('This is a message')"))
    testthat::expect_s4_class(q, "qenv")
    testthat::expect_equal(
      lapply(q@code, attr, "message"),
      list(
        NULL,
        "> This is a message\n"
      )
    )
  }
)

testthat::test_that(
  "eval_code returns a qenv object with empty messages and warnings as code attributes, when none are returned",
  {
    q <- eval_code(qenv(), quote("iris_data <- head(iris)"))
    testthat::expect_s4_class(q, "qenv")
    testthat::expect_null(attr(q@code, "message"))
    testthat::expect_null(attr(q@code, "warning"))
  }
)

testthat::test_that("eval_code returns a qenv object with dependency attribute", {
  q <- eval_code(qenv(), "iris_data <- head(iris)")
  testthat::expect_identical(get_code_attr(q, "dependency"), c("iris_data", "<-", "head", "iris"))
})
testthat::test_that("eval_code returns a qenv object with dependency attribute that contains linksto information", {
  q2 <- eval_code(qenv(), c("x <- 5", "iris_data <- head(iris)", "nrow(iris_data) #@linksto x"))
  testthat::expect_identical(
    lapply(q2@code, attr, "dependency"),
    list(
      c("x", "<-"),
      c("iris_data", "<-", "head", "iris"),
      c("x", "<-", "nrow", "iris_data")
    )
  )
})
testthat::test_that(
  "eval_code returns a qenv object with dependency attribute that extracts functions after '<-' part",
  {
    q3 <- eval_code(qenv(), c("nrow(iris)", "head(iris)"))
    testthat::expect_identical(
      lapply(q3@code, attr, "dependency"),
      list(
        c("<-", "nrow", "iris"),
        c("<-", "head", "iris")
      )
    )
  }
)



# comments --------------------------------------------------------------------------------------------------------

testthat::test_that("comments fall into proper calls", {
  # If comment is on top, it gets moved to the first call.
  # Any other comment gets moved to the call above.
  code <- "
    # initial comment
    a <- 1
    b <- 2 # inline comment
    c <- 3
    # inbetween comment
    d <- 4
    # finishing comment
  "

  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    get_code(q),
    code
  )
})

testthat::test_that("comments get pasted when they fall into calls", {
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
  testthat::expect_identical(
    get_code(q),
    code
  )
})

testthat::test_that("comments alone are pasted to the next/following call element", {
  code <- c("x <- 5", "# comment", "y <- 6")
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    unlist(q@code)[2],
    paste(code[2:3], collapse = "\n")
  )
  testthat::expect_identical(
    get_code(q),
    paste(code, collapse = "\n")
  )
})

testthat::test_that("comments at the end of src are added to the previous call element", {
  code <- c("x <- 5", "# comment")
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    unlist(q@code),
    paste(code[1:2], collapse = "\n")
  )
  testthat::expect_identical(
    get_code(q),
    paste(code, collapse = "\n")
  )
})

testthat::test_that("comments from the same line are associated with it's call", {
  code <- c("x <- 5", " y <- 4 # comment", "z <- 5")
  q <- eval_code(qenv(), code)
  testthat::expect_identical(
    unlist(q@code)[2],
    paste0(code[2], "\n")
  )
  testthat::expect_identical(
    get_code(q),
    paste(code, collapse = "\n")
  )
})

testthat::test_that("comments passed alone to eval_code are separate calls", {
  code <- c("x <- 5", "# comment")
  q <- eval_code(eval_code(qenv(), code[1]), code[2])
  testthat::expect_identical(
    get_code(q),
    pasten(code)
  )
})

testthat::test_that("comments passed alone to eval_code that contain @linksto tag have detected dependency", {
  code <- c("x <- 5", "# comment @linksto x")
  q <- eval_code(eval_code(qenv(), code[1]), code[2])
  testthat::expect_identical(
    get_code(q),
    pasten(code)
  )
  testthat::expect_identical(
    get_code(q, names = 'x'),
    pasten(code)
  )
  testthat::expect_identical(
    attr(q@code[[2]], "dependency"),
    "x"
  )
})
