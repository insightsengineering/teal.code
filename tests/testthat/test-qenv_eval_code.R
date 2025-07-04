testthat::test_that("eval_code evaluates the code in the qenvs environment", {
  q <- qenv()
  q1 <- eval_code(q, quote(a <- 1L))
  q2 <- eval_code(q1, quote(b <- 1))
  testthat::expect_equal(q2, list2env(list(a = 1L, b = 1)))
})

testthat::test_that("eval_code locks the environment", {
  q <- eval_code(qenv(), quote(iris1 <- iris))
  testthat::expect_true(environmentIsLocked(q))
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
  testthat::expect_identical(get_code(q1), "a <- 1")
  testthat::expect_equal(q1, list2env(list(a = 1)))
})

testthat::test_that("eval_code works with expression", {
  q1 <- eval_code(qenv(), expression(a <- 1, b <- 2))
  testthat::expect_identical(get_code(q1), "a <- 1\nb <- 2")
  testthat::expect_equal(q1, list2env(list(a = 1, b = 2)))
})

testthat::test_that("eval_code ignores empty code", {
  q <- qenv()
  testthat::expect_identical(q, eval_code(q, ""))
})

testthat::test_that("eval_code preserves original formatting when `srcref` is present in the expression", {
  code <- "# comment
  a <- 1L"
  expr <- parse(text = code, keep.source = TRUE)
  q1 <- eval_code(qenv(), expr)
  testthat::expect_identical(get_code(q1), code)
  testthat::expect_equal(q1, list2env(list(a = 1L)))
})

testthat::test_that("eval_code works with quoted", {
  q1 <- eval_code(qenv(), quote(a <- 1))

  testthat::expect_identical(get_code(q1), "a <- 1")
  testthat::expect_equal(q1, list2env(list(a = 1)))
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
    get_code(q1),
    c("a <- 1\nb <- 2")
  )
  testthat::expect_equal(q1, list2env(list(a = 1, b = 2)))
})

testthat::test_that("eval_code fails with code not being language nor character", {
  msg <- "eval_code accepts code being language or character"
  testthat::expect_error(eval_code(qenv(), NULL), msg)
  testthat::expect_error(eval_code(qenv(), 1), msg)
  testthat::expect_error(eval_code(qenv(), list()), msg)
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

testthat::test_that("eval_code accepts calls containing only comments and empty spaces", {
  code <- "# comment
    \n\n# comment
    \n
  "
  testthat::expect_identical(get_code(eval_code(qenv(), code)), code)
})

testthat::test_that("eval_code does not treat := as an assignment operator", {
  code <- "
    x <- 'name'
    rlang::list2(!!x := 1)
  "
  q <- eval_code(qenv(), code)
  testthat::expect_identical(get_code(q), code)
})

# comments ----------
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
  testthat::expect_identical(get_code(q), code)
})

testthat::test_that("comments from the same line are associated with it's call", {
  code <- c("x <- 5", " y <- 4 # comment", "z <- 5")
  q <- eval_code(qenv(), code)
  testthat::expect_identical(as.character(q@code)[2], code[2])
})

testthat::test_that("comments passed alone to eval_code that contain @linksto tag have detected dependency", {
  code <- c("x <- 5", "# comment @linksto x")
  q <- eval_code(eval_code(qenv(), code[1]), code[2])
  testthat::expect_identical(
    get_code(q, names = "x"),
    paste(code, collapse = "\n")
  )
  testthat::expect_identical(
    attr(q@code[[2]], "dependency"),
    "x"
  )
})

testthat::test_that("object printed (explicitly) is stored as string in the 'outputs' attribute of a code element", {
  q <- eval_code(qenv(), "print('whatever')")
  testthat::expect_identical(attr(q@code[[1]], "outputs")[[1]], '[1] "whatever"\n')
})

testthat::test_that("object printed (implicitly) is stored asis in the 'outputs' attribute of a code element", {
  q <- eval_code(qenv(), "head(letters)")
  testthat::expect_identical(attr(q@code[[1]], "outputs")[[1]], head(letters))
})

testthat::test_that("plot output is stored as recordedplot in the 'outputs' attribute of a code element", {
  q <- eval_code(qenv(), "plot(1)")
  testthat::expect_s3_class(attr(q@code[[1]], "outputs")[[1]], "recordedplot")
})

testthat::test_that("plot cannot modified previous plots when calls are seperate", {
  q <- qenv()
  q1 <- eval_code(q, expression(plot(1:10)))

  q2 <- eval_code(q1, expression(title("A title")))
  testthat::expect_s3_class(q2, "qenv.error")
})
