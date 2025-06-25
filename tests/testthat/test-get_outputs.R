testthat::describe("get_output", {
  testthat::it("returns an empty list if nothing is printed", {
    q <- qenv()
    q1 <- eval_code(q, expression(a <- 1L, b <- 2L))
    testthat::expect_identical(get_outputs(q1), list())
  })

  testthat::it("implicitly printed objects are returned asis in a list", {
    q <- qenv()
    q1 <- eval_code(q, expression(a <- 1L, a, b <- 2L, b))
    testthat::expect_identical(get_outputs(q1), list(1L, 2L))
  })

  testthat::it("explicitly printed objects are returned as console-output-string in a list", {
    q <- qenv()
    q1 <- eval_code(q, expression(a <- 1L, print(a), b <- 2L, print(b)))
    testthat::expect_identical(get_outputs(q1), list("[1] 1\n", "[1] 2\n"))
  })

  testthat::it("printed plots are returned as recordedplot in a list", {
    q <- qenv()
    q1 <- eval_code(q, expression(a <- 1L, plot(a)))
    testthat::expect_true(inherits(get_outputs(q1)[[1]], "recordedplot"))
  })

  testthat::it("warnings are returned asis in a list", {
    q <- qenv()
    q1 <- eval_code(q, expression(warning("test")))
    expected <- simpleWarning("test")
    expected["call"] <- NULL
    testthat::expect_identical(get_outputs(q1), list(expected))
  })

  testthat::it("messages are returned asis in a list", {
    q <- qenv()
    q1 <- eval_code(q, expression(message("test")))
    expected <- simpleMessage("test\n", call = quote(message("test")))
    testthat::expect_identical(get_outputs(q1), list(expected))
  })
  testthat::it("prints inside for are bundled together", {
    q <- within(qenv(), for (i in 1:3) print(i))
    testthat::expect_identical(get_outputs(q)[[1]], "[1] 1\n[1] 2\n[1] 3\n")
  })
})
