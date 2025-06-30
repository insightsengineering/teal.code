testthat::describe("get_output", {
  testthat::it("returns an empty list if nothing is printed", {
    q <- qenv()
    q1 <- eval_code(q, expression(a <- 1L, b <- 2L))
    testthat::expect_identical(get_outputs(q1), list())
  })

  testthat::it("implicitly printed objects are returned asis in a list and are identical to ones in the environment", {
    q <- qenv()
    q1 <- eval_code(
      q,
      expression(
        a <- 1L, a,
        b <- structure(list(aa = list(aaa = "aaa")), class = "class_to_break"), b
      )
    )
    testthat::expect_identical(get_outputs(q1), unname(as.list(q1)))
    testthat::expect_true(rlang::is_reference(get_outputs(q1)[[1]], q1$a))
    testthat::expect_true(rlang::is_reference(get_outputs(q1)[[2]], q1$b))
  })

  # it cannot have a package prefix here until upstream bug in testthat is solved
  it("implicitly printed S4 object is returned asis in a list and identical to the one in the environment", {
    methods::setClass("NewS4Class", slots = list(value = "numeric"))
    withr::defer(removeClass("NewS4Class"))
    q <- qenv()
    q1 <- eval_code(
      q,
      expression(
        new_obj <- methods::new("NewS4Class", value = 42),
        new_obj
      )
    )
    testthat::expect_identical(get_outputs(q1), unname(as.list(q1)))
    testthat::expect_true(rlang::is_reference(get_outputs(q1)[[1]], q1$new_obj))
    testthat::expect_s4_class(get_outputs(q1)[[1]], "NewS4Class")
  })

  testthat::it("implicitly printed list is returned asis even if its print is overridden", {
    q <- qenv()
    q1 <- eval_code(
      q,
      expression(
        print.test_class <- function(x, ...) {
          print("test_print")
          invisible(NULL)
        },
        b <- structure(list("test"), class = "test_class"),
        b
      )
    )
    testthat::expect_identical(get_outputs(q1), list(q1$b))
  })

  testthat::it("explicitly printed objects are returned as console-output-string in a list", {
    q <- qenv()
    q1 <- eval_code(q, expression(a <- 1L, print(a), b <- 2L, print(b)))
    testthat::expect_identical(get_outputs(q1), list("[1] 1\n", "[1] 2\n"))
  })

  testthat::it("explicitly printed object uses newly registered print method and returned as console-output-string", {
    q <- qenv()
    q1 <- eval_code(
      q,
      expression(
        print.test_class <- function(x, ...) {
          print("test_print")
          invisible(NULL)
        },
        b <- structure(list("test"), class = "test_class"),
        print(b)
      )
    )
    testthat::expect_identical(get_outputs(q1), list("[1] \"test_print\"\n"))
  })

  testthat::it("printed plots are returned as recordedplot in a list (1)", {
    q <- qenv()
    q1 <- eval_code(q, expression(a <- 1L, plot(a)))
    testthat::expect_true(inherits(get_outputs(q1)[[1]], "recordedplot"))
  })

  testthat::it("printed plots are returned as recordedplot in a list (2)", {
    q <- qenv()
    q1 <- eval_code(q, expression(a <- seq_len(10L), hist(a)))
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

  testthat::it("intermediate plots are not kept", {
    q <- qenv()
    q1 <- eval_code(q, expression(plot(1:10), title("A title")))
    testthat::expect_length(get_outputs(q1), 1)
  })
})
