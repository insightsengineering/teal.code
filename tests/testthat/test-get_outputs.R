describe("get_output", {
  it("returns an empty list if nothing is printed", {
    q <- qenv()
    q1 <- eval_code(q, expression(a <- 1L, b <- 2L))
    expect_identical(get_outputs(q1), list())
  })

  it("implicitly printed objects are returned asis in a list and are identical to ones in the environment", {
    q <- qenv()
    q1 <- eval_code(
      q,
      expression(
        a <- 1L, a,
        b <- structure(list(aa = list(aaa = "aaa")), class = "class_to_break"), b
      )
    )
    expect_identical(get_outputs(q1), unname(as.list(q1)))
    expect_true(rlang::is_reference(get_outputs(q1)[[1]], q1$a))
    expect_true(rlang::is_reference(get_outputs(q1)[[2]], q1$b))
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
    expect_identical(get_outputs(q1), unname(as.list(q1)))
    expect_true(rlang::is_reference(get_outputs(q1)[[1]], q1$new_obj))
    expect_s4_class(get_outputs(q1)[[1]], "NewS4Class")
  })

  it("implicitly printed list is returned asis even if its print is overridden", {
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
    expect_identical(get_outputs(q1), list(q1$b))
  })

  it("explicitly printed objects are returned as console-output-string in a list", {
    q <- qenv()
    q1 <- eval_code(q, expression(a <- 1L, print(a), b <- 2L, print(b)))
    expect_identical(get_outputs(q1), list("[1] 1\n", "[1] 2\n"))
  })

  it("explicitly printed object uses newly registered print method and returned as console-output-string", {
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
    expect_identical(get_outputs(q1), list("[1] \"test_print\"\n"))
  })

  it("printed plots are returned as recordedplot in a list (1)", {
    q <- qenv()
    q1 <- eval_code(q, expression(a <- 1L, plot(a)))
    expect_true(inherits(get_outputs(q1)[[1]], "recordedplot"))
  })

  it("printed plots are returned as recordedplot in a list (2)", {
    q <- qenv()
    q1 <- eval_code(q, expression(a <- seq_len(10L), hist(a)))
    expect_true(inherits(get_outputs(q1)[[1]], "recordedplot"))
  })

  it("warnings are returned asis in a list", {
    q <- qenv()
    q1 <- eval_code(q, expression(warning("test")))
    expected <- simpleWarning("test")
    expected["call"] <- NULL
    expect_identical(get_outputs(q1), list(expected))
  })

  it("messages are returned asis in a list", {
    q <- qenv()
    q1 <- eval_code(q, expression(message("test")))
    expected <- simpleMessage("test\n", call = quote(message("test")))
    expect_identical(get_outputs(q1), list(expected))
  })

  it("prints inside for are bundled together", {
    q <- within(qenv(), for (i in 1:3) print(i))
    expect_identical(get_outputs(q)[[1]], "[1] 1\n[1] 2\n[1] 3\n")
  })

  it("intermediate plots are not kept", {
    q <- qenv()
    q1 <- eval_code(q, expression(plot(1:10), title("A title")))
    expect_length(get_outputs(q1), 1)
  })
})
