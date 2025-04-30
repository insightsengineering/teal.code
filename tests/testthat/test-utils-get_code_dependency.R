testthat::describe("detects multiple assignments in single line", {
  testthat::it("<-", {
    # testthat::skip("Not implemented")
    code <- "var1 <- var2 <- 1"
    td <- eval_code(qenv(), code)

    testthat::expect_identical(get_code(td, names = "var1"), code)
    testthat::expect_identical(get_code(td, names = "var2"), code)
  })

  testthat::it("->", {
    # testthat::skip("Not implemented")
    code <- "1 -> var1 -> var2"
    td <- eval_code(qenv(), code)

    testthat::expect_identical(
      get_code(td, names = "var1"),
      as.character(parse(text = code, keep.source = TRUE))
    )

    testthat::expect_identical(get_code(td, names = "var1"), code)
    testthat::expect_identical(get_code(td, names = "var2"), code)
  })

  testthat::it("<- and ->", {
    # testthat::skip("Not implemented")
    code <- "var1 <- 1 -> var2"
    td <- eval_code(qenv(), code)

    testthat::expect_identical(get_code(td, names = "var1"), code)
    testthat::expect_identical(get_code(td, names = "var2"), code)
  })
})

testthat::describe("get_code with single assignments inside an expression", {
  testthat::it("detects assign() function", {
    td <- qenv() |>
      within({
        for (i in 1:10) {
          assign("var1", iris)
        }
      })

    code_source <- "for (i in 1:10) {\n    assign(\"var1\", iris)\n}"

    testthat::expect_equal(get_code(td, names = "var1"), code_source)
  })

  testthat::it("detects <-", {
    td <- qenv() |>
      within({
        for (i in 1:10) {
          var1 <- iris
        }
      })

    code_source <- "for (i in 1:10) {\n    var1 <- iris\n}"

    testthat::expect_equal(get_code(td, names = "var1"), code_source)
  })

  testthat::it("detects ->", {
    td <- qenv() |>
      within({
        for (i in 1:10) {
          iris -> var1
        }
      })

    # Reversed order of operation
    code_source <- "for (i in 1:10) {\n    var1 <- iris\n}"

    testthat::expect_equal(get_code(td, names = "var1"), code_source)
  })
})

testthat::describe("get_code with multiple assignments inside an expression", {
  testthat::it("detects assign() function", {
    td <- qenv() |>
      within({
        for (i in 1:10) {
          assign("var1", iris)
          assign("var2", mtcars)
        }
      })

    code_source <- "for (i in 1:10) {\n    assign(\"var1\", iris)\n    assign(\"var2\", mtcars)\n}"

    testthat::expect_equal(get_code(td, names = "var1"), code_source)
    testthat::expect_equal(get_code(td, names = "var2"), code_source)
  })

  testthat::it("detects <- function", {
    td <- qenv() |>
      within({
        for (i in 1:10) {
          var1 <- iris
          var2 <- mtcars
        }
      })

    code_source <- "for (i in 1:10) {\n    var1 <- iris\n    var2 <- mtcars\n}"

    testthat::expect_equal(get_code(td, names = "var1"), code_source)
    testthat::expect_equal(get_code(td, names = "var2"), code_source)
  })

  testthat::it("detects -> function", {
    td <- qenv() |>
      within({
        for (i in 1:10) {
          iris -> var1
          mtcars -> var2
        }
      })

    code_source <- "for (i in 1:10) {\n    var1 <- iris\n    var2 <- mtcars\n}"

    testthat::expect_equal(get_code(td, names = "var1"), code_source)
    testthat::expect_equal(get_code(td, names = "var2"), code_source)
  })
})
