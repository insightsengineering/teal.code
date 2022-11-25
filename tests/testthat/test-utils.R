testthat::test_that("remove_enclosing_curly_braces errors if argument is not character", {
  testthat::expect_error(remove_enclosing_curly_braces(quote(x <- 1)), "Must be of type 'character")
})

testthat::test_that("remove_enclosing_curly_braces returns argument if it has length 0", {
  testthat::expect_equal(remove_enclosing_curly_braces(character(0)), character(0))
})

testthat::test_that("remove_enclosing_curly_braces only splits string on \n if no enclosing curly brackets", {
  testthat::expect_equal(remove_enclosing_curly_braces("abc"), "abc")
  testthat::expect_equal(remove_enclosing_curly_braces("abc\n    def\n "), c("abc", "    def", " "))
  testthat::expect_equal(remove_enclosing_curly_braces("{\nABC\n}A"), c("{", "ABC", "}A"))
  testthat::expect_equal(remove_enclosing_curly_braces("{\nABC\nDEF\n A  }"), c("{", "ABC", "DEF", " A  }"))
})

testthat::test_that("remove_enclosing_curly_braces removes enclosing curly brackets", {
  testthat::expect_equal(remove_enclosing_curly_braces("{\nA\n}"), "A")
  testthat::expect_equal(remove_enclosing_curly_braces("{  \nA\n}"), "A")
  testthat::expect_equal(remove_enclosing_curly_braces("{\nA\n}  "), "A")
  testthat::expect_equal(remove_enclosing_curly_braces("  {  \nA\n  }"), "A")
})

testthat::test_that("remove_enclosing_curly_braces concatenates input character vector", {
  testthat::expect_equal(remove_enclosing_curly_braces(c("ABC", "DEF")), c("ABC", "DEF"))
  testthat::expect_equal(remove_enclosing_curly_braces(c("{\n    ABC", "    DEF\n}")), c("ABC", "DEF"))
  testthat::expect_equal(remove_enclosing_curly_braces(c("{\n    ABC\n}", " DEF")), c("{", "    ABC", "}", " DEF"))
})

testthat::test_that(
  desc = "remove_enclosing_curly_braces containing enclosing brackets and only blank lines returns blank lines",
  code = {
    testthat::expect_equal(remove_enclosing_curly_braces("{\n\n\n}"), c("", ""))
    testthat::expect_equal(remove_enclosing_curly_braces(" {  \n\n  }  "), "")
  }
)

testthat::test_that("remove_enclosing_curly_braces removes 4 spaces from lines enclosed by brackets if they exist", {
  testthat::expect_equal(remove_enclosing_curly_braces("{\n    A\n}"), "A")
  testthat::expect_equal(
    remove_enclosing_curly_braces("{\nA\n B\n  C\n   D\n    E \n    F\n    \n}"),
    c("A", " B", "  C", "   D", "E ", "F", "")
  )
})

testthat::test_that("format expression concatenates results of remove_enclosing_curly_braces", {
  code_list <- list(
    quote("x <- 1"),
    quote({
      y <- 1
      z <- 1
    })
  )
  expect_equal(format_expression(code_list), c("x <- 1", "y <- 1", "z <- 1"))
})
