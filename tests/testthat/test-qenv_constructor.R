testthat::test_that("constructor returns qenv", {
  q <- new_qenv()
  testthat::expect_s4_class(q, "qenv")
  testthat::expect_identical(ls(q@env), character(0))
  testthat::expect_identical(q@code, character(0))
  testthat::expect_identical(q@id, integer(0))
  testthat::expect_identical(q@warnings, character(0))
  testthat::expect_identical(q@messages, character(0))
})

testthat::test_that("parent of qenv environment is the parent of .GlobalEnv", {
  q <- new_qenv()
  testthat::expect_identical(parent.env(q@env), parent.env(.GlobalEnv))
})
