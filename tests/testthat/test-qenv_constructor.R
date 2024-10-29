testthat::test_that("constructor returns qenv that inherits from environment", {
  testthat::expect_true(is.environment(qenv()))
})

testthat::test_that("constructor returns qenv", {
  q <- qenv()
  testthat::expect_s4_class(q, "qenv")
  testthat::expect_identical(ls(q, all.names = TRUE), character(0))
  testthat::expect_identical(ls(q@.xData, all.names = TRUE), character(0))
  testthat::expect_identical(q@code, character(0))
  testthat::expect_identical(q@id, integer(0))
  testthat::expect_identical(q@warnings, character(0))
  testthat::expect_identical(q@messages, character(0))
})

testthat::describe("parent of qenv environment is the parent of .GlobalEnv", {
  testthat::it("via slot", {
    q <- qenv()
    testthat::expect_identical(parent.env(q@.xData), parent.env(.GlobalEnv))
  })

  testthat::it("via qenv directly", {
    q <- qenv()
    testthat::expect_identical(parent.env(q), parent.env(.GlobalEnv))
  })
})

testthat::describe("qenv environment is locked", {
  testthat::it("via slot", {
    q <- qenv()
    testthat::expect_error(q@.xData$x <- 1, "cannot add bindings to a locked environment")
  })

  testthat::it("via qenv directly", {
    q <- qenv()
    testthat::expect_error(q$x <- 1, "cannot add bindings to a locked environment")
  })
})
