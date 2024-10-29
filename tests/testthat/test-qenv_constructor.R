testthat::describe("qenv inherits from environment: ", {
  testthat::it("is an environment", {
    testthat::expect_true(is.environment(qenv()))
  })

  testthat::it("ls() shows nothing on empty environment", {
    testthat::expect_identical(ls(qenv(), all.names = TRUE), character(0))
  })

  testthat::it("ls() shows available objets", {
    q <- within(qenv(), iris <- iris)
    testthat::expect_setequal(ls(q), "iris")
  })

  testthat::it("ls() does not show hidden objects", {
    q <- within(qenv(), {
      iris <- iris
      .hidden <- 2
    })
    testthat::expect_setequal(ls(q), "iris")
  })

  testthat::it("names() show all objects", {
    q <- eval_code(qenv(), "
      iris <- iris
      .hidden <- 2
    ")
    testthat::expect_setequal(names(q), c("iris", ".hidden"))
  })

  testthat::it("does not allow binding to be added", {
    q <- qenv()
    testthat::expect_error(q$x <- 1, "cannot add bindings to a locked environment")
  })

  testthat::it("does not allow binding to be modified", {
    q <- within(qenv(), obj <- 1)
    testthat::expect_error(q$obj <- 2, "cannot change value of locked binding for 'obj'")
  })
})

testthat::test_that("constructor returns qenv", {
  q <- qenv()
  testthat::expect_s4_class(q, "qenv")
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

testthat::test_that("qenv environment is locked", {
  q <- qenv()
  testthat::expect_error(q@.xData$x <- 1, "cannot add bindings to a locked environment")
})
