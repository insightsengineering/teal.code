testthat::describe("qenv inherits from environment: ", {
  testthat::it("is an environment", {
    testthat::expect_true(is.environment(qenv()))
  })

  testthat::it("names() shows nothing on empty environment", {
    testthat::expect_identical(names(qenv()), character(0))
  })

  testthat::it("names() shows available objets", {
    q <- within(qenv(), iris <- iris)
    testthat::expect_setequal(names(q), "iris")
  })

  testthat::it("names() shows hidden objects", {
    q <- within(qenv(), {
      iris <- iris
      .hidden <- 2
    })
    testthat::expect_setequal(names(q), c("iris", ".hidden"))
  })

  testthat::it("ls() does not show hidden objects", {
    q <- within(qenv(), {
      iris <- iris
      .hidden <- 2
    })
    testthat::expect_setequal(ls(q), c("iris"))
  })

  testthat::it("ls(all.names = TRUE) show all objects", {
    q <- eval_code(qenv(), "
      iris <- iris
      .hidden <- 2
    ")
    testthat::expect_setequal(ls(q, all.names = TRUE), c("iris", ".hidden"))
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
  testthat::expect_identical(names(q), character(0))
  testthat::expect_identical(q@code, list())
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
