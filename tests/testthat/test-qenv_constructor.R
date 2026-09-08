describe("qenv inherits from environment: ", {
  it("is an environment", {
    expect_true(is.environment(qenv()))
  })

  it("names() shows nothing on empty environment", {
    expect_identical(names(qenv()), character(0))
  })

  it("names() shows available objets", {
    q <- within(qenv(), iris <- iris)
    expect_setequal(names(q), "iris")
  })

  it("names() shows hidden objects", {
    q <- within(qenv(), {
      iris <- iris
      .hidden <- 2
    })
    expect_setequal(names(q), c("iris", ".hidden"))
  })

  it("ls() does not show hidden objects", {
    q <- within(qenv(), {
      iris <- iris
      .hidden <- 2
    })
    expect_setequal(ls(q), c("iris"))
  })

  it("ls(all.names = TRUE) show all objects", {
    q <- eval_code(qenv(), "
      iris <- iris
      .hidden <- 2
    ")
    expect_setequal(ls(q, all.names = TRUE), c("iris", ".hidden"))
  })

  it("does not allow binding to be added", {
    q <- qenv()
    expect_error(q$x <- 1, "cannot add bindings to a locked environment")
  })

  it("does not allow binding to be modified", {
    q <- within(qenv(), obj <- 1)
    expect_error(q$obj <- 2, "cannot change value of locked binding for 'obj'")
  })
})

test_that("constructor returns qenv", {
  q <- qenv()
  expect_s4_class(q, "qenv")
  expect_identical(names(q), character(0))
  expect_identical(q@code, list())
})

describe("parent of qenv environment is the parent of .GlobalEnv", {
  it("via slot", {
    q <- qenv()
    expect_identical(parent.env(q@.xData), parent.env(.GlobalEnv))
  })

  it("via qenv directly", {
    q <- qenv()
    expect_identical(parent.env(q), parent.env(.GlobalEnv))
  })
})
