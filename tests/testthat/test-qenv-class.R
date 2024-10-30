testthat::describe("methods::new(qenv)", {
  testthat::it("creates a locked environment", {
    expect_true(is.environment(methods::new("qenv")))
  })

  testthat::it("throws error when id and code length doesn't match", {
    expect_error(is.environment(methods::new("qenv", id = 1)))
  })

  testthat::it("throws error when .xData is not an environment", {
    expect_true(is.environment(methods::new("qenv")))
  })
})
