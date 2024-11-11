testthat::describe("methods::new(qenv)", {
  testthat::it("creates a locked environment", {
    expect_true(environmentIsLocked(as.environment(methods::new("qenv"))))
  })

  testthat::it("creates a locked environment when .xData is manually defined", {
    new_env <- new.env()
    expect_false(environmentIsLocked(new_env))

    expect_true(environmentIsLocked(as.environment(methods::new("qenv", .xData = new_env))))
  })

  testthat::it("throws error when .xData is not an environment", {
    expect_error(methods::new("qenv", .xData = 2), "Must be an environment, not 'double'\\.")
  })

  testthat::it("initialized qenv(s) have different environments", {
    testthat::expect_false(identical(qenv()@.xData, qenv()@.xData))
  })
})
