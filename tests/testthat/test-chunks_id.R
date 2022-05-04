set.seed(1234)
x <- chunks$new()
testthat::test_that("chunks id - push", {
  x$push(id = "test_1", x = call("print", 1))
  x$push(x = call("print", 2))
  x$push(id = "test_3", x = call("print", 3))
  testthat::expect_identical(names(x$get_rcode()), c("test_1", "chunk_2", "test_3"))
})

y <- chunks$new()
testthat::test_that("chunks id - push_chunks", {
  y$push(id = "test_1", x = call("print", 1))
  y$push_chunks(x)
  testthat::expect_warning(
    y$push(id = "test_1", x = call("print", 1)),
    "push\\(\\) cannot be used to change chunks"
  )
  y$push(id = "test_12", x = call("print", 1))
  testthat::expect_identical(
    names(y$get_rcode()),
    c("test_1", "test_1_PZVEL", "chunk_2", "test_3", "test_12")
  )
})

testthat::test_that("chunks id - push_chunks empty", {
  x <- chunks$new()
  y <- chunks$new()
  y$push_chunks(x)
  testthat::expect_identical(names(y$get_rcode()), character(0))
})
