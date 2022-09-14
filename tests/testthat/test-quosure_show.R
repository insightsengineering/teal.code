testthat::test_that("showing an empty quosure states quosure is empty" , {
  q <- new_quosure()
  testthat::expect_output(show(q), "A quosure object containing no objects")
})

testthat::test_that("showing a non-empty quosure lists its contents" , {
  q <- new_quosure() %>% eval_code("x <- 1") %>% eval_code("y <- 2")
  testthat::expect_output(show(q), "A quosure object containing: x, y")
})
