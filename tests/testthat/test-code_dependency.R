testthat::test_that("get_code extract code of a binding from a 'simplest' code", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- 2")

  testthat::expect_identical(
    get_code(q, deparse = FALSE, name = "a"),
    "a <- 1"
  )
  testthat::expect_identical(
    get_code(q, deparse = FALSE, name = "b"),
    "b <- 2"
  )
})

testthat::test_that("get_code warns if binding doesn't exist in a code", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- 2")

  testthat::expect_warning(
    # TODO: throw a warning if name is missing
    get_code(q, deparse = FALSE, name = "c")
  )
})


testthat::test_that("get_code extract code of a parent binding but only those evaluated before coocurence", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- a")
  q <- eval_code(q, "a <- 2")

  testthat::expect_identical(
    get_code(q, deparse = FALSE, name = "b"),
    c("a <- 1", "b <- a")
  )
})

testthat::test_that("get_code extract code of a parent binding if used in a function", {
  q <- new_qenv()
  q <- eval_code(q, "a <- 1")
  q <- eval_code(q, "b <- identity(x = a)")
  q <- eval_code(q, "a <- 2")

  testthat::expect_identical(
    get_code(q, deparse = FALSE, name = "b"),
    c("a <- 1", "b <- identity(x = a)")
  )
})

testthat::test_that("can't determine effect when using assign", {

})

testthat::test_that("can't determine effect when using eval", {

})

testthat::test_that("@effect cause to return this line for affected binding", {

})
