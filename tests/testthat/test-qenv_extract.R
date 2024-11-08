testthat::test_that("`[.` returns empty qenv for names not in qenv", {
  data <- within(qenv(), {
    x <- 1
    a <- 2
  })
  testthat::expect_warning(
    testthat::expect_equal(data["y"], qenv()),
    "None of 'names' exist in the environment of the 'qenv'. Returning empty 'qenv."
  )
})

testthat::test_that("`[.` returns limited qenv for some names not in qenv", {
  data <- within(qenv(), {
    x <- 1
    a <- 2
  })
  testthat::expect_warning(
    testthat::expect_equal(data[c("y", "a")], data["a"]),
    "Some elements of 'names' do not exist in the environment of the 'qenv'. Skipping those: y."
  )
})

testthat::test_that("`[.` limits code for some names not in code", {
  data <- within(qenv(), {
    x <- 1
    a <- 2
    rm(x)
  })
  testthat::expect_warning(
    testthat::expect_equal(data[c("a", "x")], data["a"]),
    "Some elements of 'names' do not exist in the environment of the 'qenv'. Skipping those: x."
  )
})


testthat::test_that("`[.` subsets environment and code to specified object names", {
  q <- qenv()
  code <- c("x<-1", "a<-1;b<-2")
  q <- eval_code(q, code)
  object_names <- c("x", "a")
  qs <- q[object_names]
  testthat::expect_true(all(ls(get_env(qs)) %in% object_names))
})

testthat::test_that("`[.` extracts the code only needed to recreate objects passed through 'names'", {
  q <- qenv()
  code <- c("x<-1", "a<-1;b<-2")
  q <- eval_code(q, code)
  object_names <- c("x", "a")
  qs <- q[object_names]
  testthat::expect_identical(
    unlist(qs@code),
    c("x<-1\n", "a<-1;")
  )
})

testthat::test_that("`[.` comments are preserved in the code and associated with the following call", {
  q <- qenv()
  code <- c("x<-1 #comment", "a<-1;b<-2")
  q <- eval_code(q, code)
  qs <- q[c("x", "a")]
  testthat::expect_identical(
    unlist(qs@code),
    c("x<-1 #comment\n", "a<-1;")
  )
})

testthat::test_that("`[.` extract proper elements of @id, @warnings and @messages fiels", {
  q <- qenv()
  code <-
    c("x<-1 #comment", "message('tiny message')", "a<-1;b<-2;warning('small warning')")
  q <- eval_code(q, code)
  qs <- q[c("x", "a")]

  testthat::expect_identical(get_code_attr(qs, "id"), get_code_attr(q, "id")[c(1, 3)])
  testthat::expect_identical(unlist(qs@code), unlist(q@code[c(1, 3)]))
  testthat::expect_null(get_code_attr(qs, "warning"))
  testthat::expect_null(get_code_attr(qs, "message"))
})
