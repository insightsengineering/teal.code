testthat::test_that("`[.` warns and subsets to empty if all names not present in env nor code", {
  data <- within(qenv(), {
    a <- 1
    b <- 2
  })
  testthat::expect_warning(
    testthat::expect_equal(data[c("y", "z")], qenv()),
    "Object\\(s\\) not found in code: y, z."
  )
  testthat::expect_warning(
    testthat::expect_equal(data[c("y", "z")], qenv()),
    "None of 'names' exist in the environment of the 'qenv'. Returning empty 'qenv."
  )
})

testthat::test_that("`[.` warns and subsets to empty if all names not present in env", {
  data <- within(qenv(), {
    a <- 1
    b <- 2
    c <- 3
    rm(b, c)
  })
  testthat::expect_warning(
    testthat::expect_equal(data[c("b", "c")], qenv()),
    "None of 'names' exist in the environment of the 'qenv'. Returning empty 'qenv'."
  )
})

testthat::test_that("`[.` warns and subsets to existing if some names not present in env and code", {
  data <- within(qenv(), {
    a <- 1
    b <- 2
  })
  testthat::expect_warning(
    testthat::expect_equal(data[c("b", "c", "d")], data["b"]),
    "Some 'names' do not exist in the environment of the 'qenv'. Skipping those: c, d."
  )
  testthat::expect_warning(
    testthat::expect_equal(data[c("b", "c", "d")], data["b"]),
    "Object\\(s\\) not found in code: c, d."
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
