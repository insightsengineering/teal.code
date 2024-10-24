testthat::test_that("subset extract proper objects", {
  q <- qenv()
  code <- c("x<-1","a<-1;b<-2")
  q <- eval_code(q, code)
  object_names <- c("x", "a")
  qs <- subset(q, names = object_names)
  testthat::expect_true(all(ls(get_env(qs)) %in% object_names))
})

testthat::test_that("subset extract proper code", {
  q <- qenv()
  code <- c("x<-1","a<-1;b<-2")
  q <- eval_code(q, code)
  object_names <- c("x", "a")
  qs <- subset(q, names = object_names)
  testthat::expect_identical(
    qs@code,
    c("x <- 1", "a <- 1")
  )
})

testthat::test_that("subset preservers comments in the code", {
  q <- qenv()
  code <- c("x<-1 #comment","a<-1;b<-2")
  q <- eval_code(q, code)
  qs <- subset(q, names = c("x", "a"))
  testthat::expect_identical(
    qs@code,
    c("x <- 1 #comment", "a <- 1")
  )
})

testthat::test_that("subset extract proper elements of @id, @warnings and @messages fiels", {
  q <- qenv()
  code <-
    c("x<-1 #comment", "message('tiny message')", "a<-1;b<-2;warning('small warning')")
  q <- eval_code(q, code)
  qs <- subset(q, names = c("x", "a"))

  testthat::expect_identical(qs@id, q@id[c(1, 3)])
  testthat::expect_identical(qs@code, q@code[c(1, 3)])
  testthat::expect_identical(qs@warnings, q@warnings[c(1, 3)])
  testthat::expect_identical(qs@messages, q@messages[c(1, 3)])

})
