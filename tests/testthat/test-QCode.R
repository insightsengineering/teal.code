testthat::test_that("initialize empty Qcode", {
  cq1 <- new_qcode()
  testthat::expect_s4_class(cq1, "QCode")
  testthat::expect_identical(cq1@code, character(0))
  testthat::expect_identical(cq1@id, integer(0))
})

testthat::test_that("Qcode can be initialized by the unnamed vector which is stored asid in @code", {
  cq1 <- new_qcode(code = c("a <- 1", "b <- 2"))
  testthat::expect_s4_class(cq1, "QCode")
  testthat::expect_identical(cq1@code, c("a <- 1", "b <- 2"))
  testthat::expect_true(checkmate::test_integer(cq1@id, len = 2))
})

testthat::test_that("Qcode can be initialized by the named vector which is stored asis in @code", {
  cq1 <- new_qcode(code = c(a = "a <- 1", b = "b <- 2"))
  testthat::expect_s4_class(cq1, "QCode")
  testthat::expect_identical(cq1@code, c(a = "a <- 1", b = "b <- 2"))
  testthat::expect_true(checkmate::test_integer(cq1@id, len = 2))
})

testthat::test_that("Duplicated names in the character vector are changed to unique", {
  cq1 <- new_qcode(code = c(a = "a <- 1", a = "b <- 2"))
  testthat::expect_s4_class(cq1, "QCode")
  testthat::expect_identical(cq1@code, c(a = "a <- 1", a.1 = "b <- 2"))
  testthat::expect_true(checkmate::test_integer(cq1@id, len = 2))
})

testthat::test_that("Qcode objects are mergeable if they don't share any code (identified by id)", {
  cq1 <- new_qcode(code = "a1 <- 1")
  cq2 <- new_qcode(code = "a1 <- 1")
  testthat::expect_true(check_joinable(cq1, cq2))

  cq <- join(cq1, cq2)
  testthat::expect_s4_class(cq, "QCode")
  testthat::expect_identical(cq@code, c("a1 <- 1", "a1 <- 1"))
  testthat::expect_identical(cq@id, c(cq1@id, cq2@id))
})

testthat::test_that("Qcode objects are mergeable if they share common initial code", {
  cq1 <- new_qcode(code = "a1 <- 1")
  cq2 <- join(cq1, "b1 <- 2")
  cq1 <- join(cq1, "a2 <- 3")
  testthat::expect_true(check_joinable(cq1, cq2))

  cq <- join(cq1, cq2)
  testthat::expect_s4_class(cq, "QCode")
  testthat::expect_identical(cq@code, c("a1 <- 1", "a2 <- 3", "b1 <- 2"))
  testthat::expect_identical(cq@id, union(cq1@id, cq2@id))
})

testthat::test_that("Qcode objects aren't mergeable if they share common code proceeded with some other code", {
  cq_common <- new_qcode("c1 <- 3")
  cq1 <- new_qcode(code = "a1 <- 1")
  cq1 <- join(cq1, cq_common)
  cq2 <- new_qcode(code = "b1 <- 2")
  cq2 <- join(cq2, cq_common)
  testthat::expect_match(check_joinable(cq1, cq2), "start from index = 1")
  testthat::expect_error(join(cq1, cq2), "start from index = 1")
})

testthat::test_that("Qcode objects are not mergable if they have multiple common streaks", {
  cq_common1 <- new_qcode(code = c("c1 <- 1"))
  cq_common2 <- new_qcode(code = c("c2 <- 2"))

  cq1 <- join(cq_common1, "a1 <- 3")
  cq1 <- join(cq1, cq_common2)        # c1, a1, c2
  cq2 <- join(cq_common1, cq_common2) # c1, c2

  testthat::expect_match(check_joinable(cq1, cq2), "doesn't have the same indices")
  testthat::expect_error(join(cq1, cq2), "doesn't have the same indices")
})

testthat::test_that(".keep_code_names_unique changes if there are any duplicate names in the combined vector", {
  testthat::expect_identical(
    .keep_code_name_unique(character(0), character(0)),
    character(0)
  )
  testthat::expect_identical(
    .keep_code_name_unique("a", "b"),
    c("a", "b")
  )
  testthat::expect_identical(
    .keep_code_name_unique(c("a", "b"), "b"),
    c("a", "b", "b")
  )
  testthat::expect_identical(
    .keep_code_name_unique(c(a = "a", a = "b")),
    c(a = "a", a.1 = "b")
  )
})
