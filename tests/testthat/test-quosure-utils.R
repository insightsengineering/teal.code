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
