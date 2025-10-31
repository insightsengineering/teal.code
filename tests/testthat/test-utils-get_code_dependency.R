testthat::describe("get_code with single assignments inside an expression", {
  testthat::it("detects assign() function", {
    td <- qenv() |>
      within({
        for (i in 1:10) {
          assign("var1", iris)
        }
      })

    code_source <- "for (i in 1:10) {\n    assign(\"var1\", iris)\n}"

    testthat::expect_equal(get_code(td, names = "var1"), code_source)
  })

  testthat::it("detects <-", {
    td <- qenv() |>
      within({
        for (i in 1:10) {
          var1 <- iris
        }
      })

    code_source <- "for (i in 1:10) {\n    var1 <- iris\n}"

    testthat::expect_equal(get_code(td, names = "var1"), code_source)
  })

  testthat::it("detects ->", {
    td <- qenv() |>
      within({
        for (i in 1:10) {
          iris -> var1 # nolint: assignment.
        }
      })

    # Reversed order of operation
    code_source <- "for (i in 1:10) {\n    var1 <- iris\n}"

    testthat::expect_equal(get_code(td, names = "var1"), code_source)
  })
})

testthat::describe("get_code with multiple assignments inside an expression", {
  testthat::it("detects assign() function", {
    td <- qenv() |>
      within({
        for (i in 1:10) {
          assign("var1", iris)
          assign("var2", mtcars)
        }
      })

    code_source <- "for (i in 1:10) {\n    assign(\"var1\", iris)\n    assign(\"var2\", mtcars)\n}"

    testthat::expect_equal(get_code(td, names = "var1"), code_source)
    testthat::expect_equal(get_code(td, names = "var2"), code_source)
  })

  testthat::it("detects <- function", {
    td <- qenv() |>
      within({
        for (i in 1:10) {
          var1 <- iris
          var2 <- mtcars
        }
      })

    code_source <- "for (i in 1:10) {\n    var1 <- iris\n    var2 <- mtcars\n}"

    testthat::expect_equal(get_code(td, names = "var1"), code_source)
    testthat::expect_equal(get_code(td, names = "var2"), code_source)
  })

  testthat::it("detects -> function", {
    td <- qenv() |>
      within({
        for (i in 1:10) {
          iris -> var1 # nolint: assignment.
          mtcars -> var2 # nolint: assignment.
        }
      })

    code_source <- "for (i in 1:10) {\n    var1 <- iris\n    var2 <- mtcars\n}"

    testthat::expect_equal(get_code(td, names = "var1"), code_source)
    testthat::expect_equal(get_code(td, names = "var2"), code_source)
  })
})

testthat::describe("get_code with subassignments", {
  testthat::it("tracks [ subassignment as producing the base object", {
    td <- qenv() |>
      within({
        x <- 1:10
        x[1:3] <- c(10, 20, 30)
      })

    code_source <- "x <- 1:10\nx[1:3] <- c(10, 20, 30)"

    testthat::expect_equal(get_code(td, names = "x"), code_source)
  })

  testthat::it("tracks [[ subassignment as producing the base object", {
    td <- qenv() |>
      within({
        lst <- list(a = 1, b = 2)
        lst[["c"]] <- 3
      })

    code_source <- "lst <- list(a = 1, b = 2)\nlst[[\"c\"]] <- 3"

    testthat::expect_equal(get_code(td, names = "lst"), code_source)
  })

  testthat::it("tracks nested subassignments", {
    td <- qenv() |>
      within({
        df <- data.frame(x = 1:5, y = 6:10)
        df$x[df$y > 8] <- 99
      })

    code_source <- "df <- data.frame(x = 1:5, y = 6:10)\ndf$x[df$y > 8] <- 99"

    testthat::expect_equal(get_code(td, names = "df"), code_source)
  })

  testthat::it("tracks multiple subassignments to same object", {
    td <- qenv() |>
      within({
        iris <- iris
        iris$Species[sample.int(nrow(iris), 10)] <- NA
        iris$Sepal.Length[1:5] <- 0
      })

    code_source <- "iris <- iris\niris$Species[sample.int(nrow(iris), 10)] <- NA\niris$Sepal.Length[1:5] <- 0"

    testthat::expect_equal(get_code(td, names = "iris"), code_source)
  })

  testthat::it("tracks subassignments with complex expressions", {
    td <- qenv() |>
      within({
        mat <- matrix(1:12, nrow = 3)
        mat[mat > 5 & mat < 10] <- 0
      })

    code_source <- "mat <- matrix(1:12, nrow = 3)\nmat[mat > 5 & mat < 10] <- 0"

    testthat::expect_equal(get_code(td, names = "mat"), code_source)
  })

  testthat::it("tracks subassignments with function calls on LHS", {
    td <- qenv() |>
      within({
        lst <- list(a = 1, b = 2)
        names(lst)[1] <- "first"
      })

    code_source <- "lst <- list(a = 1, b = 2)\nnames(lst)[1] <- \"first\""

    testthat::expect_equal(get_code(td, names = "lst"), code_source)
  })

  testthat::it("tracks -> operator with subassignments", {
    td <- qenv() |>
      within({
        x <- 1:10
        c(10, 20, 30) -> x[1:3] # nolint: assignment.
      })

    code_source <- "x <- 1:10\nx[1:3] <- c(10, 20, 30)"

    testthat::expect_equal(get_code(td, names = "x"), code_source)
  })

  testthat::it("tracks attributes() function with subassignments", {
    td <- qenv() |>
      within({
        x <- 1:5
        attributes(x)$names <- letters[1:5]
      })

    code_source <- "x <- 1:5\nattributes(x)$names <- letters[1:5]"

    testthat::expect_equal(get_code(td, names = "x"), code_source)
  })

  testthat::it("handles complex nested subassignments", {
    td <- qenv() |>
      within({
        df <- data.frame(x = 1:5, y = 6:10)
        df[df$x > 2, "y"][1:2] <- c(99, 100)
      })

    code_source <- "df <- data.frame(x = 1:5, y = 6:10)\ndf[df$x > 2, \"y\"][1:2] <- c(99, 100)"

    testthat::expect_equal(get_code(td, names = "df"), code_source)
  })

  testthat::it("handles subassignments with multiple operators", {
    td <- qenv() |>
      within({
        lst <- list(a = list(b = 1, c = 2))
        lst$a$b[2] <- 99
      })

    code_source <- "lst <- list(a = list(b = 1, c = 2))\nlst$a$b[2] <- 99"

    testthat::expect_equal(get_code(td, names = "lst"), code_source)
  })

  testthat::it("handles subassignments with data frame column creation", {
    td <- qenv() |>
      within({
        df <- data.frame(x = 1:3)
        df$new_col <- c("a", "b", "c")
      })

    code_source <- "df <- data.frame(x = 1:3)\ndf$new_col <- c(\"a\", \"b\", \"c\")"

    testthat::expect_equal(get_code(td, names = "df"), code_source)
  })

  testthat::it("handles subassignments with matrix indexing", {
    td <- qenv() |>
      within({
        mat <- matrix(1:9, nrow = 3)
        mat[1:2, 2:3] <- matrix(0, nrow = 2, ncol = 2)
      })

    code_source <- "mat <- matrix(1:9, nrow = 3)\nmat[1:2, 2:3] <- matrix(0, nrow = 2, ncol = 2)"

    testthat::expect_equal(get_code(td, names = "mat"), code_source)
  })

  testthat::it("handles subassignments with logical indexing", {
    td <- qenv() |>
      within({
        vec <- 1:10
        vec[vec %% 2 == 0] <- vec[vec %% 2 == 0] * 2
      })

    code_source <- "vec <- 1:10\nvec[vec%%2 == 0] <- vec[vec%%2 == 0] * 2"

    testthat::expect_equal(get_code(td, names = "vec"), code_source)
  })
})
