testthat::test_that("get_code correctly handles function calls in assignments without false dependencies", {
  # Test reproducing the issue from GitHub #262 using iris dataset
  # Function calls on left side of assignments should not create false dependencies
  
  data_env <- qenv() |>
    within({
      # Create initial datasets
      ADSL <- iris
      ADMH <- mtcars
      ADVS <- iris
      
      # This assignment uses function calls on the left side
      # colnames(ADMH[c("mpg", "cyl")]) should not create dependency for ADVS
      colnames(ADMH[c("mpg", "cyl")]) <- c("Miles_Per_Gallon", "Cylinders")
      
      # ADVS should be independent of ADMH modifications
      ADVS <- cbind(ADVS, Species_Number = as.numeric(ADVS$Species))
    })
  
  # Get code for ADVS - should NOT include the ADMH colnames modification
  advs_code <- get_code(data_env, names = "ADVS")
  
  # ADVS code should not include the ADMH modification line
  testthat::expect_false(grepl("colnames\\\\(ADMH", advs_code)))
  
  # ADVS code should include its own definition and dependency on initial ADVS
  testthat::expect_true(grepl("ADVS <- iris", advs_code))
  
  testthat::expect_true(grepl("ADVS <- cbind", advs_code))
})

testthat::test_that("get_code correctly excludes unrelated function call assignments", {
  # Test that function calls like names(), class(), attr() don't create false dependencies
  
  data_env <- qenv() |>
    within({
      dataset_a <- iris[1:10, ]
      dataset_b <- mtcars[1:5, ]
      
      # Modify dataset_a attributes using function calls on left side
      names(dataset_a[c("Sepal.Length", "Sepal.Width")]) <- c("SL", "SW")
      class(dataset_a) <- c("custom_iris", class(dataset_a))
      
      # dataset_b should be independent of dataset_a modifications
      dataset_b$new_column <- dataset_b$mpg * 2
    })
  
  # Get code for dataset_b - should NOT include dataset_a modifications
  dataset_b_code <- get_code(data_env, names = "dataset_b")
  
  # Check that dataset_b code doesn't include dataset_a function call modifications
  testthat::expect_false(grepl("names\\\\(dataset_a", dataset_b_code)))
  testthat::expect_false(grepl("class\\\\(dataset_a", dataset_b_code)))
  
  # But should include its own definition and modifications
  testthat::expect_true(grepl("dataset_b <- mtcars", dataset_b_code))
  testthat::expect_true(grepl("dataset_b\\$new_column", dataset_b_code))
})

testthat::test_that("get_code handles complex function calls without creating circular dependencies", {
  # Test complex scenarios with nested function calls similar to the original issue
  
  data_env <- qenv() |>
    within({
      base_data <- iris
      processed_data <- mtcars
      final_data <- iris[1:5, ]
      
      # Complex assignment with nested function calls - should not affect final_data
      attr(processed_data[c("mpg", "hp")], "custom_attr") <- list(source = "mtcars", type = "numeric")
      
      # Another complex assignment with function calls
      levels(base_data$Species)[c(1, 2)] <- c("Type1", "Type2")
      
      # final_data should be independent of the above modifications
      final_data <- transform(final_data, Sepal.Sum = Sepal.Length + Sepal.Width)
    })
  
  # Get code for final_data
  final_data_code <- get_code(data_env, names = "final_data")
  
  # final_data should not include the complex function call assignments from other datasets
  testthat::expect_false(grepl("attr\\\\(processed_data", final_data_code)))
  testthat::expect_false(grepl("levels\\\\(base_data", final_data_code)))
  
  # But should include its own operations
  testthat::expect_true(grepl("final_data <- iris", final_data_code))
  testthat::expect_true(grepl("transform\\\\(final_data", final_data_code)))
})

testthat::test_that("get_code preserves function dependencies while avoiding false assignment targets", {
  # Test that functions are still tracked as dependencies but not as assignment targets
  
  data_env <- qenv() |>
    within({
      my_data <- iris
      helper_func <- function(x, cols) names(x)[cols]
      
      # Assignment that should depend on helper_func but not treat it as assignment target
      names(my_data[c(1, 2)]) <- helper_func(my_data, c(1, 2))
      
      # Create another object that uses my_data
      summary_data <- summary(my_data)
    })
  
  # Get code for summary_data - should include helper_func definition due to dependency
  summary_code <- get_code(data_env, names = "summary_data")
  
  # Should include helper_func since it's used in modifying my_data
  testthat::expect_true(grepl("helper_func <- function", summary_code))
  
  # Should include the names assignment that uses helper_func
  testthat::expect_true(grepl("names\\\\(my_data", summary_code)))
  
  # Should include my_data initial definition
  testthat::expect_true(grepl("my_data <- iris", summary_code))
})