testthat::test_that("function calls should not appear on left side of assignment in dependencies", {
  # Test for the specific issue reported in GitHub issue #262
  
  # Create a minimal example that reproduces the problem
  code1 <- 'ADMH[c("test")] <- c("value")'
  
  # Parse and extract dependencies
  parsed1 <- parse(text = code1, keep.source = TRUE)
  
  # Extract dependencies using the internal function
  deps1 <- teal.code:::extract_dependency(parsed1)
  
  # The issue: function 'c' should not appear on left side of assignment
  # In deps1, 'c' appears before '<-', which is incorrect
  assign_pos1 <- which(deps1 == "<-")
  left_side1 <- if(length(assign_pos1) > 0) deps1[seq_len(assign_pos1[1] - 1)] else character(0)
  
  # Function calls should NOT appear on the left side of assignment
  testthat::expect_false("c" %in% left_side1, 
    info = "Function 'c' should not appear on left side of assignment in dependencies")
  
  # Only actual variables/objects should appear on left side
  # In this case, only 'ADMH' should be on the left side
  testthat::expect_true("ADMH" %in% left_side1,
    info = "Variable 'ADMH' should appear on left side as it's being assigned to")
})

testthat::test_that("complex assignment with function calls handles dependencies correctly", {
  # This test reproduces the exact scenario from the GitHub issue
  
  code_admh <- 'teal.data::col_labels(ADMH[c("MHDISTAT")]) <- c("Status of Disease")'
  
  parsed_admh <- parse(text = code_admh, keep.source = TRUE)
  
  deps_admh <- teal.code:::extract_dependency(parsed_admh)
  
  # Check ADMH dependencies
  assign_pos_admh <- which(deps_admh == "<-")
  left_side_admh <- if(length(assign_pos_admh) > 0) deps_admh[seq_len(assign_pos_admh[1] - 1)] else character(0)
  right_side_admh <- if(length(assign_pos_admh) > 0) deps_admh[seq(assign_pos_admh[1] + 1, length(deps_admh))] else character(0)
  
  # Function calls should not be on left side of assignment
  testthat::expect_false("c" %in% left_side_admh,
    info = "Function 'c' should not be on left side in ADMH dependencies")
  testthat::expect_false("col_labels" %in% left_side_admh,
    info = "Function 'col_labels' should not be on left side in ADMH dependencies")
  
  # Functions can be on right side as dependencies
  testthat::expect_true("c" %in% right_side_admh,
    info = "Function 'c' can appear on right side as a dependency")
    
  # Variables being modified should be on left side
  testthat::expect_true("ADMH" %in% left_side_admh,
    info = "Variable 'ADMH' should be on left side as it's being modified")
})

testthat::test_that("function calls in complex expressions are handled correctly", {
  # Test various complex expressions to ensure function calls don't appear on left side
  
  test_cases <- list(
    'result <- fun(a, b)',
    'data[filter(x)] <- transform(y)', 
    'obj$method(params) <- compute(values)'
  )
  
  for (test_code in test_cases) {
    parsed_code <- parse(text = test_code, keep.source = TRUE)
    deps <- teal.code:::extract_dependency(parsed_code)
    
    assign_pos <- which(deps == "<-")
    if (length(assign_pos) > 0) {
      left_side <- deps[seq_len(assign_pos[1] - 1)]
      
      # No function calls should appear on left side 
      function_names <- c("fun", "filter", "transform", "method", "compute")
      for (func_name in function_names) {
        testthat::expect_false(func_name %in% left_side,
          info = paste("Function", func_name, "should not appear on left side in:", test_code))
      }
    }
  }
})