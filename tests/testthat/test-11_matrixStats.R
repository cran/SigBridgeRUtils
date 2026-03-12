# test-11_matrixStats.R - Tests for matrix statistics functions

test_that("rowMeans3 works correctly", {
  # Basic test
  m <- matrix(1:9, 3, 3)
  result <- rowMeans3(m)
  expected <- rowMeans(m)
  expect_equal(result, expected)

  # Test with NA values
  m_na <- m
  m_na[1, 1] <- NA
  result_na <- rowMeans3(m_na, na.rm = TRUE)
  expected_na <- rowMeans(m_na, na.rm = TRUE)
  expect_equal(result_na, expected_na)

  # Test with all NA row
  m_all_na <- matrix(NA, 2, 3)
  result_all_na <- rowMeans3(m_all_na, na.rm = TRUE)
  expect_true(all(is.nan(result_all_na)))
})

test_that("colMeans3 works correctly", {
  # Basic test
  m <- matrix(1:9, 3, 3)
  result <- colMeans3(m)
  expected <- colMeans(m)
  expect_equal(result, expected)

  # Test with NA values
  m_na <- m
  m_na[1, 1] <- NA
  result_na <- colMeans3(m_na, na.rm = TRUE)
  expected_na <- colMeans(m_na, na.rm = TRUE)
  expect_equal(result_na, expected_na)
})

test_that("rowVars3 works correctly", {
  # Basic test
  set.seed(123)
  m <- matrix(rnorm(15), 3, 5)
  result <- rowVars3(m)

  # Manual calculation
  manual_result <- apply(m, 1, var)
  expect_equal(result, manual_result, tolerance = 1e-10)

  # Test with NA values
  m_na <- m
  m_na[1, 2] <- NA
  result_na <- rowVars3(m_na, na.rm = TRUE)
  manual_na <- apply(m_na, 1, var, na.rm = TRUE)
  expect_equal(result_na, manual_na, tolerance = 1e-10)

  # Test with constant row (variance = 0)
  m_const <- matrix(rep(1:3, each = 4), 3, 4, byrow = TRUE)
  result_const <- rowVars3(m_const)
  expect_true(all(result_const == 0))
})

test_that("colVars3 works correctly", {
  # Basic test
  set.seed(123)
  m <- matrix(rnorm(15), 3, 5)
  result <- colVars3(m)

  # Manual calculation
  manual_result <- apply(m, 2, var)
  expect_equal(result, manual_result, tolerance = 1e-10)

  # Test with NA values
  m_na <- m
  m_na[1, 2] <- NA
  result_na <- colVars3(m_na, na.rm = TRUE)
  manual_na <- apply(m_na, 2, var, na.rm = TRUE)
  expect_equal(result_na, manual_na, tolerance = 1e-10)

  # Test with constant column (variance = 0)
  m_const <- matrix(rep(1:5, each = 3), 3, 5)
  result_const <- colVars3(m_const)
  expect_true(all(result_const == 0))
})

test_that("rowSds3 works correctly", {
  # Basic test
  set.seed(123)
  m <- matrix(rnorm(15), 3, 5)
  result <- rowSds3(m)

  # Manual calculation (sqrt of variance)
  manual_result <- sqrt(apply(m, 1, var))
  expect_equal(result, manual_result, tolerance = 1e-10)

  # Test with NA values
  m_na <- m
  m_na[1, 2] <- NA
  result_na <- rowSds3(m_na, na.rm = TRUE)
  manual_na <- sqrt(apply(m_na, 1, var, na.rm = TRUE))
  expect_equal(result_na, manual_na, tolerance = 1e-10)
})

test_that("colSds3 works correctly", {
  # Basic test
  set.seed(123)
  m <- matrix(rnorm(15), 3, 5)
  result <- colSds3(m)

  # Manual calculation (sqrt of variance)
  manual_result <- sqrt(apply(m, 2, var))
  expect_equal(result, manual_result, tolerance = 1e-10)

  # Test with NA values
  m_na <- m
  m_na[1, 2] <- NA
  result_na <- colSds3(m_na, na.rm = TRUE)
  manual_na <- sqrt(apply(m_na, 2, var, na.rm = TRUE))
  expect_equal(result_na, manual_na, tolerance = 1e-10)
})

test_that("colQuantiles3 works correctly", {
  # Basic test with default probs
  set.seed(123)
  m <- matrix(rnorm(20), 4, 5)
  result <- colQuantiles3(m)

  # Check dimensions
  expect_equal(dim(result), c(5, 5)) # 5 quantiles, 5 columns

  # Test with custom probabilities
  custom_probs <- c(0.25, 0.75)
  result_custom <- colQuantiles3(m, probs = custom_probs)
  expect_equal(dim(result_custom), c(5, 2))

  # Test with single probability
  result_single <- colQuantiles3(m, probs = 0.5)
  expect_equal(length(result_single), 5)
})

test_that("matrixStats functions handle edge cases", {
  # Empty matrix
  empty_m <- matrix(numeric(0), 0, 0)
  expect_length(rowMeans3(empty_m), 0)
  expect_length(colMeans3(empty_m), 0)

  # Single element matrix
  single_m <- matrix(42)
  expect_equal(rowMeans3(single_m), 42)
  expect_equal(colMeans3(single_m), 42)
  expect_equal(rowVars3(single_m), NA_real_) # variance of single value is NA
  expect_equal(colVars3(single_m), NA_real_)

  # Single row/column
  single_row <- matrix(1:5, 1, 5)
  expect_equal(rowMeans3(single_row), mean(1:5))
  expect_equal(length(colMeans3(single_row)), 5)

  single_col <- matrix(1:5, 5, 1)
  expect_equal(length(rowMeans3(single_col)), 5)
  expect_equal(colMeans3(single_col), mean(1:5))
})

test_that("matrixStats functions handle different data types", {
  # Integer matrix
  int_m <- matrix(1:9, 3, 3)
  expect_type(rowMeans3(int_m), "double")
  expect_type(colMeans3(int_m), "double")

  # Numeric matrix
  num_m <- matrix(as.numeric(1:9), 3, 3)
  expect_type(rowMeans3(num_m), "double")
  expect_type(colMeans3(num_m), "double")

  # Logical matrix (should work)
  log_m <- matrix(c(TRUE, FALSE, TRUE, FALSE), 2, 2, byrow = TRUE)
  expect_equal(rowMeans3(log_m), c(0.5, 0.5))
  expect_equal(colMeans3(log_m), c(1, 0))
})

test_that("matrixStats functions with matrixStats package", {
  skip_if_not_installed("matrixStats")

  # Test that functions delegate to matrixStats when available
  set.seed(123)
  m <- matrix(rnorm(20), 4, 5)

  # These should work without error when matrixStats is available
  expect_silent(rowMeans3(m))
  expect_silent(colMeans3(m))
  expect_silent(rowVars3(m))
  expect_silent(colVars3(m))
  expect_silent(rowSds3(m))
  expect_silent(colSds3(m))
  expect_silent(colQuantiles3(m))
})

test_that("matrixStats functions without matrixStats package", {
  # This tests the fallback implementations
  # We can't easily uninstall matrixStats, but we can test the logic
  # by checking that the functions work with base R calculations

  set.seed(123)
  m <- matrix(rnorm(20), 4, 5)

  # Compare with base R functions
  expect_equal(rowMeans3(m), rowMeans(m))
  expect_equal(colMeans3(m), colMeans(m))
  expect_equal(rowVars3(m), apply(m, 1, var))
  expect_equal(colVars3(m), apply(m, 2, var))
  expect_equal(rowSds3(m), sqrt(apply(m, 1, var)))
  expect_equal(colSds3(m), sqrt(apply(m, 2, var)))
})

test_that("performance with large matrices", {
  skip_on_cran()

  set.seed(123)
  large_m <- matrix(rnorm(10000), 100, 100)

  expect_time_lt <- function(expr, time_limit = 1) {
    start_time <- Sys.time()
    force(expr)
    end_time <- Sys.time()
    expect_lt(
      as.numeric(difftime(end_time, start_time, units = "secs")),
      time_limit
    )
  }

  # Test that functions complete in reasonable time
  expect_time_lt(rowMeans3(large_m), time_limit = 0.5)
  expect_time_lt(colMeans3(large_m), time_limit = 0.5)
  expect_time_lt(rowVars3(large_m), time_limit = 1)
  expect_time_lt(colVars3(large_m), time_limit = 1)
})
