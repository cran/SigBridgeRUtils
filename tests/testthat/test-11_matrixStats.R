# test-11_matrixStats.R - Tests for matrix statistics functions

test_that("rowMeans3 works correctly", {
  # Basic test
  m <- matrix(1L:9L, 3L, 3L)
  result <- rowMeans3(m)
  expected <- rowMeans(m)
  expect_equal(result, expected)

  # Test with NA values
  m_na <- m
  m_na[1L, 1L] <- NA
  result_na <- rowMeans3(m_na, na.rm = TRUE)
  expected_na <- rowMeans(m_na, na.rm = TRUE)
  expect_equal(result_na, expected_na)

  # Test with all NA row
  m_all_na <- matrix(NA, 2L, 3L)
  result_all_na <- rowMeans3(m_all_na, na.rm = TRUE)
  expect_true(all(is.nan(result_all_na)))
})

test_that("colMeans3 works correctly", {
  # Basic test
  m <- matrix(1L:9L, 3L, 3L)
  result <- colMeans3(m)
  expected <- colMeans(m)
  expect_equal(result, expected)

  # Test with NA values
  m_na <- m
  m_na[1L, 1L] <- NA
  result_na <- colMeans3(m_na, na.rm = TRUE)
  expected_na <- colMeans(m_na, na.rm = TRUE)
  expect_equal(result_na, expected_na)
})

test_that("rowVars3 works correctly", {
  # Basic test
  set.seed(123L)
  m <- matrix(rnorm(15L), 3L, 5L)
  result <- rowVars3(m)

  # Manual calculation
  manual_result <- apply(m, 1L, var)
  expect_equal(result, manual_result, tolerance = 1e-10)

  # Test with NA values
  m_na <- m
  m_na[1L, 2L] <- NA
  result_na <- rowVars3(m_na, na.rm = TRUE)
  manual_na <- apply(m_na, 1L, var, na.rm = TRUE)
  expect_equal(result_na, manual_na, tolerance = 1e-10)

  # Test with constant row (variance = 0)
  m_const <- matrix(rep(1L:3L, each = 4L), 3L, 4L, byrow = TRUE)
  result_const <- rowVars3(m_const)
  expect_true(all(result_const == 0L))
})

test_that("colVars3 works correctly", {
  # Basic test
  set.seed(123L)
  m <- matrix(rnorm(15L), 3L, 5L)
  result <- colVars3(m)

  # Manual calculation
  manual_result <- apply(m, 2L, var)
  expect_equal(result, manual_result, tolerance = 1e-10)

  # Test with NA values
  m_na <- m
  m_na[1L, 2L] <- NA
  result_na <- colVars3(m_na, na.rm = TRUE)
  manual_na <- apply(m_na, 2L, var, na.rm = TRUE)
  expect_equal(result_na, manual_na, tolerance = 1e-10)

  # Test with constant column (variance = 0)
  m_const <- matrix(rep(1L:5L, each = 3L), 3L, 5L)
  result_const <- colVars3(m_const)
  expect_true(all(result_const == 0L))
})

test_that("rowSds3 works correctly", {
  # Basic test
  set.seed(123L)
  m <- matrix(rnorm(15L), 3L, 5L)
  result <- rowSds3(m)

  # Manual calculation (sqrt of variance)
  manual_result <- sqrt(apply(m, 1L, var))
  expect_equal(result, manual_result, tolerance = 1e-10)

  # Test with NA values
  m_na <- m
  m_na[1L, 2L] <- NA
  result_na <- rowSds3(m_na, na.rm = TRUE)
  manual_na <- sqrt(apply(m_na, 1L, var, na.rm = TRUE))
  expect_equal(result_na, manual_na, tolerance = 1e-10)
})

test_that("colSds3 works correctly", {
  # Basic test
  set.seed(123L)
  m <- matrix(rnorm(15L), 3L, 5L)
  result <- colSds3(m)

  # Manual calculation (sqrt of variance)
  manual_result <- sqrt(apply(m, 2L, var))
  expect_equal(result, manual_result, tolerance = 1e-10)

  # Test with NA values
  m_na <- m
  m_na[1L, 2L] <- NA
  result_na <- colSds3(m_na, na.rm = TRUE)
  manual_na <- sqrt(apply(m_na, 2L, var, na.rm = TRUE))
  expect_equal(result_na, manual_na, tolerance = 1e-10)
})

test_that("colQuantiles3 works correctly", {
  # Basic test with default probs
  set.seed(123L)
  m <- matrix(rnorm(20L), 4L, 5L)
  result <- colQuantiles3(m)

  # Check dimensions
  expect_equal(dim(result), c(5L, 5L)) # 5 quantiles, 5 columns

  # Test with custom probabilities
  custom_probs <- c(0.25, 0.75)
  result_custom <- colQuantiles3(m, probs = custom_probs)
  expect_equal(dim(result_custom), c(5L, 2L))

  # Test with single probability
  result_single <- colQuantiles3(m, probs = 0.5)
  expect_equal(length(result_single), 5L)
})

test_that("matrixStats functions handle edge cases", {
  # Empty matrix
  empty_m <- matrix(numeric(0L), 0L, 0L)
  expect_length(rowMeans3(empty_m), 0L)
  expect_length(colMeans3(empty_m), 0L)

  # Single element matrix
  single_m <- matrix(42L)
  expect_equal(rowMeans3(single_m), 42L)
  expect_equal(colMeans3(single_m), 42L)
  expect_equal(rowVars3(single_m), NA_real_) # variance of single value is NA
  expect_equal(colVars3(single_m), NA_real_)

  # Single row/column
  single_row <- matrix(1L:5L, 1L, 5L)
  expect_equal(rowMeans3(single_row), mean(1L:5L))
  expect_equal(length(colMeans3(single_row)), 5L)

  single_col <- matrix(1L:5L, 5L, 1L)
  expect_equal(length(rowMeans3(single_col)), 5L)
  expect_equal(colMeans3(single_col), mean(1L:5L))
})

test_that("matrixStats functions handle different data types", {
  # Integer matrix
  int_m <- matrix(1L:9L, 3L, 3L)
  expect_type(rowMeans3(int_m), "double")
  expect_type(colMeans3(int_m), "double")

  # Numeric matrix
  num_m <- matrix(as.numeric(1L:9L), 3L, 3L)
  expect_type(rowMeans3(num_m), "double")
  expect_type(colMeans3(num_m), "double")

  # Logical matrix (should work)
  log_m <- matrix(c(TRUE, FALSE, TRUE, FALSE), 2L, 2L, byrow = TRUE)
  expect_equal(rowMeans3(log_m), c(0.5, 0.5))
  expect_equal(colMeans3(log_m), c(1L, 0L))
})

test_that("matrixStats functions with matrixStats package", {
  skip_if_not_installed("matrixStats")

  # Test that functions delegate to matrixStats when available
  set.seed(123L)
  m <- matrix(rnorm(20L), 4L, 5L)

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

  set.seed(123L)
  m <- matrix(rnorm(20L), 4L, 5L)

  # Compare with base R functions
  expect_equal(rowMeans3(m), rowMeans(m))
  expect_equal(colMeans3(m), colMeans(m))
  expect_equal(rowVars3(m), apply(m, 1L, var))
  expect_equal(colVars3(m), apply(m, 2L, var))
  expect_equal(rowSds3(m), sqrt(apply(m, 1L, var)))
  expect_equal(colSds3(m), sqrt(apply(m, 2L, var)))
})

test_that("performance with large matrices", {
  skip_on_cran()

  set.seed(123L)
  large_m <- matrix(rnorm(10000L), 100L, 100L)

  expect_time_lt <- function(expr, time_limit = 1L) {
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
  expect_time_lt(rowVars3(large_m), time_limit = 1L)
  expect_time_lt(colVars3(large_m), time_limit = 1L)
})

test_that("matrix statistics support sparse and delayed matrices", {
  skip_if_not_installed("Matrix")
  skip_if_not_installed("DelayedArray")

  set.seed(8047L)
  dense <- matrix(rnorm(30L), nrow = 5L)
  dense[c(2L, 11L)] <- NA_real_
  matrices <- list(
    Matrix::Matrix(dense, sparse = TRUE),
    DelayedArray::DelayedArray(dense)
  )

  for (x in matrices) {
    expect_equal(rowMeans3(x, na.rm = TRUE), rowMeans(dense, na.rm = TRUE))
    expect_equal(colMeans3(x, na.rm = TRUE), colMeans(dense, na.rm = TRUE))
    expect_equal(rowSums3(x, na.rm = TRUE), rowSums(dense, na.rm = TRUE))
    expect_equal(colSums3(x, na.rm = TRUE), colSums(dense, na.rm = TRUE))
    expect_equal(rowVars3(x, na.rm = TRUE), apply(dense, 1L, var, na.rm = TRUE))
    expect_equal(colVars3(x, na.rm = TRUE), apply(dense, 2L, var, na.rm = TRUE))
    expect_equal(
      rowMedians3(x, na.rm = TRUE),
      apply(x, 1L, stats::median, na.rm = TRUE)
    )
    expect_equal(
      colMedians3(x, na.rm = TRUE),
      apply(dense, 2L, median, na.rm = TRUE)
    )
    expect_equal(rowMaxs3(x, na.rm = TRUE), apply(dense, 1L, max, na.rm = TRUE))
    expect_equal(
      colQuantiles3(x, probs = c(0.25, 0.5), na.rm = TRUE),
      matrixStats::colQuantiles(dense, probs = c(0.25, 0.5), na.rm = TRUE)
    )
  }
})

test_that("matrix statistics preserve subsets and names", {
  x <- matrix(
    1L:12L,
    nrow = 3L,
    dimnames = list(letters[1L:3L], LETTERS[1L:4L])
  )

  expect_equal(rowMaxs3(x, rows = 2L:3L, cols = 2L:4L), c(b = 11L, c = 12L))
  expect_null(names(rowMaxs3(x, useNames = FALSE)))
  expect_equal(rowMedians3(x, rows = 2L:3L), c(b = 6.5, c = 7.5))
  expect_equal(colMedians3(x, cols = 2L:3L), c(B = 5L, C = 8L))
})
