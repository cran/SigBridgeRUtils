# test-13_MASS.R - Tests for MASS functions (ginv2)

test_that("ginv2 works with basic matrices", {
  # Test with square invertible matrix
  m <- matrix(c(1, 2, 3, 4), 2, 2)
  result <- ginv2(m)

  result2 <- MASS::ginv(m)

  expect_equal(dim(result), c(2, 2))

  # Test Moore-Penrose properties: X * ginv(X) * X = X
  expect_equal(m %*% result %*% m, m, tolerance = 1e-10)
  expect_lt(max(abs(result2 - result)), 1e-10)
})

test_that("ginv2 works with non-square matrices", {
  # Test with rectangular matrix (more rows than columns)
  m1 <- matrix(1:6, 3, 2)
  result1 <- ginv2(m1)
  expect_equal(dim(result1), c(2, 3))

  # Test with rectangular matrix (more columns than rows)
  m2 <- matrix(1:6, 2, 3)
  result2 <- ginv2(m2)
  expect_equal(dim(result2), c(3, 2))
})

test_that("ginv2 handles singular matrices", {
  # Test with singular matrix (determinant = 0)
  singular_m <- matrix(c(1, 2, 2, 4), 2, 2)
  result <- ginv2(singular_m)
  expect_equal(dim(result), c(2, 2))
})

test_that("ginv2 works with complex matrices", {
  # Test with complex numbers
  complex_m <- matrix(c(1 + 1i, 2 - 1i, 3 + 2i, 4 - 2i), 2, 2)
  result <- ginv2(complex_m)
  expect_equal(dim(result), c(2, 2))
  expect_true(is.complex(result))
})

test_that("ginv2.default method works correctly", {
  m <- matrix(c(1, 2, 3, 6), 2, 2)
  result <- ginv2.default(m)
  expect_equal(dim(result), c(2, 2))
})

test_that("ginv2 handles different tolerance values", {
  m <- matrix(c(1, 2, 3, 6), 2, 2)

  # Test with default tolerance
  result_default <- ginv2(m)

  # Test with custom tolerance
  result_custom <- ginv2(m, tol = 1e-8)
  expect_equal(dim(result_default), dim(result_custom))
})

test_that("ginv2 handles zero matrix", {
  zero_m <- matrix(0, 3, 3)
  result <- ginv2(zero_m)
  expect_equal(dim(result), c(3, 3))
  expect_true(all(abs(result) < 1e-10))
})

test_that("ginv2 handles identity matrix", {
  identity_m <- diag(3)
  result <- ginv2(identity_m)
  expect_equal(result, identity_m, tolerance = 1e-10)
})

test_that("ginv2 works with Matrix objects", {
  skip_if_not_installed("Matrix")
  library(Matrix)

  m <- matrix(c(1, 2, 3, 4), 2, 2)
  dense_m <- Matrix(m, sparse = FALSE)

  result <- ginv2(dense_m)
  expect_equal(dim(result), c(2, 2))
})

test_that("ginv2 error handling", {
  # Test with non-numeric input
  expect_error(
    ginv2("not a matrix"),
    "'X' must be a numeric or complex matrix"
  )

  # Test with 3D array
  array_3d <- array(1:8, dim = c(2, 2, 2))
  expect_error(
    ginv2(array_3d),
    "'X' must be a numeric or complex matrix"
  )

  # Test with logical matrix
  logical_m <- matrix(c(TRUE, FALSE, TRUE, FALSE), 2, 2)
  expect_error(
    ginv2(logical_m),
    "'X' must be a numeric or complex matrix"
  )
})

test_that("ginv2 handles vectors (coerced to matrix)", {
  v <- 1:4
  result <- ginv2(v)
  expect_equal(dim(result), c(1, 4))
})

test_that("ginv2 Moore-Penrose properties", {
  # Test the four Moore-Penrose conditions
  m <- matrix(c(1, 2, 3, 4, 5, 6), 2, 3)
  ginv_m <- ginv2(m)

  # 1. A * G * A = A
  expect_equal(m %*% ginv_m %*% m, m, tolerance = 1e-10)

  # 2. G * A * G = G
  expect_equal(ginv_m %*% m %*% ginv_m, ginv_m, tolerance = 1e-10)

  # 3. (A * G)' = A * G (Hermitian)
  ag <- m %*% ginv_m
  expect_equal(t(ag), ag, tolerance = 1e-10)

  # 4. (G * A)' = G * A (Hermitian)
  ga <- ginv_m %*% m
  expect_equal(t(ga), ga, tolerance = 1e-10)
})

test_that("ginv2 performance with larger matrices", {
  skip_on_cran()

  # Test with larger matrix
  set.seed(123)
  large_m <- matrix(rnorm(1000), 100, 10)

  expect_time_lt <- function(expr, time_limit = 5) {
    start_time <- Sys.time()
    force(expr)
    end_time <- Sys.time()
    expect_lt(
      as.numeric(difftime(end_time, start_time, units = "secs")),
      time_limit
    )
  }

  expect_time_lt(
    {
      result <- ginv2(large_m)
      expect_equal(dim(result), c(10, 100))
    },
    time_limit = 10
  )
})
