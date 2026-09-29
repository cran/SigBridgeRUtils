# test-13_MASS.R - Tests for MASS functions (ginv2)

test_that("ginv2 works with basic matrices", {
  # Test with square invertible matrix
  m <- matrix(c(1L, 2L, 3L, 4L), 2L, 2L)
  result <- ginv2(m)

  result2 <- MASS::ginv(m)

  expect_equal(dim(result), c(2L, 2L))

  # Test Moore-Penrose properties: X * ginv(X) * X = X
  expect_equal(m %*% result %*% m, m, tolerance = 1e-10)
  expect_lt(max(abs(result2 - result)), 1e-10)
})

test_that("ginv2 works with non-square matrices", {
  # Test with rectangular matrix (more rows than columns)
  m1 <- matrix(1L:6L, 3L, 2L)
  result1 <- ginv2(m1)
  expect_equal(dim(result1), c(2L, 3L))

  # Test with rectangular matrix (more columns than rows)
  m2 <- matrix(1L:6L, 2L, 3L)
  result2 <- ginv2(m2)
  expect_equal(dim(result2), c(3L, 2L))
})

test_that("ginv2 handles singular matrices", {
  # Test with singular matrix (determinant = 0)
  singular_m <- matrix(c(1L, 2L, 2L, 4L), 2L, 2L)
  result <- ginv2(singular_m)
  expect_equal(dim(result), c(2L, 2L))
})

test_that("ginv2 works with complex matrices", {
  # Test with complex numbers
  complex_m <- matrix(c(1L + 1i, 2L - 1i, 3L + 2i, 4L - 2i), 2L, 2L)
  result <- ginv2(complex_m)
  expect_equal(dim(result), c(2L, 2L))
  expect_true(is.complex(result))
})

test_that("ginv2_default method works correctly", {
  m <- matrix(c(1L, 2L, 3L, 6L), 2L, 2L)
  result <- ginv2_default(m)
  expect_equal(dim(result), c(2L, 2L))
})

test_that("ginv2 handles different tolerance values", {
  m <- matrix(c(1L, 2L, 3L, 6L), 2L, 2L)

  # Test with default tolerance
  result_default <- ginv2(m)

  # Test with custom tolerance
  result_custom <- ginv2(m, tol = 1e-8)
  expect_equal(dim(result_default), dim(result_custom))
})

test_that("ginv2 handles zero matrix", {
  zero_m <- matrix(0L, 3L, 3L)
  result <- ginv2(zero_m)
  expect_equal(dim(result), c(3L, 3L))
  expect_true(all(abs(result) < 1e-10))
})

test_that("ginv2 handles identity matrix", {
  identity_m <- diag(3L)
  result <- ginv2(identity_m)
  expect_equal(result, identity_m, tolerance = 1e-10)
})

test_that("ginv2 works with Matrix objects", {
  skip_if_not_installed("Matrix")
  library(Matrix)

  m <- matrix(c(1L, 2L, 3L, 4L), 2L, 2L)
  dense_m <- Matrix(m, sparse = FALSE)

  result <- ginv2(dense_m)
  expect_equal(dim(result), c(2L, 2L))
})

test_that("ginv2 error handling", {
  # Test with non-numeric input
  expect_error(
    ginv2("not a matrix"),
    "integer or real"
  )

  # Test with 3D array
  array_3d <- array(1L:8L, dim = c(2L, 2L, 2L))
  expect_error(
    ginv2(array_3d),
    "'X' must be a numeric or complex matrix"
  )
})

test_that("ginv2 handles vectors (coerced to matrix)", {
  v <- 1L:4L
  result <- ginv2(v)
  expect_equal(dim(result), c(1L, 4L))
})

test_that("ginv2 Moore-Penrose properties", {
  # Test the four Moore-Penrose conditions
  m <- matrix(c(1L, 2L, 3L, 4L, 5L, 6L), 2L, 3L)
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
  set.seed(123L)
  large_m <- matrix(rnorm(1000L), 100L, 10L)

  expect_time_lt <- function(expr, time_limit = 5L) {
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
      expect_equal(dim(result), c(10L, 100L))
    },
    time_limit = 10L
  )
})
