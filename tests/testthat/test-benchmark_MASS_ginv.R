skip_on_cran()
skip_if_not_installed("ggplot2")
skip_if_not_installed("microbenchmark")
skip_if_not_installed("MASS")

set.seed(123L)
mat_10_10 <- matrix(runif(100L), 10L)
mat_100_100 <- matrix(runif(10000L), 100L)

tolerant <- function(x, y) {
  max(abs(x - y))
}

ginv2_v0.2.7 <- function(X, tol = sqrt(.Machine$double.eps), ...) {
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }

  Xsvd <- svd(X)
  d <- Xsvd$d
  u <- Xsvd$u
  v <- Xsvd$v

  if (is.complex(X)) {
    u <- Conj(u)
  }

  Positive <- d > max(tol * d[1L], 0L)

  if (!any(Positive)) {
    return(array(0L, dim(X)[c(2L, 1L)]))
  }

  if (all(Positive)) {
    v %*% (1L / d * t(u))
  } else {
    v[, Positive, drop = FALSE] %*%
      ((1L / d[Positive]) * t(u[, Positive, drop = FALSE]))
  }
}


test_that("it's faster?", {
  stats_10 <- microbenchmark::microbenchmark(
    MASS = MASS::ginv(mat_10_10),
    R = ginv2_v0.2.7(mat_10_10),
    Cpp = ginv_cpp(beachmat::initializeCpp(mat_10_10))
  )

  stats_100 <- microbenchmark::microbenchmark(
    MASS = MASS::ginv(mat_100_100),
    R = ginv2_v0.2.7(mat_100_100),
    Cpp = ginv_cpp(beachmat::initializeCpp(mat_100_100))
  )

  ggplot2::autoplot(stats_10)
  ggplot2::autoplot(stats_100)
})

test_that("it's correct?", {
  mat_10_10_ginv <- MASS::ginv(mat_10_10)
  mat_100_100_ginv <- MASS::ginv(mat_100_100)

  mat_10_10_ginv_cpp <- ginv_cpp(beachmat::initializeCpp(mat_10_10))
  mat_100_100_ginv_cpp <- ginv_cpp(beachmat::initializeCpp(mat_100_100))

  Mat_10_10_ginv_cpp <- ginv_cpp(beachmat::initializeCpp(
    Matrix::Matrix(mat_10_10)
  ))
  Mat_100_100_ginv_cpp <- ginv_cpp(beachmat::initializeCpp(
    Matrix::Matrix(mat_100_100)
  ))

  expect_lt(tolerant(mat_10_10_ginv, mat_10_10_ginv_cpp), 1e-6)
  expect_lt(tolerant(mat_100_100_ginv, mat_100_100_ginv_cpp), 1e-6)
  expect_lt(tolerant(mat_10_10_ginv, Mat_10_10_ginv_cpp), 1e-6)
  expect_lt(tolerant(mat_100_100_ginv, Mat_100_100_ginv_cpp), 1e-6)
})
