skip_on_cran()
skip_if_not_installed("ggplot2")
skip_if_not_installed("microbenchmark")

mat_10_10 <- matrix(runif(100L), 10L)
mat_100_100 <- matrix(runif(10000L), 100L)

tolerant <- function(x, y) {
  max(abs(x - y))
}


test_that("it's faster?", {
  stats_10 <- microbenchmark::microbenchmark(
    preprocessCore = preprocessCore::normalize.quantiles(mat_10_10),
    preprocessCore_inplace = preprocessCore::normalize.quantiles(
      mat_10_10,
      copy = FALSE
    ),
    Cpp = normalize_quantiles_cpp(beachmat::initializeCpp(mat_10_10))
  )

  stats_100 <- microbenchmark::microbenchmark(
    preprocessCore = preprocessCore::normalize.quantiles(mat_100_100),
    preprocessCore_inplace = preprocessCore::normalize.quantiles(
      mat_100_100,
      copy = FALSE
    ),
    Cpp = normalize_quantiles_cpp(beachmat::initializeCpp(mat_100_100))
  )

  ggplot2::autoplot(stats_10)
  ggplot2::autoplot(stats_100)
})

test_that("it's correct?", {
  mat_10_10_norm <- preprocessCore::normalize.quantiles(mat_10_10)
  mat_100_100_norm <- preprocessCore::normalize.quantiles(mat_100_100)

  mat_10_10_norm_cpp <- normalize_quantiles_cpp(beachmat::initializeCpp(
    mat_10_10
  ))
  mat_100_100_norm_cpp <- normalize_quantiles_cpp(beachmat::initializeCpp(
    mat_100_100
  ))

  Mat_10_10_norm_cpp <- normalize_quantiles_cpp(beachmat::initializeCpp(
    Matrix::Matrix(mat_10_10)
  ))
  Mat_100_100_norm_cpp <- normalize_quantiles_cpp(beachmat::initializeCpp(
    Matrix::Matrix(mat_100_100)
  ))

  expect_lt(tolerant(mat_10_10_norm, mat_10_10_norm_cpp), 1e-6)
  expect_lt(tolerant(mat_100_100_norm, mat_100_100_norm_cpp), 1e-6)
})
