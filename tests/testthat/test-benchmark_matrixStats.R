skip_on_cran()
skip_if_not_installed("matrixStats")
skip_if_not_installed("microbenchmark")

set.seed(8047L)
mat_100_100 <- matrix(rnorm(10000L), nrow = 100L)
mat_100_100[c(125L, 857L)] <- NA_real_
probs <- c(0.1, 0.5, 0.9)

benchmark_statistic <- function(reference, candidate, times = 20L) {
  microbenchmark::microbenchmark(
    matrixStats = reference(),
    SigBridgeRUtils = candidate(),
    times = times
  )
}

test_that("matrix statistic implementations have comparable benchmarks", {
  skip_on_cran()

  benchmarks <- list(
    row_means = benchmark_statistic(
      function() matrixStats::rowMeans2(mat_100_100, na.rm = TRUE),
      function() rowMeans3(mat_100_100, na.rm = TRUE)
    ),
    col_means = benchmark_statistic(
      function() matrixStats::colMeans2(mat_100_100, na.rm = TRUE),
      function() colMeans3(mat_100_100, na.rm = TRUE)
    ),
    row_sums = benchmark_statistic(
      function() matrixStats::rowSums2(mat_100_100, na.rm = TRUE),
      function() rowSums3(mat_100_100, na.rm = TRUE)
    ),
    col_sums = benchmark_statistic(
      function() matrixStats::colSums2(mat_100_100, na.rm = TRUE),
      function() colSums3(mat_100_100, na.rm = TRUE)
    ),
    row_variances = benchmark_statistic(
      function() matrixStats::rowVars(mat_100_100, na.rm = TRUE),
      function() rowVars3(mat_100_100, na.rm = TRUE)
    ),
    col_variances = benchmark_statistic(
      function() matrixStats::colVars(mat_100_100, na.rm = TRUE),
      function() colVars3(mat_100_100, na.rm = TRUE)
    ),
    row_standard_deviations = benchmark_statistic(
      function() matrixStats::rowSds(mat_100_100, na.rm = TRUE),
      function() rowSds3(mat_100_100, na.rm = TRUE)
    ),
    col_standard_deviations = benchmark_statistic(
      function() matrixStats::colSds(mat_100_100, na.rm = TRUE),
      function() colSds3(mat_100_100, na.rm = TRUE)
    ),
    row_maxima = benchmark_statistic(
      function() matrixStats::rowMaxs(mat_100_100, na.rm = TRUE),
      function() rowMaxs3(mat_100_100, na.rm = TRUE)
    ),
    row_medians = benchmark_statistic(
      function() matrixStats::rowMedians(mat_100_100, na.rm = TRUE),
      function() rowMedians3(mat_100_100, na.rm = TRUE)
    ),
    col_medians = benchmark_statistic(
      function() matrixStats::colMedians(mat_100_100, na.rm = TRUE),
      function() colMedians3(mat_100_100, na.rm = TRUE)
    ),
    col_quantiles = benchmark_statistic(
      function() {
        matrixStats::colQuantiles(mat_100_100, probs = probs, na.rm = TRUE)
      },
      function() colQuantiles3(mat_100_100, probs = probs, na.rm = TRUE)
    )
  )

  expect_true(all(vapply(
    benchmarks,
    inherits,
    logical(1L),
    what = "microbenchmark"
  )))
})
