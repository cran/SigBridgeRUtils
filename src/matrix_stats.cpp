#include <Eigen/Dense>
#include <R_ext/Arith.h>
#include <Rcpp.h>
#include <Rtatami.h>

// #include <algorithm>
#include <cmath>
// #include <cstddef>
// #include <limits>
// #include <string>
// #include <vector>

// [[Rcpp::depends(beachmat, assorthead)]]
// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;
using Eigen::ArrayXd;
using Eigen::Map;
using Eigen::MatrixXd;
using Eigen::VectorXd;

namespace {

double quantile_type7(std::vector<double>& values, double probability) {
  if (values.empty()) return NA_REAL;

  const double index = probability * static_cast<double>(values.size() - 1);
  const auto lower = static_cast<std::size_t>(std::floor(index));
  const auto upper = static_cast<std::size_t>(std::ceil(index));

  std::nth_element(values.begin(), values.begin() + lower, values.end());
  const double lower_value = values[lower];
  if (lower == upper) return lower_value;

  std::nth_element(values.begin(), values.begin() + upper, values.end());
  return lower_value +
         (index - static_cast<double>(lower)) * (values[upper] - lower_value);
}

// Eigen SIMD
double summarize_no_na(const double* values, std::size_t length,
                       const std::string& statistic) {
  Map<const VectorXd> v(values, length);

  if (statistic == "sum") return v.sum();
  if (statistic == "mean") return v.mean();
  if (statistic == "max") return v.maxCoeff();
  if (statistic == "var") {
    if (length <= 1) return NA_REAL;
    const double m = v.mean();
    return (v.array() - m).square().sum() / static_cast<double>(length - 1);
  }

  if (statistic == "median") {
    std::vector<double> copy(values, values + length);
    return quantile_type7(copy, 0.5);
  }
  stop("Unknown matrix statistic.");
}

// Welford
double summarize_with_na(const double* values, std::size_t length,
                         const std::string& statistic, bool na_rm) {
  if (length == 0) {
    return statistic == "sum" ? 0.0 : NA_REAL;
  }

  double total = 0.0, mean = 0.0, squared_deviation_sum = 0.0;
  std::size_t count = 0;
  double maximum = -std::numeric_limits<double>::infinity();

  for (std::size_t i = 0; i < length; ++i) {
    const double value = values[i];
    if (ISNAN(value)) {
      if (!na_rm) return NA_REAL;
      continue;
    }
    ++count;
    total += value;
    const double delta = value - mean;
    mean += delta / static_cast<double>(count);
    squared_deviation_sum += delta * (value - mean);
    maximum = std::max(maximum, value);
  }

  if (statistic == "sum") return total;
  if (statistic == "mean") return count ? total / static_cast<double>(count) : R_NaN;
  if (statistic == "var")
    return count > 1 ? squared_deviation_sum / (count - 1) : NA_REAL;
  if (statistic == "max")
    return count ? maximum : -std::numeric_limits<double>::infinity();
  if (statistic == "median") {
    std::vector<double> observed;
    observed.reserve(count);
    for (std::size_t i = 0; i < length; ++i)
      if (!ISNAN(values[i])) observed.push_back(values[i]);
    return quantile_type7(observed, 0.5);
  }
  stop("Unknown matrix statistic.");
}

double summarize(const double* values, std::size_t length, const std::string& statistic,
                 bool na_rm) {
  if (length == 0) {
    return statistic == "sum" ? 0.0 : NA_REAL;
  }

  // detect NA
  Map<const ArrayXd> arr(values, length);
  const bool has_na = arr.isNaN().any();

  if (!has_na) {
    return summarize_no_na(values, length, statistic);
  }
  return summarize_with_na(values, length, statistic, na_rm);
}
} // namespace

// [[Rcpp::export]]
NumericVector matrix_summary_cpp(SEXP initialized_matrix, std::string statistic,
                                 bool by_row, bool na_rm = false) {
  Rtatami::BoundNumericPointer parsed(initialized_matrix);
  auto matrix = parsed->ptr;

  const std::size_t output_length = by_row ? matrix->nrow() : matrix->ncol();
  const std::size_t input_length = by_row ? matrix->ncol() : matrix->nrow();

  if (output_length > static_cast<std::size_t>(INT_MAX) ||
      input_length > static_cast<std::size_t>(INT_MAX)) {
    stop("Matrix dimensions exceed R integer limits.");
  }

  NumericVector output(static_cast<R_xlen_t>(output_length));

#ifdef _OPENMP
#pragma omp parallel
#endif
  {
    auto accessor = by_row ? matrix->dense_row() : matrix->dense_column();
    std::vector<double> buffer(input_length);
#ifdef _OPENMP
#pragma omp for
#endif
    for (std::size_t i = 0; i < output_length; ++i) {
      const auto values = accessor->fetch(i, buffer.data());
      output[static_cast<R_xlen_t>(i)] =
          summarize(values, input_length, statistic, na_rm);
    }
  }

  return output;
}

// [[Rcpp::export]]
NumericMatrix matrix_quantiles_cpp(SEXP initialized_matrix, NumericVector probabilities,
                                   bool na_rm = false) {
  Rtatami::BoundNumericPointer parsed(initialized_matrix);
  auto matrix = parsed->ptr;

  const std::size_t nr = matrix->nrow();
  const std::size_t nc = matrix->ncol();
  if (nr > static_cast<std::size_t>(INT_MAX) ||
      nc > static_cast<std::size_t>(INT_MAX)) {
    stop("Matrix dimensions exceed R integer limits.");
  }

  for (double probability : probabilities) {
    if (!std::isfinite(probability) || probability < 0.0 || probability > 1.0) {
      stop("'probs' must contain values between 0 and 1.");
    }
  }

  NumericMatrix output(static_cast<R_xlen_t>(nc), probabilities.size());
  auto accessor = matrix->dense_column();
  std::vector<double> buffer(nr);
  std::vector<double> observed;
  observed.reserve(nr);

  for (std::size_t col = 0; col < nc; ++col) {
    const auto values = accessor->fetch(col, buffer.data());
    observed.clear();

    for (std::size_t row = 0; row < nr; ++row) {
      if (ISNAN(values[row])) {
        if (!na_rm) {
          observed.clear();
          break;
        }
      } else {
        observed.push_back(values[row]);
      }
    }

    if (!na_rm && observed.size() != nr) {
      std::fill(output.row(col).begin(), output.row(col).end(), NA_REAL);
      continue;
    }

    for (R_xlen_t p = 0; p < probabilities.size(); ++p) {
      auto copy = observed;
      output(col, p) = quantile_type7(copy, probabilities[p]);
    }
  }

  return output;
}
