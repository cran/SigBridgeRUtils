#include <R_ext/Arith.h>
#include <Rcpp.h>
#include <Rtatami.h>

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <vector>

// [[Rcpp::depends(beachmat, assorthead)]]
// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

/*
 * Quantile normalization based on beachmat/tatami.
 *
 * initialized_matrix must be the external pointer returned on the R side by:
 *
 *     beachmat::initializeCpp(x)
 *
 * Algorithm:
 *
 * 1. Read the matrix column by column;
 * 2. Sort each column after removing NA/NaN;
 * 3. Compute the mean across all columns for each sorted position;
 * 4. Read the original matrix a second time and replace each value with the
 *    target distribution value corresponding to its rank within its column;
 * 5. Keep the original NA/NaN positions as NA.
 */
// [[Rcpp::export]]
NumericMatrix normalize_quantiles_cpp(SEXP initialized_matrix) {
  Rtatami::BoundNumericPointer parsed(initialized_matrix);

  /*
   * Keep a local shared pointer to ensure the backend matrix object
   * stays alive for the duration of this function.
   */
  auto matrix = parsed->ptr;

  const std::size_t nr = static_cast<std::size_t>(matrix->nrow());

  const std::size_t nc = static_cast<std::size_t>(matrix->ncol());

  if (nr > static_cast<std::size_t>(INT_MAX) ||
      nc > static_cast<std::size_t>(INT_MAX)) {
    stop("Matrix dimensions exceed R integer limits.");
  }

  NumericMatrix output(static_cast<int>(nr), static_cast<int>(nc));

  /*
   * Return immediately for an empty matrix.
   */
  if (nr == 0 || nc == 0) {
    return output;
  }

  /*
   * sorted_columns[col] stores the sorted values of column col after
   * removing NA/NaN.
   *
   * For columns containing missing values, its length is less than nr.
   */
  std::vector<std::vector<double>> sorted_columns(nc);

  /*
   * target_distribution[rank] is the non-NA mean across all columns
   * at that sorted position.
   */
  std::vector<double> target_distribution(nr, NA_REAL);

  std::vector<double> target_sum(nr, 0.0);

  std::vector<std::size_t> target_count(nr, 0);

  /*
   * Access the matrix column by column.
   */
  auto accessor = matrix->dense_column();

  std::vector<double> buffer(nr);

  /*
   * First pass:
   *
   * - Read each column;
   * - Remove NA/NaN;
   * - Sort the non-missing values;
   * - Accumulate the mean for each rank.
   */
  for (std::size_t col = 0; col < nc; ++col) {
    auto values = accessor->fetch(col, buffer.data());

    std::vector<double> &sorted = sorted_columns[col];

    sorted.reserve(nr);

    for (std::size_t row = 0; row < nr; ++row) {
      const double value = values[row];

      /*
       * ISNAN detects both NA_real_ and NaN, matching the behavior of
       * is.na() on numeric matrices in R.
       */
      if (!ISNAN(value)) {
        sorted.push_back(value);
      }
    }

    std::sort(sorted.begin(), sorted.end());

    for (std::size_t rank = 0; rank < sorted.size(); ++rank) {
      target_sum[rank] += sorted[rank];
      ++target_count[rank];
    }
  }

  /*
   * Compute the target distribution.
   *
   * This is equivalent to:
   *
   *     rowMeans(sorted_mat, na.rm = TRUE)
   *
   * Ranks for which no column has a valid value are never actually used;
   * they are kept as NA here.
   */
  for (std::size_t rank = 0; rank < nr; ++rank) {
    if (target_count[rank] > 0) {
      target_distribution[rank] =
          target_sum[rank] / static_cast<double>(target_count[rank]);
    } else {
      target_distribution[rank] = NA_REAL;
    }
  }

  /*
   * Second pass:
   *
   * - Read each column again;
   * - For each non-NA value, find its first occurrence in the sorted column;
   * - Replace it with the corresponding target distribution value;
   * - Keep NA/NaN as NA.
   */
  for (std::size_t col = 0; col < nc; ++col) {
    auto values = accessor->fetch(col, buffer.data());

    const std::vector<double> &sorted = sorted_columns[col];

    for (std::size_t row = 0; row < nr; ++row) {
      const double value = values[row];

      if (ISNAN(value)) {
        output(static_cast<int>(row), static_cast<int>(col)) = NA_REAL;

        continue;
      }

      /*
       * std::lower_bound returns the first position equal to value.
       *
       * This corresponds to the original R implementation:
       *
       *     match(value, sort(col, na.last = NA))
       *
       * Therefore duplicated values share the same first rank.
       */
      auto it = std::lower_bound(sorted.begin(), sorted.end(), value);

      if (it == sorted.end() || *it != value) {
        stop("Internal error while computing quantile ranks.");
      }

      const std::size_t rank =
          static_cast<std::size_t>(std::distance(sorted.begin(), it));

      output(static_cast<int>(row), static_cast<int>(col)) =
          target_distribution[rank];
    }
  }

  return output;
}
