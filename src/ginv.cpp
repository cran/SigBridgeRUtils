#include <RcppArmadillo.h>
#include <Rtatami.h>

#include <cmath>
#include <cstddef>
#include <vector>

// [[Rcpp::depends(RcppArmadillo, beachmat, assorthead)]]
// [[Rcpp::plugins(cpp17)]]

using namespace Rcpp;

/*
 * Read the matrix with beachmat/tatami and compute the Moore-Penrose
 * generalized inverse with Armadillo.
 *
 * initialized_matrix must be the external pointer returned on the R side by:
 *
 *     beachmat::initializeCpp(X)
 *
 * The current implementation supports real matrices only. The beachmat
 * NumericMatrix backend is not used to handle complex matrices.
 */
// [[Rcpp::export]]
NumericMatrix
ginv_cpp(SEXP initialized_matrix,
         double tol = 1e-6) {
  if (!std::isfinite(tol)) {
    stop("tol must be a finite numeric value.");
  }

  /*
   * Stay consistent with the original implementation:
   *
   *     Positive <- d > max(tol * d[1L], 0)
   *
   * A negative tol ultimately amounts to a threshold of 0.
   */
  const double effective_tol = std::max(tol, 0.0);

  /*
   * Parse the beachmat external pointer.
   */
  Rtatami::BoundNumericPointer parsed(initialized_matrix);
  auto matrix = parsed->ptr;

  const std::size_t nr = static_cast<std::size_t>(matrix->nrow());

  const std::size_t nc = static_cast<std::size_t>(matrix->ncol());

  if (nr == 0 || nc == 0) {
    /*
     * R's svd() does not normally treat empty matrices as regular input.
     * Here we return a zero matrix with dimensions ncol x nrow.
     */
    return NumericMatrix(static_cast<int>(nc), static_cast<int>(nr));
  }

  if (nr > static_cast<std::size_t>(INT_MAX) ||
      nc > static_cast<std::size_t>(INT_MAX)) {
    stop("Matrix dimensions exceed R integer limits.");
  }

  /*
   * beachmat handles reading; Armadillo handles the SVD.
   *
   * The SVD itself requires a random-access matrix, so the beachmat
   * backend is copied into an arma::mat here.
   */
  arma::mat input(static_cast<arma::uword>(nr), static_cast<arma::uword>(nc),
                  arma::fill::none);

  auto accessor = matrix->dense_column();
  std::vector<double> buffer(nr);

  /*
   * beachmat reads the matrix column by column.
   */
  for (std::size_t col = 0; col < nc; ++col) {
    auto values = accessor->fetch(col, buffer.data());

    for (std::size_t row = 0; row < nr; ++row) {
      const double value = values[row];

      /*
       * Like R's svd(), the input must not contain non-finite values.
       */
      if (!std::isfinite(value)) {
        stop("X contains NA, NaN, or Inf.");
      }

      input(static_cast<arma::uword>(row), static_cast<arma::uword>(col)) =
          value;
    }
  }

  /*
   * Compute the economy-size SVD:
   *
   *     input = U * diag(d) * V.t()
   *
   * U: nr x min(nr, nc)
   * d: min(nr, nc)
   * V: nc x min(nr, nc)
   */
  arma::mat U;
  arma::vec d;
  arma::mat V;

  const bool success = arma::svd_econ(U, d, V, input, "both");

  if (!success) {
    stop("SVD failed to converge.");
  }

  if (d.n_elem == 0) {
    return NumericMatrix(static_cast<int>(nc), static_cast<int>(nr));
  }

  /*
   * d is sorted in descending order, so d[0] is the largest singular value.
   *
   * Original R code:
   *
   *     Positive <- d > max(tol * d[1L], 0)
   */
  const double threshold = std::max(effective_tol * d[0], 0.0);

  /*
   * The result has dimensions ncol x nrow.
   */
  arma::mat inverse(static_cast<arma::uword>(nc), static_cast<arma::uword>(nr),
                    arma::fill::zeros);

  bool has_positive_singular_value = false;

  /*
   * Generalized inverse:
   *
   *     X+ = V %*% diag(1 / d) %*% t(U)
   *
   * Singular values less than or equal to the threshold do not
   * contribute to the computation.
   *
   * Accumulate per-singular-vector outer products to avoid constructing
   * an extra diagonal matrix:
   *
   *     X+ += (1 / d[k]) * V[, k] %*% t(U[, k])
   */
  for (arma::uword k = 0; k < d.n_elem; ++k) {
    if (d[k] > threshold) {
      has_positive_singular_value = true;

      inverse += (1.0 / d[k]) * (V.col(k) * U.col(k).t());
    }
  }

  /*
   * If there are no valid singular values, return an all-zero matrix.
   */
  NumericMatrix output(static_cast<int>(nc), static_cast<int>(nr));

  if (!has_positive_singular_value) {
    std::fill(output.begin(), output.end(), 0.0);

    return output;
  }

  /*
   * Both arma::mat and R matrix are column-major, so the underlying
   * data can be copied directly.
   */
  std::copy(inverse.begin(), inverse.end(), output.begin());

  return output;
}
