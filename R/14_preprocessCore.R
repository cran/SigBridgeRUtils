#' @rdname matrix-stats
#' @title Quantile Normalization
#' @description
#' normalize.quantiles performs quantile normalization on a matrix, transforming
#' the distributions of each column to match a common target distribution.
#' Uses preprocessCore::normalize.quantiles if available, otherwise provides
#' a pure R implementation.
#'
#' @param x A numeric matrix where columns represent samples and rows represent features.
#' @param copy Logical indicating whether to work on a copy of the matrix (TRUE)
#' or modify in-place (FALSE).
#' @param keep.names Logical indicating whether to preserve row and column names.
#' @param ... Additional arguments (currently not used).
#'
#' @return A numeric matrix of the same dimensions as x with quantile-normalized data.
#'
#' @examples
#' mat <- matrix(rnorm(100), nrow = 10, ncol = 10)
#'
#' # Perform quantile normalization
#' normalized_mat <- normalize.quantiles(mat)
#'
#' # Preserve original names
#' rownames(mat) <- paste0("Gene", 1:10)
#' colnames(mat) <- paste0("Sample", 1:10)
#' normalized_with_names <- normalize.quantiles(mat, keep.names = TRUE)
#'
#' @seealso [preprocessCore::normalize.quantiles()] for the underlying implementation
#' @export
normalize.quantiles <- function(x, copy = TRUE, keep.names = FALSE, ...) {
  normalize_quantiles_cpp(beachmat::initializeCpp(x))
}
