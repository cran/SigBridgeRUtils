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
  if (rlang::is_installed("preprocessCore")) {
    return(getExportedValue("preprocessCore", "normalize.quantiles")(
      x,
      copy,
      keep.names
    ))
  }

  if (!is.matrix(x)) {
    cli::cli_abort(c(
      "x" = "Matrix expected in normalize.quantiles",
      ">" = "Input is a {.field {class(x)}}"
    ))
  }
  rows <- nrow(x)
  cols <- ncol(x)

  if (copy) {
    mat <- matrix(as.numeric(x), rows, cols)
  } else {
    mat <- x
    if (is.integer(mat)) {
      mat <- matrix(as.double(mat), rows, cols)
    }
  }

  if (keep.names) {
    orig_rownames <- rownames(x)
    orig_colnames <- colnames(x)
  }

  na_positions <- is.na(mat)

  sorted_mat <- if (rlang::is_installed("Rfast")) {
    Rfast::colSort(mat)
  } else {
    apply(mat, 2, sort, na.last = TRUE)
  }

  target_dist <- rowMeans3(sorted_mat, na.rm = TRUE)

  rank_mat <- apply(mat, 2, function(col) {
    match(col, sort(col, na.last = NA))
  })

  for (j in seq_len(cols)) {
    valid_idx <- !is.na(rank_mat[, j])
    mat[valid_idx, j] <- target_dist[rank_mat[valid_idx, j]]
  }

  mat[na_positions] <- NA

  if (keep.names) {
    rownames(mat) <- orig_rownames
    colnames(mat) <- orig_colnames
  }

  mat
}
