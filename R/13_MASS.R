#' @title Generalized Inverse (Moore-Penrose Inverse)
#' @description
#' Compute the Moore-Penrose generalized inverse of a matrix.
#' This is an S3 generic function with methods for base matrices,
#' dense Matrix objects, and sparse Matrix objects.
#'
#' @param X A numeric or complex matrix
#' @param tol Tolerance for determining rank. Default is sqrt(.Machine$double.eps)
#' @param ... Additional arguments passed to methods
#'
#' @return The generalized inverse of X
#'
#' @examples
#' \dontrun{
#' # Base R matrix
#' m <- matrix(c(1, 2, 3, 4, 5, 6), 2, 3)
#' ginv2(m)
#'
#' # Dense Matrix
#' library(Matrix)
#' dm <- Matrix(m, sparse = FALSE)
#' ginv2(dm)
#'
#' # Sparse Matrix
#' sm <- Matrix(m, sparse = TRUE)
#' ginv2(sm)
#' }
#' @export
#'
ginv2 <- function(X, tol = sqrt(.Machine$double.eps), ...) {
  is_s4matrix <- inherits(X, "Matrix")
  is_delayedmatrix <- inherits(X, "DelayedMatrix")
  is_matrix <- is.matrix(X)
  is_vector <- is.vector(X)

  is_supported_matrix <- is_s4matrix ||
    is_delayedmatrix ||
    is_matrix ||
    is_vector

  if (length(dim(X)) > 2L || !is_supported_matrix) {
    stop("'X' must be a numeric or complex matrix")
  }

  if (is_s4matrix) {
    return(Matrix::Matrix(ginv2_beachmat(X, tol = tol, ...)))
  }
  if (is_delayedmatrix) {
    rlang::check_installed("DelayedArray")
    return(DelayedArray::DelayedArray(ginv2_beachmat(X, tol = tol, ...)))
  }

  if (!is_matrix) {
    X <- as.matrix(X)
  }

  if (is.complex(X)) {
    return(ginv2_default(X, tol = tol, ...))
  }

  ginv2_beachmat(X, tol = tol, ...)
}

#' @rdname ginv2
ginv2_beachmat <- function(X, tol = sqrt(.Machine$double.eps), ...) {
  initialized <- beachmat::initializeCpp(X)

  result <- ginv_cpp(
    initialized,
    tol
  )

  dimnames_X <- dimnames(X)
  if (!is.null(dimnames_X)) {
    dimnames(result) <- rev(dimnames_X)
  }

  result
}


#' @rdname ginv2
#' @export
ginv2_default <- function(X, tol = sqrt(.Machine$double.eps), ...) {
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
