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
  # if (inherits(X, "sparseMatrix")) {
  #     return(ginv2.sparseMatrix(X, tol = tol, ...))
  # }

  if (inherits(X, "Matrix")) {
    return(Matrix::Matrix(ginv2.default(X, tol = tol, ...)))
  }

  # Base R matrix or coercible to matrix
  if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) {
    cli::cli_abort(c("x" = "'X' must be a numeric or complex matrix"))
  }
  UseMethod("ginv2")
}


#' @title Generalized Inverse for Base R Matrices
#' @description Default method for base R matrices (from MASS::ginv)
#' @rdname ginv2
#' @export
ginv2.default <- function(X, tol = sqrt(.Machine$double.eps), ...) {
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

  Positive <- d > max(tol * d[1L], 0)

  if (!any(Positive)) {
    return(array(0, dim(X)[c(2L, 1L)]))
  }

  if (all(Positive)) {
    v %*% (1 / d * t(u))
  } else {
    v[, Positive, drop = FALSE] %*%
      ((1 / d[Positive]) * t(u[, Positive, drop = FALSE]))
  }
}

# #' @title Generalized Inverse for Sparse Matrix Objects
# #' @description Method for all sparse Matrix package objects
# #' @param method Method to use: "svd" (default) or "qr"
# #' @param return_sparse Whether to return sparse matrix if appropriate
# #' @rdname ginv2
# #' @export
# ginv2.sparseMatrix <- function(
#     X,
#     tol = sqrt(.Machine$double.eps),
#     method = c("auto", "svd", "qr"),
#     return_sparse = TRUE,
#     ...
# ) {
#     method <- match.arg(method)
#     if (method == "auto") {
#         density <- Matrix::nnzero(X) / length(X)
#         if (density < 0.3 && ncol(X) <= 1000) {
#             method <- "qr"
#         } else {
#             method <- "svd"
#         }
#     }

#     if (method == "qr") {
#         if (!inherits(X, "CsparseMatrix")) {
#             X <- as(X, "CsparseMatrix")
#         }

#         qr_x <- Matrix::qr(X)
#         R <- Matrix::qr.R(qr_x)

#         d <- abs(Matrix::diag(R))
#         rank <- sum(d > tol * max(d))

#         if (rank < ncol(X)) {
#             cli::cli_warn(
#                 "Matrix is rank deficient, using regularized solution"
#             )
#             XtX <- Matrix::crossprod(X)
#             reg_param <- tol * mean(Matrix::diag(XtX))
#             XtX_reg <- XtX + reg_param * Matrix::Diagonal(ncol(X))
#             result <- Matrix::solve(XtX_reg, Matrix::t(X))
#         } else {
#             result <- Matrix::solve(R, Matrix::t(Matrix::qr.Q(qr_x)))
#         }
#     } else {
#         # SVD
#         X_dense <- as.matrix(X)
#         svd_result <- svd(X_dense)
#         d <- svd_result$d
#         keep <- d > tol * max(d)
#         d_inv <- rep(0, length(d))
#         d_inv[keep] <- 1 / d[keep]

#         result <- svd_result$v[, keep, drop = FALSE] %*%
#             (d_inv[keep] * t(svd_result$u[, keep, drop = FALSE]))
#     }

#     if (return_sparse) {
#         result_max <- max(abs(result))
#         if (result_max > 0) {
#             zero_threshold <- tol * result_max
#             result[abs(result) < zero_threshold] <- 0
#         }
#         return(Matrix::drop0(Matrix::Matrix(result, sparse = TRUE)))
#     }

#     as.matrix(result)
# }
