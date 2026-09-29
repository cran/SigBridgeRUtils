#' @title Matrix Statistics Functions
#' @name matrix-stats
#' @param na.rm Whether to remove missing values.
#' @description
#' Matrix statistics implemented in C++ through beachmat and tatami. These
#' functions support ordinary, sparse (S4 Matrix), and DelayedArray matrices without
#' coercing them to a dense R matrix.
NULL

.matrix_stats_names <- function(result, x, by_row, useNames) {
  if (useNames) {
    names(result) <- if (by_row) {
      rownames(x = x)
    } else {
      colnames(x = x)
    }
  }
  result
}

.matrix_stats_subset <- function(x, rows, cols) {
  if (!is.null(rows) || !is.null(cols)) {
    rows <- rows %||% seq_len(nrow(x = x))
    cols <- cols %||% seq_len(ncol(x = x))
    x <- x[rows, cols, drop = FALSE]
  }
  x
}

#' @param x A numeric matrix or matrix-like object supported by beachmat.
#' @param na.rm Logical indicating whether to remove missing values.
#' @param ... Additional arguments passed to methods.
#' @return A numeric vector containing the requested statistic.
#' @rdname matrix-stats
#' @export
rowMeans3 <- function(x, na.rm = FALSE, ...) {
  if (is.vector(x = x)) {
    return(mean(x, na.rm = na.rm, ...))
  }
  .matrix_stats_names(
    result = matrix_summary_cpp(
      initialized_matrix = beachmat::initializeCpp(x),
      statistic = "mean",
      by_row = TRUE,
      na_rm = na.rm
    ),
    x = x,
    by_row = TRUE,
    useNames = TRUE
  )
}

#' @rdname matrix-stats
#' @export
colMeans3 <- function(x, na.rm = FALSE, ...) {
  if (is.vector(x = x)) {
    return(mean(x, na.rm = na.rm, ...))
  }
  .matrix_stats_names(
    result = matrix_summary_cpp(
      initialized_matrix = beachmat::initializeCpp(x),
      statistic = "mean",
      by_row = FALSE,
      na_rm = na.rm
    ),
    x = x,
    by_row = FALSE,
    useNames = TRUE
  )
}

#' @rdname matrix-stats
#' @export
rowVars3 <- function(x, na.rm = FALSE, ...) {
  if (is.vector(x = x)) {
    return(stats::var(x, na.rm = na.rm, ...))
  }
  .matrix_stats_names(
    result = matrix_summary_cpp(
      initialized_matrix = beachmat::initializeCpp(x),
      statistic = "var",
      by_row = TRUE,
      na_rm = na.rm
    ),
    x = x,
    by_row = TRUE,
    useNames = TRUE
  )
}

#' @rdname matrix-stats
#' @export
colVars3 <- function(x, na.rm = FALSE, ...) {
  if (is.vector(x = x)) {
    return(stats::var(x, na.rm = na.rm, ...))
  }
  .matrix_stats_names(
    result = matrix_summary_cpp(
      initialized_matrix = beachmat::initializeCpp(x),
      statistic = "var",
      by_row = FALSE,
      na_rm = na.rm
    ),
    x = x,
    by_row = FALSE,
    useNames = TRUE
  )
}

#' @rdname matrix-stats
#' @export
rowSds3 <- function(x, na.rm = FALSE, ...) {
  sqrt(rowVars3(x, na.rm = na.rm, ...))
}

#' @rdname matrix-stats
#' @export
colSds3 <- function(x, na.rm = FALSE, ...) {
  sqrt(colVars3(x, na.rm = na.rm, ...))
}

#' @rdname matrix-stats
#' @param probs Numeric vector of probabilities with values between 0 and 1.
#' @return A matrix of quantiles with ncol(x) rows and length(probs) columns.
#' @export
colQuantiles3 <- function(x, probs = seq(0L, 1L, 0.25), ...) {
  na.rm <- list(...)$na.rm %||% FALSE
  if (is.vector(x = x)) {
    return(stats::quantile(x, probs = probs, ...))
  }
  output <- matrix_quantiles_cpp(
    initialized_matrix = beachmat::initializeCpp(x),
    probabilities = probs,
    na_rm = na.rm
  )
  rownames(x = output) <- colnames(x = x)
  colnames(x = output) <- names(probs) %||% paste0(probs * 100L, "%")
  output
}

#' @rdname matrix-stats
#' @param rows,cols Indices specifying a subset of rows or columns.
#' @param dim. Dimensions of the input matrix.
#' @param useNames Logical indicating whether to preserve output names.
#' @return A numeric vector containing row maxima.
#' @export
rowMaxs3 <- function(
  x,
  rows = NULL,
  cols = NULL,
  na.rm = FALSE,
  dim. = dim(x),
  ...,
  useNames = TRUE
) {
  x <- .matrix_stats_subset(x = x, rows = rows, cols = cols)
  .matrix_stats_names(
    result = matrix_summary_cpp(
      initialized_matrix = beachmat::initializeCpp(x),
      statistic = "max",
      by_row = TRUE,
      na_rm = na.rm
    ),
    x = x,
    by_row = TRUE,
    useNames = useNames
  )
}

#' @rdname matrix-stats
#' @return A numeric vector containing column sums.
#' @export
colSums3 <- function(x, na.rm = FALSE, ...) {
  if (is.vector(x = x)) {
    return(sum(x, na.rm = na.rm, ...))
  }
  .matrix_stats_names(
    result = matrix_summary_cpp(
      initialized_matrix = beachmat::initializeCpp(x),
      statistic = "sum",
      by_row = FALSE,
      na_rm = na.rm
    ),
    x = x,
    by_row = FALSE,
    useNames = TRUE
  )
}

#' @rdname matrix-stats
#' @return A numeric vector containing row sums.
#' @export
rowSums3 <- function(x, na.rm = FALSE, ...) {
  if (is.vector(x = x)) {
    return(sum(x, na.rm = na.rm, ...))
  }
  .matrix_stats_names(
    result = matrix_summary_cpp(
      initialized_matrix = beachmat::initializeCpp(x),
      statistic = "sum",
      by_row = TRUE,
      na_rm = na.rm
    ),
    x = x,
    by_row = TRUE,
    useNames = TRUE
  )
}

#' @rdname matrix-stats
#' @return A numeric vector containing row medians.
#' @export
rowMedians3 <- function(
  x,
  rows = NULL,
  cols = NULL,
  na.rm = FALSE,
  dim. = dim(x),
  ...,
  useNames = TRUE
) {
  if (is.vector(x = x)) {
    return(stats::median(x, na.rm = na.rm, ...))
  }
  x <- .matrix_stats_subset(x = x, rows = rows, cols = cols)
  .matrix_stats_names(
    result = matrix_summary_cpp(
      initialized_matrix = beachmat::initializeCpp(x),
      statistic = "median",
      by_row = TRUE,
      na_rm = na.rm
    ),
    x = x,
    by_row = TRUE,
    useNames = useNames
  )
}

#' @rdname matrix-stats
#' @return A numeric vector containing column medians.
#' @export
colMedians3 <- function(
  x,
  rows = NULL,
  cols = NULL,
  na.rm = FALSE,
  dim. = dim(x),
  ...,
  useNames = TRUE
) {
  if (is.vector(x = x)) {
    return(stats::median(x, na.rm = na.rm, ...))
  }
  x <- .matrix_stats_subset(x = x, rows = rows, cols = cols)
  .matrix_stats_names(
    result = matrix_summary_cpp(
      initialized_matrix = beachmat::initializeCpp(x),
      statistic = "median",
      by_row = FALSE,
      na_rm = na.rm
    ),
    x = x,
    by_row = FALSE,
    useNames = useNames
  )
}
