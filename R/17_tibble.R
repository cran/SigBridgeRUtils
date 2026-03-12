#' @title Row Name Utilities
#' @name rowname-utils
#' @description
#' Functions for converting between column-based row names and explicit row name attributes.
#' These utilities provide convenient ways to handle row names in data frames and matrices,
#' serving as alternatives to tibble's row name handling functions.
NULL

#' @rdname rowname-utils
#' @description
#' Col2Rownames converts a specified column to row names and removes the original column.
#' This is useful when working with data that has identifier columns that should
#' serve as row names rather than regular data columns.
#'
#' @param .data A data frame or matrix-like object
#' @param var Character string specifying the column name to convert to row names.
#' Defaults to "rowname".
#'
#' @return The input object with row names set from the specified column and
#' that column removed from the data.
#'
#' @examples
#' # Create sample data with an ID column
#' df <- data.frame(
#' gene_id = paste0("GENE", 1:5),
#' expression = rnorm(5),
#' p_value = runif(5)
#' )
#'
#' # Convert gene_id column to row names
#' df_with_rownames <- Col2Rownames(df, var = "gene_id")
#' print(rownames(df_with_rownames))
#'
#' @export
Col2Rownames <- function(.data, var = "rowname") {
    rownames(.data) <- .data[[var]]
    .data[[var]] <- NULL
    .data
}

#' @rdname rowname-utils
#' @description
#' Rownames2Col converts row names to an explicit column and removes the row names attribute.
#' This is useful when preparing data for functions that don't handle row names well,
#' or when row names need to be included as regular data for analysis or visualization.
#'
#' @param .data A data frame or matrix-like object with row names
#' @param var Character string specifying the name for the new column that will
#' contain the row names. Defaults to "rowname".
#'
#' @return The input object with row names converted to a new column and the
#' row names attribute set to NULL.
#'
#' @examples
#' # Create sample data with row names
#' df <- data.frame(
#' expression = rnorm(5),
#' p_value = runif(5)
#' )
#' rownames(df) <- paste0("GENE", 1:5)
#'
#' # Convert row names to explicit column
#' df_with_col <- Rownames2Col(df, var = "gene_id")
#' print(df_with_col$gene_id)
#'
#' @export
Rownames2Col <- function(.data, var = "rowname") {
    rownames_col <- data.frame(rownames(.data))
    names(rownames_col) <- var
    rownames(.data) <- NULL
    .data <- cbind(rownames_col, .data)
    .data
}
