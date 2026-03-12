#' @title Null Coalescing Operator
#' @description
#' The %||% operator provides a convenient way to handle NULL values by
#' returning a default value when the left-hand side is NULL. This is particularly
#' useful for providing fallback values in function arguments and data processing.
#'
#' @name NULL_or
#'
#' @param lhs Left-hand side value to check for NULL
#' @param rhs Right-hand side value to return if lhs is NULL
#'
#' @return Returns lhs if it is not NULL, otherwise returns rhs.
#'
#' @details
#' This operator follows the same semantics as the null coalescing operator
#' found in other programming languages (e.g., ?? in C#, ?: in JavaScript).
#' It provides a concise way to specify default values without verbose
#' if-else statements.
#'
#' @examples
#' \dontrun{
#' # Basic usage with NULL values
#' NULL %||% "default value"
#' # Returns "default value"
#'
#' "actual value" %||% "default value"
#' # Returns "actual value"
#'
#' # Practical use in functions
#' my_function <- function(x = NULL) {
#' x <- x %||% "default_parameter"
#' print(x)
#' }
#'
#' my_function() # Prints "default_parameter"
#' my_function("custom_value") # Prints "custom_value"
#'
#' # Handling potentially NULL results
#' result <- tryCatch(
#' some_operation(),
#' error = function(e) NULL
#' )
#'
#' final_value <- result %||% "fallback"
#' }
#'
#' @seealso
#' [is.null()] for checking NULL values,
#' [ifelse()] for more complex conditional logic
NULL

#' @rdname NULL_or
#' @export
`%||%` <- function(lhs, rhs) {
    if (is.null(lhs)) {
        return(rhs)
    }
    lhs
}
