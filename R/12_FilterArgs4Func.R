#' @title Keep Wanted Arguments According to A Function from Dots
#' @description
#' FilterArgs4Func filters a list of arguments to include only those that
#' match the formal arguments of a specified function, with optional support
#' for preserving additional arguments via the \code{keep} parameter.
#' This is useful for preparing argument lists for function calls, especially
#' when dealing with functions that have many optional parameters or when
#' passing arguments through multiple function layers.
#'
#' @param args_list A named list of arguments to filter
#' @param fun The target function whose formal arguments will be used for filtering
#' @param keep Character vector of argument names to preserve regardless of
#'   whether they appear in \code{fun}'s formal parameters. Default is \code{NULL}.
#'   Useful for retaining arguments needed by downstream functions or wrappers
#'   that consume \code{...}.
#'
#' @return A filtered list containing:
#'   \itemize{
#'     \item Arguments from \code{args_list} that match formal parameters of \code{fun}
#'       (excluding the "..." parameter)
#'     \item Additional arguments specified in \code{keep} (if not \code{NULL})
#'   }
#'
#' @details
#' This function is particularly useful in scenarios where you have a large
#' list of parameters and want to pass only the relevant ones to a specific
#' function while preserving certain arguments for downstream processing
#' (e.g., arguments consumed by nested \code{...} parameters).
#'
#' The \code{keep} parameter enables flexible argument forwarding patterns
#' common in wrapper functions and pipeline designs.
#'
#' @examples
#' \dontrun{
#' # Example function with specific parameters
#' example_function <- function(a, b, c = 10, d = 20) {
#'   return(a + b + c + d)
#' }
#'
#' # Create a list with both relevant and irrelevant arguments
#' all_args <- list(
#'   a = 1,
#'   b = 2,
#'   c = 3,
#'   e = 4,  # Not in function formals
#'   f = 5   # Not in function formals
#' )
#'
#' # Basic usage: filter to only include arguments matching function parameters
#' filtered_args <- FilterArgs4Func(all_args, example_function)
#' print(filtered_args)
#' #> $a
#' #> [1] 1
#' #>
#' #> $b
#' #> [1] 2
#' #>
#' #> $c
#' #> [1] 3
#'
#' # Advanced usage: preserve additional arguments for downstream processing
#' filtered_with_keep <- FilterArgs4Func(all_args, example_function, keep = c("e", "f"))
#' print(filtered_with_keep)
#' #> $a
#' #> [1] 1
#' #>
#' #> $b
#' #> [1] 2
#' #>
#' #> $c
#' #> [1] 3
#' #>
#' #> $e
#' #> [1] 4
#' #>
#' #> $f
#' #> [1] 5
#'
#' # Execute with filtered arguments
#' result <- do.call(example_function, filtered_args)
#' print(result)
#' #> [1] 16
#' }
#'
#' @seealso
#' [formals()] for accessing function formal arguments,
#' [do.call()] for executing functions with argument lists,
#' [names()] for working with list names
#' @export
FilterArgs4Func <- function(args_list, fun, keep = NULL) {
  # Extract formal parameter names (excluding "...")
  fun_formals <- names(formals(fun))
  fun_formals <- fun_formals[fun_formals != "..."]

  # Combine function formals with explicitly preserved arguments
  keep_names <- if (is.null(keep)) character(0) else keep
  valid_names <- unique(c(fun_formals, keep_names))

  # Filter args_list to retain only valid names
  args_list[names(args_list) %in% valid_names]
}
