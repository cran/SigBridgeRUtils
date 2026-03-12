#' @title Match Functions to Argument List
#' @description
#' Identifies functions compatible with a given set of named arguments. A function
#' is considered compatible if:
#' \itemize{
#'   \item It has a \code{...} parameter (\strong{loose matching}), OR
#'   \item All argument names in \code{args_list} exist in its formal parameters
#'         (\strong{strict matching}, default behavior).
#' }
#'
#' @param args_list Named list of arguments to match. Must have non-empty names
#'   when non-empty.
#' @param ... Functions to test for compatibility.
#' @param name_only Logical. If \code{TRUE}, return character vector of function
#'   names/identifiers instead of function objects. Default: \code{FALSE}.
#' @param top_one_only Logical. If \code{TRUE}, return only the single best-matching
#'   function (ranked by number of matched parameters and parameter position).
#'   Default: \code{FALSE}.
#' @param dots_enabled Logical. If \code{TRUE}, enable loose matching: any function
#'   with a \code{...} parameter is considered compatible regardless of other
#'   parameter names. Default: \code{FALSE} (strict matching).
#'
#' @return
#'   \itemize{
#'     \item \code{name_only = FALSE, top_one_only = FALSE}: List of compatible function objects
#'     \item \code{name_only = TRUE, top_one_only = FALSE}: Character vector of function identifiers
#'           (named functions retain their symbol name; anonymous functions become \code{"anonymous_<index>"})
#'     \item \code{top_one_only = TRUE}: Single function object or name (depending on \code{name_only})
#'   }
#'
#' @examples
#' \dontrun{
#' f1 <- function(a, b) a + b
#' f2 <- function(x, y, ...) x * y
#' f3 <- function(p, q) p - q
#'
#' args <- list(a = 1, b = 2)
#'
#' # Strict matching (default): returns f1 only
#' MatchFunc2Args(args, f1, f2, f3)
#'
#' # Loose matching: returns f1 and f2 (both accept 'a' and 'b' via strict match or ...)
#' MatchFunc2Args(args, f1, f2, f3, dots_enabled = TRUE)
#'
#' # Return only function names
#' MatchFunc2Args(args, f1, f2, name_only = TRUE)
#' # Returns: c("f1", "f2") when dots_enabled=TRUE
#' }
#' @export
#' @importFrom data.table `%chin%` `:=`
#' @seealso [FilterArgs4Func()] for filtering arguments to a function.(Reverse of this function)
MatchFunc2Args <- function(
  args_list,
  ...,
  name_only = FALSE,
  top_one_only = FALSE,
  dots_enabled = FALSE
) {
  # Validate args_list has proper names when non-empty
  if (length(args_list) > 0) {
    if (is.null(names(args_list)) || any(names(args_list) == "")) {
      cli::cli_abort(c(
        "x" = "`args_list` must be a named list with non-empty names for all elements when non-empty."
      ))
    }
  }

  # Capture actual function objects
  dots_funcs <- rlang::list2(...)

  # Handle empty ... case
  if (length(dots_funcs) == 0L) {
    return(if (name_only) character(0L) else list())
  }

  func_names <- names(rlang::enquos(..., .named = TRUE))
  names(dots_funcs) <- func_names
  func_formals <- lapply(dots_funcs, formals)
  names(func_formals) <- func_names

  guess <- purrr::imap(
    .x = func_formals,
    .f = \(lst, func_name) {
      # * This function contains a ... parameter, so it can accept any arguments
      # * In this case, we find the position of arguments

      has_dots <- "..." %chin% names(lst)

      has_these_args <- names(lst) %chin% names(args_list)

      positions <- which(has_these_args) #
      count <- sum(has_these_args) # number of matched args

      if (length(positions) == 0) {
        # integre(0), not found
        list(
          func_name = func_name,
          position_sum = 0,
          arg_count = 0,
          has_dots = has_dots
        )
      } else {
        list(
          func_name = func_name,
          position_sum = sum(positions),
          arg_count = count,
          has_dots = has_dots
        )
      }
    }
  )

  guess_dt <- data.table::rbindlist(guess)
  data.table::setorder(guess_dt, -arg_count, position_sum, has_dots)
  guess_dt <- guess_dt[arg_count > 0]

  if (dots_enabled) {
    # because dots can accrpt any args,
    # so all funcs with `has_dots` are returned.
    if (name_only) {
      return(guess_dt$func_name[guess_dt$has_dots])
    } else {
      return(dots_funcs[guess_dt$func_name[guess_dt$has_dots]])
    }
  }

  # handle without `has_dots`, which means strict matching
  logical_vec <- vapply(
    X = func_formals,
    FUN = \(lst) {
      modified <- utils::modifyList(lst, args_list, keep.null = TRUE)
      if (length(modified) != length(lst)) FALSE else TRUE
    },
    FUN.VALUE = logical(1L)
  )

  # filter out funcs that cannot exactly match all args
  names(logical_vec) <- func_names
  guess_dt[, exactly_matched := logical_vec[func_name]]
  guess_dt <- guess_dt[guess_dt$exactly_matched]

  # return one value
  if (top_one_only) {
    if (
      all(
        guess_dt[1, .(position_sum, arg_count)] ==
          guess_dt[2, .(position_sum, arg_count)]
      )
    ) {
      cli::cli_warn(
        "Arguments provided is not enough to select a function, still return the first function but result may differ from expected"
      )
    }
    if (name_only) {
      return(guess_dt$func_name[1])
    } else {
      return(dots_funcs[[guess_dt$func_name[1]]])
    }
  }

  if (name_only) {
    return(guess_dt$func_name)
  }

  dots_funcs[guess_dt$func_name]
}
