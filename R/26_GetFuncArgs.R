#' @title Get Function Arguments from Calling Context
#'
#' @description Retrieves the arguments passed to the calling function, optionally filtering by name or returning only argument names.
#'
#' @param exclude A character or interger vector of argument names to exclude from the result. Default is `NULL`.
#' @param name_only Logical. If `TRUE`, returns only argument names. If `FALSE`, returns the full argument list. Default is `FALSE`.
#' @param call The call expression to extract arguments from. Default is `rlang::caller_call()`.
#' @param func which function to seek
#' @param dots_expand Logical. Whether to expand `...` arguments. Default is `TRUE`.
#' @param envir The environment in which to evaluate the call. Default is `rlang::caller_env()`.
#'
#' @return If `name_only` is `TRUE`, a character vector of argument names. Otherwise, a named list of arguments.
#'
#' @examples
#' \dontrun{
#' inner_func <- function(a, b, ...) {
#'   GetFuncArgs(exclude = "b", name_only = TRUE)
#' }
#' inner_func(1, 2, 3, 4)  # Returns c("a", "...")
#' }
#'
#' @export
GetFuncArgs <- function(
  exclude = NULL,
  name_only = FALSE,
  func = rlang::caller_fn(),
  call = rlang::caller_call(),
  dots_expand = TRUE,
  envir = rlang::caller_env()
) {
  cl <- rlang::call_match(
    call = call,
    fn = func,
    defaults = TRUE,
    dots_env = envir,
    dots_expand = TRUE
  )
  args_list <- as.list(cl)[-1L]
  if (length(exclude) != 0L) {
    if (is.character(exclude)) {
      args_list <- args_list[!names(args_list) %in% exclude]
    } else if (is.numeric(exclude)) {
      args_list <- args_list[-exclude]
    } else {
      cli::cli_abort(c(
        "x" = "GetFuncArgs: `exclude` cannot be {.cls {class(exclude)}}",
        ">" = "Expect {.cls {c('character','integer','numeric')}}"
      ))
    }
  }
  if (name_only) {
    return(names(args_list))
  }

  lapply(args_list, eval, envir = envir)
}
