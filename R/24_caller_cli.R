#' @title Get Caller Name
#'
#' @description
#' Retrieves the name of the function that called the current execution context.
#' If called from the global environment, returns "global".
#'
#' @param offset Integer. The number of stack frames to go back.
#'        Defaults to 2 (skipping the GetCallerInfo frame and the Wrapper frame
#'        to find the User's function).
#'
#' @return Character string. E.g., "my_function()" or "global".
#'
#' @examples
#' \dontrun{
#' f <- function() { GetCallerInfo() }
#' f() # Returns "f()"
#' }
#'
#' @export
GetCallerInfo <- function(offset = 2) {
  # Calculate absolute frame position
  # sys.nframe() gives current depth.
  # We subtract offset to find the target frame index.
  target_frame <- sys.nframe() - offset

  if (target_frame < 1) {
    return("global")
  }

  # Retrieve the call object that initiated the target frame
  # sys.call(target_frame) gets the call expression (e.g., f(1, 2))
  call_obj <- sys.call(target_frame)

  # Extract function name using rlang for robustness against anonymous functions/formulas
  fn_name <- rlang::call_name(call_obj)

  if (is.null(fn_name)) {
    return("expression") # Handling cases like local({ ... }) or anonymous funcs
  }

  paste0(fn_name, "()")
}

#' @title A Decorator for Adding Caller Info to CLI Functions
#'
#' @description
#' Wraps CLI functions to automatically prepend the caller's identity
#' (function name or 'global') to the output message.
#'
#' @param cli_func A CLI function (e.g., \code{cli_alert_info}).
#'
#' @return A wrapper function that formats output as "\[caller\]: message".
#'
#' @export
AddCaller2cli <- function(cli_func) {
  force(cli_func)

  function(...) {
    # offset = 1 because we are inside this anonymous wrapper function.
    # We look back 1 frame to find who called this wrapper.
    caller_name <- GetCallerInfo(offset = 2)

    messages <- list(...)

    if (length(messages) > 0) {
      if (is.character(messages[[1]])) {
        # Construct the prefix: [caller]:
        prefix <- paste0("[", caller_name, "]: ")
        messages[[1]] <- paste0(prefix, messages[[1]])
      }
    }

    do.call(cli_func, messages)
  }
}

#' @title Create Environment with Caller-Aware CLI Functions
#'
#' @description
#' Generates an environment containing CLI functions that automatically
#' report their caller (Global or Function Name).
#'
#' @param cli_functions Character vector of function names from package \code{cli}.
#'
#' @return An environment with wrapped functions.
#'
#' @examples
#' \dontrun{
#' caller_cli <- CreateCallerCliEnv()
#'
#' # Global context
#' caller_cli$cli_alert_info("Hello")
#' # Output: [global]: Hello
#'
#' # Function context
#' f <- function(x) { caller_cli$cli_alert_success("Result is {x}") }
#' f(100)
#' # Output: [f()]: Result is 100
#' }
#'
#' @export
CreateCallerCliEnv <- function(
  cli_functions = c(
    "cli_alert_info",
    "cli_alert_success",
    "cli_alert_warning",
    "cli_alert_danger",
    "cli_inform"
  )
) {
  cli_env <- new.env()

  purrr::walk(cli_functions, function(func_name) {
    if (exists(func_name, envir = asNamespace("cli"))) {
      orig_func <- get(func_name, envir = asNamespace("cli"))

      # We use eval(substitute(...)) to generate a clean wrapper
      # This ensures the function looks nice and handles arguments correctly
      new_func <- eval(substitute(
        function(..., .envir = parent.frame()) {
          # Calculate caller info immediately.
          # Offset = 1: The user called THIS wrapper function.
          # We want the name of the function that called THIS wrapper.
          caller_id <- GetCallerInfo(offset = 2)

          args <- list(...)
          if (length(args) > 0 && is.character(args[[1]])) {
            # We prepend the caller string directly.
            # We avoid using glue for the prefix to prevent evaluation issues.
            args[[1]] <- paste0(
              "[{.strong ",
              caller_id,
              "}] ", # Using .strong for visual distinction
              args[[1]]
            )
          }

          args$.envir <- .envir
          do.call(ORIG_FUNC, args)
        },
        list(ORIG_FUNC = orig_func)
      ))

      assign(func_name, new_func, envir = cli_env)
    }
  })

  invisible(cli_env)
}
