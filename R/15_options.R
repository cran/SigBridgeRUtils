#' @title Configuration Functions for SigBridgeR Package
#' @description
#' These functions provide a centralized configuration system for the SigBridgeR
#' package, allowing users to set and retrieve package-specific options with
#' automatic naming conventions.
#'
#' @section Available Opions:
#' - `verbose`: A logical value indicating whether to print verbose messages, defaults to `TRUE`
#' - `timeout`: An integer specifying the timeout in seconds for parallel processing, defaults to `180L``
#' - `seed`: An integer specifying the random seed for reproducible results, defaults to `123L``
#'
#'
#' @name SigBridgeR_Function_Setting
NULL

#' @rdname SigBridgeR_Function_Setting
#' @description
#' setFunOption sets one or more configuration options for the SigBridgeR package.
#' Options are automatically prefixed with "SigBridgeR." if not already present,
#' ensuring proper namespace isolation.
#'
#' @param ... Named arguments representing option-value pairs. Options can be
#' provided with or without the "SigBridgeR." prefix. If the prefix is missing,
#' it will be automatically added.
#'
#' @return Invisibly returns NULL. The function is called for its side effects
#' of setting package options.
#'
#' @examples
#' # Set options with automatic prefixing
#' setFuncOption(verbose = TRUE)
#'
#' @export
setFuncOption <- function(...) {
  opts <- rlang::list2(...)
  if (length(opts) > 0) {
    opt_names <- names(opts)
    needs_prefix <- !startsWith(opt_names, "SigBridgeR.")
    opt_names[needs_prefix] <- paste0(
      "SigBridgeR.",
      opt_names[needs_prefix]
    )
    names(opts) <- opt_names

    purrr::walk2(names(opts), opts, checkFuncOption)

    options(opts)
  }

  invisible()
}

#' @rdname SigBridgeR_Function_Setting
#' @description
#' getFuncOption retrieves configuration options for the SigBridgeR package.
#' The function automatically handles the "SigBridgeR." prefix, allowing users
#' to reference options with or without the explicit prefix.
#'
#' @param option Character string specifying the option name to retrieve.
#' The "SigBridgeR." prefix is optional and will be added if missing.
#' @param default The default value to return if the option is not set.
#' Defaults to NULL.
#'
#' @return The value of the specified option if it exists, otherwise the
#' provided default value.
#'
#' @examples
#' # Retrieve options with automatic prefixing
#' getFuncOption("verbose")
#'
#' @export
getFuncOption <- function(option = NULL, default = NULL) {
  if (is.null(option)) {
    all_options <- options()
    sigbridger_options <- all_options[startsWith(
      names(all_options),
      "SigBridgeR."
    )]
    return(sigbridger_options)
  }

  if (!startsWith(option, "SigBridgeR.")) {
    option <- paste0("SigBridgeR.", option)
  }
  getOption(option, default = default)
}

#' @rdname SigBridgeR_Function_Setting
#' @description
#' checkFuncOption validates configuration options for the SigBridgeR package.
#' This function ensures that all options meet type and value requirements
#' before they are set in the global options.
#'
#' @param option Character string specifying the option name to check
#' @param value The proposed value to assign to the option
#' @param call The execution environment of a currently running function
#'
#' @return No return value. The function throws an error if the value doesn't
#' meet the required specifications for the given option.
#'
#' @details
#' This function performs the following validations:
#' \describe{
#' \item{verbose}{Must be single logical values (TRUE/FALSE)}
#' \item{timeout, seed}{Must be single integer values}
#' }
#'
#' The function is automatically called by setFuncOption to ensure all
#' configuration options are valid before they are set.
#'
checkFuncOption <- function(option, value, call = rlang::caller_env()) {
  if (!startsWith(option, "SigBridgeR.")) {
    option <- paste0("SigBridgeR.", option)
  }
  checker <- list(
    'scalar_logical' = function(x) {
      if (!rlang::is_scalar_logical(x)) {
        cli::cli_abort(
          c(
            "x" = "{.var {option}} must be a single logical value.",
            ">" = "Current value is {.val {x}} ({.type {class(x)}})."
          ),
          call = call
        )
      }
    },
    'scalar_integer' = function(x) {
      if (!rlang::is_scalar_integer(x)) {
        cli::cli_abort(
          c(
            "x" = "{.var {option}} must be an integer value.",
            ">" = "Current value is {.val {x}} ({.type {class(x)}})."
          ),
          call = call
        )
      }
    },
    'scalar_character' = function(x) {
      if (!rlang::is_scalar_character(x)) {
        cli::cli_abort(
          c(
            "x" = "{.var {option}} must be a single character string.",
            ">" = "Current value is {.val {x}} ({.type {class(x)}})."
          ),
          call = call
        )
      }
    }
  )
  switch(
    option,
    "SigBridgeR.verbose" = checker$scalar_logical(value),
    "SigBridgeR.timeout" = ,
    "SigBridgeR.seed" = checker$scalar_integer(value),
    cli::cli_abort('Unknown option: {.var {option}}')
  )

  invisible()
}
