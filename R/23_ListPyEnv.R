# ? ----List Environments as Data Frame----

#' @title List Available Python Environments
#'
#' @description
#' Discovers and lists available Python environments of various types on the system.
#' This generic function provides a unified interface to find Conda environments
#' and virtual environments (venv) through S3 method dispatch.
#'
#' @param env_type Character string specifying the type of environments to list.
#'   One of: `"all"`, `"conda"`, `"venv"`. Defaults to `"all"`.
#' @param timeout Numeric value specifying the timeout in seconds for the Conda
#'   environment discovery process. Defaults to 30 minutes.
#' @param venv_locations Character vector of additional locations to search for
#'   virtual environments. Defaults to `c("~/.virtualenvs", "~/.venvs", "./venv", "./.venv")`.
#' @param verbose Logical value indicating whether to print verbose output.
#'   Defaults to `TRUE`.
#' @param ... For future use.
#'
#' @return
#' A data frame with the following columns:
#' \itemize{
#'   \item `name` - Character vector of environment names
#'   \item `python` - Character vector of paths to Python executables
#'   \item `type` - Character vector indicating environment type (`"conda"` or `"venv"`)
#' }
#' Returns an empty data frame with these columns if no environments are found.
#'
#' @details
#' The function uses S3 method dispatch to handle different environment types:
#'
#' - **`"all"`**: Combines results from all environment types using `rbind()`
#' - **`"conda"`**: Searches for Conda environments using multiple methods:
#'   - Primary: `reticulate::conda_list()` for reliable environment detection
#'   - Fallback: System `conda info --envs` command for broader compatibility
#' - **`"venv"`**: Searches common virtual environment locations including
#'   user directories and project folders
#'
#' Each method includes comprehensive error handling and will return empty
#' results with informative warnings if no environments are found or if
#' errors occur during discovery.
#'
#' @examples
#' \dontrun{
#' # List all Python environments
#' ListPyEnv("all")
#'
#' # List only Conda environments
#' ListPyEnv("conda")
#'
#' # List only virtual environments with custom search paths
#' ListPyEnv("venv", venv_locations = c("~/my_envs", "./project_env"))
#' }
#'
#' @export
ListPyEnv <- function(
  env_type = c("all", "conda", "venv", "virtualenv"),
  timeout = 30000L,
  venv_locations = c("~/.virtualenvs", "~/.venvs", "./venv", "./.venv"),
  verbose = TRUE,
  ...
) {
  UseMethod("ListPyEnv")
}

#' @rdname ListPyEnv
#' @description
#' Default method that lists all Python environments by combining results from
#' Conda and virtual environment discovery methods.
#'
#' @param timeout The maximum timeout time when using system commands, only effective when `env_type=conda`.
#' @param venv_locations Character vector specifying custom directories to search
#'   for virtual environments. Default locations include standard virtualenv
#'   directories and common project locations.
#' @param verbose Logical indicating whether to print verbose output.
#'
#' @export
#'
ListPyEnv.default <- function(
  env_type = c("all", "conda", "venv", "virtualenv"),
  timeout = 30000L,
  venv_locations = c("~/.virtualenvs", "~/.venvs", "./venv", "./.venv"),
  verbose = getFuncOption("verbose") %||% TRUE,
  ...
) {
  env_type <- MatchArg(
    env_type,
    c("all", "conda", "venv", "virtualenv")
  )
  switch(
    env_type,
    "conda" = ListPyEnv.conda(
      timeout = timeout,
      verbose = verbose,
      ...
    ),
    "virtualenv" = ListPyEnv.venv(venv_locations = venv_locations),
    "venv" = ListPyEnv.venv(venv_locations = venv_locations),
    "all" = rbind(
      ListPyEnv.conda(
        timeout = timeout,
        verbose = verbose,
        ...
      ),
      ListPyEnv.venv(
        venv_locations = venv_locations,
        verbose = verbose,
        ...
      )
    ),
    cli::cli_abort(c(
      "x" = "Invalid environment type: {.val {env_type}}",
      "i" = "Valid types are: {.code all}, {.code conda} or {.code venv}"
    ))
  )
}

#' @rdname ListPyEnv
#' @description
#' Discovers Conda environments using multiple detection strategies for maximum
#' reliability. First attempts to use system Conda commands,
#' then falls back to reticulate's built-in Conda interface if Conda command is unavailable or
#' fails. Returns empty data frame if Conda is not available or no environments
#' are found.
#'
#' @export
ListPyEnv.conda <- function(
  env_type = c("all", "conda", "venv", "virtualenv"),
  timeout = 30000L,
  venv_locations = c("~/.virtualenvs", "~/.venvs", "./venv", "./.venv"),
  verbose = getFuncOption("verbose") %||% TRUE,
  ...
) {
  methods <- c(
    system = function() {
      # Method1: system
      process_result <- processx::run(
        command = "conda",
        args = c("info", "--envs"),
        error_on_status = FALSE,
        timeout = timeout,
        cleanup = TRUE,
        windows_verbatim_args = FALSE
      )

      if (process_result$status != 0) {
        error_msg <- if (nzchar(process_result$stderr)) {
          # nolint
          process_result$stderr
        } else {
          process_result$stdout
        }

        cli::cli_abort(c(
          "x" = "Conda command failed with status {process_result$status}:",
          ">" = "{error_msg}"
        ))
      }
      conda_output <- strsplit(process_result$stdout, "\n")[[1]]

      env_lines <- grep(
        "^[a-zA-Z_]",
        conda_output,
        value = TRUE
      )
      env_lines <- gsub("\\*", "", env_lines) |>
        trimws() |>
        strsplit("\\s+")

      if (length(env_lines) == 0) {
        cli::cli_warn(
          "No Conda environments found, return empty result."
        )
        return(data.frame(
          name = character(),
          python = character(),
          type = character(),
        ))
      }

      env_matrix <- do.call(rbind, env_lines)
      env_names <- env_matrix[, 1]
      env_paths <- env_matrix[, 2]

      python_paths <- vapply(env_paths, GetPythonPath, character(1))

      conda_result <- data.frame(
        name = env_names,
        python = python_paths,
        type = "conda",
        stringsAsFactors = FALSE
      )

      if (!is.null(conda_result) && nrow(conda_result) > 0) {
        return(conda_result)
      }

      cli::cli_warn(
        "No conda environments found, return empty result."
      )

      data.frame(
        name = character(),
        python = character(),
        type = character()
      )
    },
    reticulate = function() {
      # Method2: reticulate
      cli::cli_warn(
        "Failed to find conda environments via system command,\\
         trying reticulate as fallback."
      )
      conda_envs <- reticulate::conda_list()

      if (!is.null(conda_envs) && nrow(conda_envs) > 0) {
        conda_envs$type <- "conda"
        return(conda_envs)
      }

      cli::cli_warn(
        "No conda environments found, return empty result."
      )

      data.frame(
        name = character(),
        python = character(),
        type = character()
      )
    },
    default = function() {
      cli::cli_warn(
        "All methods have failed to find the conda environment,\\
         returning empty conda environment result ."
      )
      data.frame(
        name = character(),
        python = character(),
        type = character()
      )
    }
  ) |>
    purrr::map(purrr::safely)

  for (func_name in names(methods)) {
    method_result <- methods[[func_name]]()
    if (is.null(method_result$error) || func_name == "default") {
      return(method_result$result)
    }
  }
}

#' @rdname ListPyEnv
#' @description
#' Discovers virtual environments by searching common venv locations including
#' user directories (`~/.virtualenvs`, `~/.venvs`) and project folders
#' (`./venv`, `./.venv`). Supports custom search paths through the
#' `venv_locations` parameter. Returns empty data frame if no virtual
#' environments are found in the specified locations.
#'
#' @param venv_locations Character vector of directory paths to search for
#'   virtual environments. Default includes standard locations and common
#'   project directories.
#'
#' @export
ListPyEnv.venv <- function(
  env_type = c("all", "conda", "venv", "virtualenv"),
  timeout = 30000L,
  venv_locations = c("~/.virtualenvs", "~/.venvs", "./venv", "./.venv"),
  verbose = getFuncOption("verbose") %||% TRUE,
  ...
) {
  venv_dirs <- c()

  for (location in venv_locations) {
    expanded_path <- path.expand(location)
    if (dir.exists(expanded_path)) {
      dirs <- list.dirs(expanded_path, recursive = FALSE)
      venv_dirs <- c(venv_dirs, dirs)
    }
  }

  if (length(venv_dirs) > 0) {
    return(data.frame(
      name = basename(venv_dirs),
      python = file.path(
        venv_dirs,
        ifelse(
          .Platform$OS.type == "windows",
          "Scripts/python.exe",
          "bin/python"
        )
      ),
      type = "venv"
    ))
  } else if (verbose) {
    cli::cli_warn(
      "No venv found in {.val {venv_locations}}, return empty virtual environment result"
    )
  }

  data.frame(
    name = character(),
    python = character(),
    type = character()
  )
}
