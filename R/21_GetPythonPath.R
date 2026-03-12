#' @title  Get Python Executable Path
#' @description
#' This function attempts to find the Python executable within a given environment path.
#' It checks different candidate paths based on the operating system.
#'
#' @param path Character string specifying the path to the environment where Python might be located.
#'
#' @return Character string with the normalized path to the Python executable if found,
#'         otherwise `NA_character_`.
#'
#' @export
GetPythonPath <- function(path) {
  # * path - Character string specifying the path to the environment.
  if (is.na(path)) {
    return(NA_character_)
  }
  candidates <- if (.Platform$OS.type == "windows") {
    c("python.exe", "Scripts/python.exe")
  } else {
    c("bin/python", "bin/python3")
  }
  for (candidate in candidates) {
    full_path <- file.path(path, candidate)
    if (file.exists(full_path)) {
      return(normalizePath(full_path, mustWork = FALSE))
    }
  }
  NA_character_
}
