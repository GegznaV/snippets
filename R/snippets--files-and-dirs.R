# ======================================================================== ~~~~
# Files and directories for snippets ------------------------------------- ====
# ======================================================================== ~~~~

#' Edit RStudio Snippets
#'
#' Edit file with RStudio snippets.
#'
#' @inheritParams match_snippet_type
#' @inheritParams get_path_rstudio_snippets_dir
#'
#' @export
#'
#' @concept snippet files and dirs
#'
# @seealso [path_rstudio_snippets_file()]
#'
#' @examples
#' \dontrun{\donttest{
#' open_rstudio_snippets_file("r")
#'
#' open_rstudio_snippets_file("markdown")
#' }}
#'
open_rstudio_snippets_file <- function(type, rstudio_version = "auto") {
  force(type)
  type <- match_snippet_type(type, several.ok = FALSE)
  file <- path_rstudio_snippets_file(type = type, rstudio_version = rstudio_version)
  not_found <- !fs::file_exists(type)
  if (any(not_found)) {
    type_txt <- usethis::ui_field(type)
    usethis::ui_stop(paste0(
      "The file with user-defined {type_txt} snippets is not found: \n",
      "{usethis::ui_path(file[not_found])} \n",
      "This means that the default {type_txt} snippets are used in RStudio."
    ))
  }
  rstudioapi::navigateToFile(file)
}


#' Get path to directory for RStudio snippets
#'
#' Create a string with a path to directory for RStudio snippets.
#' The directory might not exist.
#'
#' @param rstudio_version Numeric version of RStudio (the output is different
#'        for RStudio < 1.3 and 1.3 or newer series). If `"current"`, the
#'        current version of RStudio will be determined automatically.
#'
#' @return (string) Path to directory for RStudio snippets.
#' @export
#'
#' @concept snippet files and dirs
#' @examples
#' \dontrun{\donttest{
#' # Regularly, you should use this:
#' get_path_rstudio_snippets_dir()
#' }}
#'
#' # For testing purposes:
#' get_path_rstudio_snippets_dir("1.4.1103")
#' get_path_rstudio_snippets_dir("1.2.5044")

get_path_rstudio_snippets_dir <- function(rstudio_version = "current") {

  if (rstudio_version %in% c("current", "auto")) {
    rstudio_version <- rstudioapi::versionInfo()$version

  } else {
    rstudio_version <- as.numeric_version(rstudio_version)
  }

  if (rstudio_version > "1.3") {
    # For RStudio 1.3 or newer~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (get_os_type() == "windows") {
      # on Windows
      base <- fs::path(Sys.getenv("APPDATA"), "RStudio")

    } else {
      # on Unix (MacOS, Linux)
      base <- fs::path_expand_r("~/.config/rstudio")
    }

    base <- Sys.getenv("XDG_CONFIG_HOME",     unset = base)
    base <- Sys.getenv("RSTUDIO_CONFIG_HOME", unset = base)
    fs::path(base, "snippets")

  } else {
    # For RStudio 1.2, 1.1, etc. (i.e., versions before 1.3) ~~~~~~~~~~~~~~~~~
    fs::path_expand_r("~/.R/snippets/")
  }
}


#' Open directory of RStudio snippets
#' @export
#' @importFrom utils browseURL
#' @concept snippet files and dirs

open_rstudio_snippets_dir <- function() {
  create_rstudio_snippets_dir()
  browseURL(get_path_rstudio_snippets_dir())
}


