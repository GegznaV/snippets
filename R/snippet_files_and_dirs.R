# ======================================================================== ~~~~
# Files and directories for snippets ------------------------------------- ====
# ======================================================================== ~~~~

#' Make a filename for snippets.
#'
#' Create a string with a filename that is appropriate for certain type of
#' RStudio snippets.
#'
#' @inheritParams match_snippet_type
#'
#' @return String with a filename.
#' @export
#'
#' @concept snippet files and dirs
#' @examples
#' # By default, filename for R snippets is created.
#' make_snippet_filename()
#'
#' make_snippet_filename("markdown")
#'
#' make_snippet_filename(c("r", "markdown"), several.ok = TRUE)

make_snippet_filename <- function(type = get_default_snippet_types(),
  several.ok = FALSE) {
  type <- match_snippet_type(type, several.ok = several.ok)
  stringr::str_glue("{type}.snippets")
}


#' Get path to directory for RStudio snippets.
#'
#' Create a string with a path to directory for RStudio snippets.
#' The directory might not exist.
#'
#' @return (string) Path to directory for RStudio snippets.
#' @export
#'
#' @concept snippet files and dirs
#' @examples
#' get_path_to_rs_snippets_dir()

get_path_to_rs_snippets_dir <- function() {
  # TODO:
  # in RStudio 1.3, the following paths are also available:
  # - on Windiws: fs::path(Sys.getenv("APPDATA"), "RStudio", "snippets")
  #                      AppData/Roaming/RStudio/snippets/
  # - on the oter OS'es:               ~/.config/snippets/
  #

  fs::path_expand_r("~/.R/snippets/")
}


#' Create directory for RStudio snippets.
#'
#' @return Invisibly returns the path to created directory.
#'         See also [fs::dir_create()].
#' @export
#'
#' @concept snippet files and dirs
#' @examples
#' if (FALSE) {
#' create_rs_snippets_dir()
#' }

create_rs_snippets_dir <- function() {
  fs::dir_create(get_path_to_rs_snippets_dir())
}


#' Open directory of RStudio snippets.
#' @export
#' @importFrom utils browseURL
#' @concept snippet files and dirs

open_rs_snippets_dir <- function() {
  create_rs_snippets_dir()
  browseURL(get_path_to_rs_snippets_dir())
}

#' Construct path to file of certain type snippets.
#'
#'  Construct path to file of certain type snippets:
#'
#' - `get_path_to_snippets_file()`    in any folder
#' - `get_path_to_rs_snippet_file()` in RS snippets folder
#'
#' @inheritParams match_snippet_type
#'
#' @param dir (string) Directory name.
#' @param create (logical) If `TRUE`, as a side effect, the file is created
#'        (if it does not exist).
#'
#' @return (character) Path to file.
#' @export
#' @concept snippet files and dirs

get_path_to_snippets_file <- function(dir, type = get_default_snippet_types(),
  create = FALSE, several.ok = FALSE) {

  paths <-
    fs::path(dir, make_snippet_filename(type = type, several.ok = several.ok))

  if (isTRUE(create)) {
    for (path_i in paths) {
      if (!file.exists(path_i)) {
        fs::dir_create(fs::path_dir(path_i))
        fs::file_create(path_i)
      }
    }
  }

  paths
}

#' @rdname get_path_to_snippets_file
#' @export
get_path_to_rs_snippet_file <- function(type = get_default_snippet_types(),
  create = FALSE, several.ok = FALSE) {

  get_path_to_snippets_file(dir = get_path_to_rs_snippets_dir(), type = type,
    create = create, several.ok = several.ok)
}


#' Does file with snippets exist?
#'
#' Does a file with certain types of snippets exist in RStudio snippets directory?
#'
#' @inheritParams match_snippet_type
#'
#' @return Returns `TRUE` if file exists and `FALSE` otherwise.
#' @export
#'
#' @concept snippet files and dirs
#' @examples
#' snippets_file_exists("r")
#' snippets_file_exists("markdown")
snippets_file_exists <- function(type) {
  fs::file_exists(get_path_to_rs_snippet_file(type, create = FALSE))
}

# ~~~~~~~~~ Internal ~~~~~~~~~ -----------------------------------------------

#' @noRd
#' @examples
#' get_path_to_snippets_files()
get_pkg_snippets_dir <- function(..., package = "snippets") {
  system.file("snippets", ... , package = package)
}

#' @noRd
#' @examples
#' get_path_to_snippets_files()
get_path_to_snippets_files <- function(package = "snippets") {
  fs::dir_ls(get_pkg_snippets_dir(package = package), regexp = "[.]snippets$")
}
