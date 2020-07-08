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
#' path_to_rs_snippets_dir()

path_to_rs_snippets_dir <- function() {
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
  fs::dir_create(path_to_rs_snippets_dir())
}


#' Open directory of RStudio snippets.
#' @export
#' @importFrom utils browseURL
#' @concept snippet files and dirs

open_rs_snippets_dir <- function() {
  create_rs_snippets_dir()
  browseURL(path_to_rs_snippets_dir())
}

#' Construct path to file of certain type of snippets.
#'
#' Create a string with a path to file of certain type of snippets:
#'
#' - `path_to_snippets_file()`    in any folder.
#' - `path_to_rs_snippets_file()` in a folder from which RStudio reads
#'   snippets.
#'
#' @inheritParams match_snippet_type
#'
#' @param dir (string) Directory name.
#' @param create (logical) If `TRUE`, as a side effect, the file is created
#'        (if it does not exist). Defaults to `FALSE`.
#'
#' @return (character) Path to file.
#' @export
#' @concept snippet files and dirs
#'

path_to_snippets_file <- function(dir, type = get_default_snippet_types(),
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

#' @rdname path_to_snippets_file
#' @export
path_to_rs_snippets_file <- function(type = get_default_snippet_types(),
  create = FALSE, several.ok = FALSE) {

  path_to_snippets_file(dir = path_to_rs_snippets_dir(), type = type,
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
  fs::file_exists(path_to_rs_snippets_file(type, create = FALSE))
}

# ~~~~~~~~~ Internal ~~~~~~~~~ -----------------------------------------------

#' @noRd
#' @examples
#' path_to_snippets_files()
path_to_snippets_dir_of_pkg <- function(package, ...) {
  system.file("snippets", ... , package = package)
}

#' @noRd
#' @examples
#' path_to_snippets_files()
path_to_snippets_files <- function(package = "snippets") {
  fs::dir_ls(path_to_snippets_dir_of_pkg(package = package), regexp = "[.]snippets$")
}
