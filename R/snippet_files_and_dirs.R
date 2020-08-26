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
  if (rstudioapi::versionInfo()$version > "1.3") {
    # For RStudio 1.3 or newer
    if (get_os_type() == "windows") {
      # on Windows
      fs::path(Sys.getenv("APPDATA"), "RStudio", "snippets")

    } else {
      # on Unix (MacOS, Linux)
      fs::path_expand_r("~/.config/snippets/")
    }

  } else {
    # For RStudio 1.2, 1.1, etc. (i.e., versions before 1.3)
    fs::path_expand_r("~/.R/snippets/")
  }
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


#' Get path to snippets in a package.
#'
#' Get path to snippets in a package:
#'
#' - `path_to_snippets_dir_of_pkg()` gets path to directory with snippet files
#'  in a package. Defaults to `{path to package}/inst/snippets`. Returns empty
#'  string, if the directory does not exist.
#' - `path_to_snippets_files_of_pkg()` gets paths to all files with snippets
#'  in a package. If the directory of interest does not exist, `NULL` is
#'  returned.
#'
#' @param package (character) Package name.
#'
#' @param ... (character) Path to subdirectory with snippets of interest:
#'        `{path to package}/inst/snippets/{path of subdirectory provided via ...}`
#'
#' @export
#' @examples
#' path_to_snippets_dir_of_pkg("snippets")
path_to_snippets_dir_of_pkg <- function(package, ...) {
  system.file("snippets", ... , package = package)
}

#' @rdname path_to_snippets_dir_of_pkg
#' @export
#' @examples
#'
#' path_to_snippets_files_of_pkg("snippets")
path_to_snippets_files_of_pkg <- function(package, ...) {
  folder <- path_to_snippets_dir_of_pkg(package = package, ...)

  if (folder == "") {
    return(NULL)
  } else {
    fs::dir_ls(folder, regexp = "[.]snippets$")
  }
}

# ~~~~~~~~~ Internal ~~~~~~~~~ -----------------------------------------------

