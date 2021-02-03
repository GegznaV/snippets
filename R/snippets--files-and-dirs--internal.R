# ======================================================================== ~~~~
# Files and directories for snippets ------------------------------------- ====
# ======================================================================== ~~~~

#' Make a filename for snippets
#'
#' Create a string with a filename that is appropriate for certain type of
#' RStudio snippets.
#'
#' @inheritParams match_snippet_type
#'
#' @return String with a filename.
#  @export
#' @noRd
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


#' Create directory for RStudio snippets
#'
#' @return Invisibly returns the path to created directory.
#'         See also [fs::dir_create()].
#  @export
#' @noRd
#'
#' @concept snippet files and dirs
#' @examples
#' if (FALSE) {
#' create_rstudio_snippets_dir()
#' }

create_rstudio_snippets_dir <- function() {
  fs::dir_create(get_path_rstudio_snippets_dir())
}


#' Construct path to file of certain type of snippets
#'
#' Create a string with a path to file of certain type of snippets:
#'
#' - `path_snippets_file()`         in any folder.
#' - `path_rstudio_snippets_file()` in a folder from which RStudio reads
#'   snippets.
#'
#' @inheritParams match_snippet_type
#' @inheritParams get_path_rstudio_snippets_dir
#'
#' @param dir (string) Directory name.
#' @param create (logical) If `TRUE`, as a side effect, the file is created
#'        (if it does not exist). Defaults to `FALSE`.
#'
#' @return (character) Path to file.
#' @concept snippet files and dirs
#'
#  @export
#' @noRd

path_snippets_file <- function(dir, type = get_default_snippet_types(),
  create = FALSE, several.ok = FALSE) {
  force(dir)
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

# @rdname path_snippets_file
# @export
path_rstudio_snippets_file <- function(type = get_default_snippet_types(),
  create = FALSE, several.ok = FALSE, rstudio_version = "auto") {

  path_snippets_file(
    dir = get_path_rstudio_snippets_dir(rstudio_version = rstudio_version),
    type = type,
    create = create,
    several.ok = several.ok
  )
}

#' Does the file with snippets exist?
#'
#' Does the file with certain types of snippets exist in RStudio snippets directory?
#'
#' @inheritParams match_snippet_type
#' @inheritParams get_path_rstudio_snippets_dir
#'
#' @return Returns `TRUE` if file exists and `FALSE` otherwise.
#'
#  @export
#' @noRd
#'
#' @concept snippet files and dirs
#' @examples
#' \dontrun{\donttest{
#' snippets_file_exists("r")
#' snippets_file_exists("markdown")
#' }}

snippets_file_exists <- function(type, rstudio_version = "auto") {
  fs::file_exists(path_rstudio_snippets_file(type, create = FALSE,
    rstudio_version = rstudio_version))
}


#' Get path to snippets in a package
#'
#' Get path to snippets in a package:
#'
#' - `get_path_snippets_dir_of_pkg()` gets path to directory with snippet files
#'  in a package. Defaults to `{path to package}/inst/snippets`. Returns empty
#'  string, if the directory does not exist.
#' - `get_path_snippet_files_of_pkg()` gets paths to all files with snippets
#'  in a package. If the directory of interest does not exist, `NULL` is
#'  returned.
#'
#' @param package (character) Package name.
#'
#' @param ... (character) Path to subdirectory with snippets of interest:
#'        `{path to package}/inst/snippets/{path of subdirectory provided via ...}`
#'
#  @export
#' @noRd
#'
#' @examples
#' get_path_snippets_dir_of_pkg("snippets")
get_path_snippets_dir_of_pkg <- function(package, ...) {
  system.file("snippets", ... , package = package)
}

# @rdname get_path_snippets_dir_of_pkg
# @export
# @examples
#
# get_path_snippet_files_of_pkg("snippets")
get_path_snippet_files_of_pkg <- function(package, ...) {
  folder <- get_path_snippets_dir_of_pkg(package = package, ...)

  if (folder == "") {
    return(NULL)
  } else {
    fs::dir_ls(folder, regexp = "[.]snippets$")
  }
}

