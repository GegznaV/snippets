# ======================================================================== ~~~~
# Install snippets ------------------------------------------------------- ====
# ======================================================================== ~~~~

#' @name install-snippets
#' @title Install snippets.
#' @description
#' Replace current file with snippets with the other file.
#'
#' @inheritParams match_snippet_type
#'
#' @param package  (character) The name of R package.
#' @param subdir   (character) The sub-directory with replacement file(s).
#' @param from_dir (character) The directory with replacement file.
#' @param backup (logical) Indication if a back-up copy should be created.
#'
#' @concept install snippets
#'
#' @export
#'
#' @examples
#' if (FALSE) {
#'
#' # Replace your R and Markdown snippets with those in package "snippets":
#' install_snippets_from_package("snippets", type = "r",        backup = TRUE)
#' install_snippets_from_package("snippets", type = "markdown", backup = TRUE)
#'
#' # Check if back-up copies exist:
#' list_snippet_file_backups(type = "r")
#' list_snippet_file_backups(type = "markdown")
#'
#' }
install_snippets_from_package <- function(package = "snippets",
  type = get_default_snippet_types(), subdir = "", backup = TRUE) {

  from_dir <- get_pkg_snippets_dir(subdir, package = package)
  install_snippets_from_dir(type = type, from_dir = from_dir, backup = backup)
}

#' @rdname install-snippets
#' @export
install_snippets_from_dir <- function(from_dir = ".",
  type = get_default_snippet_types(), backup = TRUE) {

  from_dir <- fs::path(from_dir)
  if (!fs::dir_exists(from_dir)) {
    usethis::ui_oops("Directory was not found: {usethis::ui_path(from_dir)}  ")
    usethis::ui_stop("Please, select directory that exists.")
  }

  type <- match_snippet_type(type, several.ok = TRUE)

  replacement <-
    get_path_to_snippets_file(dir = from_dir, type = type, several.ok = TRUE)

  f_exists <- fs::file_exists(replacement)

  if (any(!f_exists)) {
    f_missing <- crayon::red(fs::path_file(replacement[f_exists]))

    usethis::ui_stop(paste0(
      "In directory {usethis::ui_path(from_dir)},\n",
      "these files with new snippets are not present: {paste(f_missing, collapse = ', ')}.  "
    ))
  }

  # Create a back-up copy
  if (backup) {
    backup_rs_snippets(type = type)
  }

  # Copy/Overwrite the file
  original <- get_path_to_rs_snippet_file(type = type, several.ok = TRUE)
  create_rs_snippets_dir()

  status_updated <- FALSE
  for (i in seq_along(original)) {
    is_copied <- file.copy(from = replacement[i], to = original[i], overwrite = TRUE)
    orig_path <- usethis::ui_path(original[i])

    if (is_copied) {
      usethis::ui_done("File with {crayon::green(type[i])} snippets was updated: {orig_path}")
      status_updated <- TRUE
    } else {
      usethis::ui_info("File with {crayon::red(type[i])} snippets was not changed: {orig_path}")
    }
  }

  if (status_updated) {
    cat("\n")
    usethis::ui_info(paste0(
      'You will be able to use the snippets after ',
      '{usethis::ui_field("RStudio")} is ',
      '{crayon::underline("closed and reopened")}.'
    ))
  }
}
