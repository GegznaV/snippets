# ======================================================================== ~~~~
# Install snippets ------------------------------------------------------- ====
# ======================================================================== ~~~~

#' @name install-snippets
#' @title Install snippets
#' @description
#' Replace current file with snippets with the other file.
#'
#' @inheritParams match_snippet_type
#'
#' @param package  (character) The name of an R package.
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
#'   # Replace your R and Markdown snippets with those in package "snippets":
#'   install_snippets_from_package("snippets")
#'   install_snippets_from_package("snippets", type = "r")
#'   install_snippets_from_package("snippets", type = "markdown")
#'
#'   # Check if back-up copies exist:
#'   list_snippet_file_backups(type = "r")
#'   list_snippet_file_backups(type = "markdown")
#'
#'   remove_snippet_backup_duplicates()
#' }
install_snippets_from_package <- function(package,
                                          type = "auto-detect-all",
                                          subdir = "", backup = TRUE) {
  if (missing(package)) {
    usethis::ui_stop(
      "{usethis::ui_field('package')} name is missing. Please, specify it."
    )
  }

  from_dir <- get_path_snippets_dir_of_pkg(package = package, subdir)

  if (any(type == "auto-detect-all")) {
    all_files <- fs::dir_ls(from_dir, regexp = "[.]snippets$", type = "file")
    if (length(all_files) == 0) {
      usethis::ui_oops(
        "No snippet files were found in {usethis::ui_path(from_dir)}"
      )
      usethis::ui_stop(paste0(
        "Should this directory of package '{usethis::ui_field(package)}' ",
        "contain snippets?"
      ))
    }
    type <- sub(".*/(.*?)[.]snippets$", "\\1", all_files)
  }

  install_snippets_from_dir(type = type, from_dir = from_dir, backup = backup)
}

#' @rdname install-snippets
#' @export
install_snippets_from_dir <- function(from_dir = ".",
                                      type = get_default_snippet_types(),
                                      backup = TRUE) {
  from_dir <- fs::path(from_dir)
  if (!fs::dir_exists(from_dir)) {
    usethis::ui_oops("Directory was not found: {usethis::ui_path(from_dir)}  ")
    usethis::ui_stop("Please, select directory that exists.")
  }

  type <- match_snippet_type(type, several.ok = TRUE)

  replacement <-
    path_snippets_file(dir = from_dir, type = type, several.ok = TRUE)

  f_exists <- fs::file_exists(replacement)

  if (any(!f_exists)) {
    f_missing <- crayon::red(fs::path_file(replacement[!f_exists]))

    usethis::ui_stop(paste0(
      "In directory {usethis::ui_path(from_dir)},\n",
      "the following files with snippets are not present: ",
      "{paste(f_missing, collapse = ', ')}. \n",
      "Did you specify snippet types correctly?"
    ))
  }

  # Create a back-up copy
  if (backup) {
    backup_rstudio_snippets(type = type)
  }

  # Copy/Overwrite the file
  original <- path_rstudio_snippets_file(type = type, several.ok = TRUE)
  create_rstudio_snippets_dir()

  status_updated <- FALSE
  for (i in seq_along(original)) {
    is_copied <- file.copy(from = replacement[i], to = original[i], overwrite = TRUE)
    orig_path <- usethis::ui_path(original[i])

    if (is_copied) {
      usethis::ui_done(
        "File with {crayon::green(type[i])} snippets was updated:\n {orig_path}"
      )
      status_updated <- TRUE
    } else {
      usethis::ui_info(
        "File with {crayon::red(type[i])} snippets was not changed:\n {orig_path}"
      )
    }
  }

  if (status_updated) {
    cat("\n")
    usethis::ui_info(paste0(
      "You will be able to use the snippets after ",
      '{usethis::ui_field("RStudio")} is ',
      '{crayon::underline("closed and reopened")}.'
    ))
  }
}
