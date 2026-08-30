# ======================================================================== ~~~~
# Manage snippet back-ups ------------------------------------------------ ====
# ======================================================================== ~~~~

#' Manage back-up copies of snippets
#'
#' - `backup_rstudio_snippets()` creates a back-up of snippets file.
#' - `list_snippet_file_backups()` lists the names of current file with snippets
#'    and its back-ups.
#' - `remove_snippet_backup_duplicates()` removes duplicated backup files.
# - `restore_snippets_from_backup()` restores a back-up file.
#'
#' @inheritParams match_snippet_type
#'
#' @concept backup snippets
#'
#' @export
#'
#' @examples
#' if (FALSE) {
#'   backup_rstudio_snippets("r")
#'   backup_rstudio_snippets("markdown")
#'
#'   list_snippet_file_backups("r")
#' }
#' #
# if (FALSE) {
# # Use name of an existing back-up file
# restore_snippets_from_backup("r.snippets--backup-2019-10-31-01430")
# }
# @return Invisibly returns the name of back-up copy. See [fs::file_copy()].
backup_rstudio_snippets <- function(type) {
  create_rstudio_snippets_dir()
  type <- match_snippet_type(type, several.ok = TRUE)
  file_name <- path_rstudio_snippets_file(type = type, several.ok = TRUE)

  for (i in seq_along(file_name)) {
    backup.tools::create_backup_copy(
      file           = file_name[i],
      backup_subdir  = "snippets",
      of_what        = stringr::str_glue("{type[i]}.snippets")
    )
  }
}

#' @rdname backup_rstudio_snippets
#' @export
list_snippet_file_backups <- function(type = get_default_snippet_types()) {
  backup_dir <- create_snippets_backup_dir()
  fs::dir_ls(backup_dir, regexp = get_snippets_backup_file_pattern(type))
}

# @rdname backup_rstudio_snippets
# @export
path_snippets_backup_dir <- function() {
  backup.tools::get_path_backup_dir("snippets")
}

# @rdname backup_rstudio_snippets
# @export
create_snippets_backup_dir <- function() {
  backup_dir <- path_snippets_backup_dir()
  fs::dir_create(backup_dir)
  invisible(backup_dir)
}

#' @rdname backup_rstudio_snippets
#' @export
open_snippets_backup_dir <- function() {
  browseURL(path_snippets_backup_dir())
}

# @rdname backup_rstudio_snippets
# @export
get_snippets_backup_file_pattern <- function(type, several.ok = TRUE) {
  type <- match_snippet_type(type, several.ok = several.ok)
  types <- paste(type, collapse = "|")
  stringr::str_glue("/({types})[^/]+?[.]snippets$")
}

#' @rdname backup_rstudio_snippets
#' @export
remove_snippet_backup_duplicates <- function() {
  # files <- list_snippet_file_backups(type = type)
  files <-
    path_snippets_backup_dir() %>%
    fs::dir_ls(type = "file")

  dups <- duplicated(tools::md5sum(files))

  if (any(dups)) {
    rem <- files[dups]
    fs::file_delete(rem)

    str <- paste(crayon::blue(rem), collapse = "\n")
    usethis::ui_done("Removed duplicate(s):\n{str}")
  } else {
    usethis::ui_done("No back-up duplicates were found.")
  }
}

# ======================================================================== ~~~~
# TODO: implement method to restore snippets ----------------------------- ====
# ======================================================================== ~~~~

# @rdname backup_rstudio_snippets
#
# @param filename (character) The name of snippets back-up file.
#         E.g., `"r.snippets--backup-2019-10-31-01430"`.
# @param backup (logical) If `TRUE`, current file with snippets will be
#        backed up.
#
# @export
# filename <- "r--backup-2019-10-31-01430.snippets"
restore_snippets_from_backup <- function(filename, backup = TRUE) {
  # FIXME: use new version of backing up and restoring

  withr::with_dir(
    get_path_rstudio_snippets_dir(),
    {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      filename <- fs::path_file(filename)
      filename_str <- usethis::ui_path(filename)

      if (file.exists(filename)) {
        usethis::ui_done("Back-up file was found: {filename_str}")
      } else {
        usethis::ui_stop("Back-up file {filename_str} does not exist.")
      }
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      type <- stringr::str_extract(filename, ".*?(?=.snippets)")
      type <- match_snippet_type(type)

      usethis::ui_info("Snippets' type: {usethis::ui_field(type)}")
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      tobe_replaced <- make_snippet_filename(type = type)
      tobe_replaced_str <- usethis::ui_path(tobe_replaced)

      if (isTRUE(backup)) {
        backup_rstudio_snippets(type = type)
      } else {
        usethis::ui_oops("Current file was not backed up: {tobe_replaced_str}")
      }
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      if (file.copy(filename, tobe_replaced, overwrite = TRUE)) {
        usethis::ui_done(stringr::str_c(
          "Snippets were restored from the back-up file:\n",
          "   {filename_str} -> {tobe_replaced_str}."
        ))
      } else {
        usethis::ui_oops(
          "Failure to restore snippets from the back-up file {filename_str}."
        )
      }
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    }
  )
}
