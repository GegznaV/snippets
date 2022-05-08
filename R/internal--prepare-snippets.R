# ==========================================================================~~
#' Merge files with RStudio snippets
#'
#' Merge files that end in "--{type}.snippets" into file named "{type}.snippets"
#'
#' @inheritParams match_snippet_type
#' @param in_dir Path of directory.
#' @param rm regexp for files to remove.
#'
#' @noRd
#'
#' @return
#'
#' @examples
#'
#' snippets_dir <- "snippets/"
#' #
#' merge_snippets(type = "r",        in_dir = snippets_dir)
#' merge_snippets(type = "markdown", in_dir = snippets_dir)
#'
#' install_snippets_from_dir(type = c("r", "markdown"), from_dir = snippets_dir)
#' #
#' merge_snippets(type = "r",        in_dir = snippets_dir, rm = "-VG-snippets")
#' merge_snippets(type = "markdown", in_dir = snippets_dir, rm = "-VG-snippets")
#'
#' update_snippets_in_snippets("r")
#' update_snippets_in_snippets("markdown")
#'
#' # install_snippets_from_dir(type = "r",        from_dir = snippets_dir)
#' # install_snippets_from_dir(type = "markdown", from_dir = snippets_dir)

merge_snippets <- function(type = get_default_snippet_types(), in_dir = ".",
  rm = NULL) {
  # in_dir <- "snippets/"
  # rm = "-VG-snippets"

  type <- match_snippet_type(type)

  withr::with_dir(
    in_dir,
    {
      files <- fs::dir_ls(regexp = stringr::str_glue("--{type}.snippets$"))
      if (!is.null(rm)) {
        files <- files[stringr::str_detect(files, rm, negate = TRUE)]
      }

      files %>%
        purrr::map(readr::read_lines) %>%
        purrr::reduce(c) %>%
        # Remove comments:
        # stringr::str_subset(pattern = "^# ", negate = TRUE) %>%
        readr::write_lines(make_snippet_filename(type = type))
    }
  )
}

# ==========================================================================~~
#' Update snippets in package \pkg{snippets}
#'
#' Internal function to update (copy) snippets of package \pkg{snippets}
#' into directorry accessible by users of the package.
#'
#' @noRd
update_snippets_in_snippets <- function(type, snippets_dir = "snippets") {
  type <- match_snippet_type(type)
  base <- stringr::str_glue("{type}.snippets")
  fs::file_copy(
    path      = fs::path(snippets_dir, base),
    new_path  = fs::path("inst", "snippets", base),
    overwrite = TRUE
  )
}

# ==========================================================================~~
#' Merge and update snippets
#'
#' Functions creates one file for one type of snippets and copies it to the
#' direcctory accessible by the users of the package.
#'
#' @inheritParams match_snippet_type
#' @param snippets_dir Path to directory.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' if (FALSE) {
#'
#' merge_and_update_snippets("r")
#' merge_and_update_snippets("markdown")
#'
#' remove_snippet_backup_duplicates()
#' }
merge_and_update_snippets <- function(type, snippets_dir = "snippets/") {

  merge_snippets(type = type,            in_dir   = snippets_dir)
  install_snippets_from_dir(type = type, from_dir = snippets_dir)

  # Remove personal VG snippets
  merge_snippets(type = type,            in_dir = snippets_dir, rm = "-VG-snippets")
  update_snippets_in_snippets(type)
}
