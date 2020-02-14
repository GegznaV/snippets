

#' Get default snippet types
#'
#' @return Character vector with possible snippet type values.
#' @export
#'
#' @examples
#' get_default_snippet_types()
get_default_snippet_types <- function() {

  # default_snippets_rs_1.2.5001 <-
  # c(
  #   "r.snippets",
  #   "markdown.snippets",
  #   "c_cpp.snippets",
  #   "css.snippets",
  #   "html.snippets",
  #   "java.snippets",
  #   "javascript.snippets",
  #   "python.snippets",
  #   "sql.snippets",
  #   "stan.snippets",
  #   "tex.snippets"
  # ) %>%
  #   stringr::str_extract(".*(?=\\.snippets)")
  c("r", "markdown", "c_cpp", "css", "html", "java", "javascript",
    "python", "sql", "stan", "tex")

}

#' Return correct snippet type
#'
#' @param type A character vector of candidate values.
#'             One of `"r"`, `"markdown"`, `"c_cpp"`, `"css"`, `"html"`,
#'            `"java"`, `"javascript"`, `"python"`, `"sql"`, `"stan`", `"tex"`.
#'            May be unambiguously truncated.
#'            The default is `"r"`.
#' @param several.ok (logical) Specify if `type` should be allowed to have more
#'        than one element. Default is `FALSE`.
#'
#' @seealso
#' [base::match.arg()]
#'
#' @return (sting) Correct snippet type in lower case. By default returns `"r"`.
#' @export
#'
#' @examples
#' match_snippet_type()
#'
#' match_snippet_type("r")
#'
#' match_snippet_type("m")
match_snippet_type <- function(type = get_default_snippet_types(),
  several.ok = FALSE) {
  type <- tolower(type)
  match.arg(type, several.ok = several.ok)
}

#' Make a filename for snippets
#'
#' Make a filename for certain type of snippets.
#'
#' @inheritParams match_snippet_type
#'
#' @return String with a filename.
#' @export
#'
#' @examples
#' make_snippet_filename()
#'
#' make_snippet_filename("markdown")
#'
make_snippet_filename <- function(type = get_default_snippet_types()) {
  type <- match_snippet_type(type)
  stringr::str_glue("{type}.snippets")
}

# Snippet directories and files ========================================= ====

#' Get path to directory for RStudio snippets
#'
#' @return (string) Directory name.
#' @export
#'
#' @examples
#' get_rs_snippets_dir()
get_rs_snippets_dir <- function() {
  fs::path_expand_r("~/.R/snippets/")
}

#' Create directory for RStudio snippets
#'
#' @return Invisibly returns the path to created directory.
#'         See also [fs::dir_create()].
#' @export
#' @examples
#' if (FALSE) {
#' create_rs_snippets_dir()
#' }
create_rs_snippets_dir <- function() {
  fs::dir_create(get_rs_snippets_dir())
}

#' Open directory of RStudio snippets
#' @export
#' @importFrom utils browseURL
open_rs_snippets_dir <- function() {
  create_rs_snippets_dir()
  browseURL(get_rs_snippets_dir())
}

#' Construct path to file of certain type snippets
#'
#'  Construct path to file of certain type snippets:
#'
#' - `get_path_to_snippet_file()`    in any folder
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
get_path_to_snippet_file <- function(dir, type = get_default_snippet_types(),
  create = FALSE) {

  path <- fs::path(dir, make_snippet_filename(type = type))

  if (isTRUE(create) && !file.exists(path)) {
    fs::dir_create(fs::path_dir(path))
    fs::file_create(path)
  }
  path
}

#' @rdname get_path_to_snippet_file
#' @export
get_path_to_rs_snippet_file <- function(type = get_default_snippet_types(),
  create = FALSE) {

  get_path_to_snippet_file(dir = get_rs_snippets_dir(), type = type, create = create)
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
#' @examples
#' snippets_file_exists("r")
#' snippets_file_exists("markdown")
snippets_file_exists <- function(type) {
  fs::file_exists(get_path_to_rs_snippet_file(type, create = FALSE))
}


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
#' }
install_snippets_from_package <- function(package = "snippets",
  type = get_default_snippet_types(), subdir = "", backup = TRUE) {

  from_dir <- get_pkg_snippets_dir(subdir, package = package)
  install_snippets_from_dir(type = type, from_dir = from_dir, backup = backup)
}

#' @rdname install-snippets
#' @export
# TODO: Adapt the function to accept several values of "type".
install_snippets_from_dir <- function(from_dir = ".",
  type = get_default_snippet_types(), backup = TRUE) {
  if (!fs::dir_exists(from_dir)) {
    usethis::ui_oops("Directory was not found: {usethis::ui_path(from_dir)}  ")
    usethis::ui_stop("Please, select directory that exists.")
  }

  # FIXME: search for files of certain type only and not all ".snippet" files
  ext_snippet <- usethis::ui_path(".snippet")
  n_snippet_files <- length(dir(from_dir, pattern = ".snippets$"))

  if (n_snippet_files > 0) {
    usethis::ui_done("Directory contains {n_snippet_files} file(s) with extension {ext_snippet}. ")

  } else {
    usethis::ui_stop("No files with extension {ext_snippet} were found in the directory.")
  }

  replacement <- get_path_to_snippet_file(dir = from_dir, type = type)

  if (!file.exists(replacement)) {
    stop("The replacement file was not found: \n", fs::path_abs(replacement))
  }


  # Create a back-up copy
  if (backup) {
    backup_rs_snippets(type = type)
  }

  # Copy/Overwrite the file
  original <- get_path_to_rs_snippet_file(type = type)
  create_rs_snippets_dir()
  is_copied <- file.copy(from = replacement, to = original, overwrite = TRUE)
  if (is_copied) {
    usethis::ui_done("Snippets were updated: {usethis::ui_path(original)}")
    usethis::ui_info('To use the updated snippets, {usethis::ui_field("RStudio")} must be restarted (closed and reopened).')

  } else {
    usethis::ui_info("No snippets were changed in {usethis::ui_path(original)}")
  }
}

# @name edit_rstudio_snippets
# @title Edit file with RStudio snippets
#
# @description
# Open and edit file with RStudio snippets.
# This function is imported from package \pkg{usethis}.
#
# @details
# Files created by `edit_rstudio_snippets()`` will mask, not supplement,
# the built-in default snippets. If you like the built-in snippets, copy
# them and include with your custom snippets.
#
# @inheritParams match_snippet_type
#
# @seealso
# [usethis::edit_rstudio_snippets()]

#' @importFrom usethis edit_rstudio_snippets
#' @export
usethis::edit_rstudio_snippets



# ~ ======================================================================= ====

# Fix snippets file
#
# @param file
#
# @return
# @export
#
# @examples
# if (FALSE) {
#
# file = "snippets/md-comments--markdown.snippets"
# purrr::walk(
#   dir("snippets", "\\.snippets$", full.names = TRUE),
#   fix_snippets_file
# )
#
# fs::path_home_r(".R/snippets/r.snippets") %>%
#   readr::read_file() %>%
#   structure(class = "glue")
#
# fs::path_home_r(".R/rstudio/keybindings") %>%
#     fs::file_show()
#
# }

fix_snippets_file <- function(file) {
  # Remove tab in the line above word "snippet"
  new <- old <- readr::read_lines(file)
  ind1 <- stringr::str_which(new, "^snippet ") - 1
  ind2 <- stringr::str_which(new, "^\t$")
  ind <- ind1[ind1 %in% ind2]
  new[ind] <- ""

  if (!identical(new, old)) {
    readr::write_lines(new, file)
  }
}


get_pkg_snippets_dir <- function(..., package = "snippets") {
  system.file("snippets", ... , package = package)
}

# get_path_to_snippets_files()
get_path_to_snippets_files <- function(package = "snippets") {
  fs::dir_ls(get_pkg_snippets_dir(package = package), regexp = ".snippets$")
}



# Manage snippets ====================================================== =====
# Add snippets ============================================================= ~
# Considerations:
#    1) Create file, if it does not exist
#    2) Rename old snippets

# library(tidyverse)
#
# "%>%" <- magrittr::`%>%`



# Read snippet names
#
# @param file
#
# @return
# @export
#
# @examples
read_snippet_names <- function(file) {
  as_text <- file %>% readr::read_lines()

  # Vector with snippet names
  as_text %>%
    stringr::str_subset("snippet ") %>%
    stringr::str_extract("(?<=snippet )(.*)")
}


# Read snippets to tibble
#
# @param file
#
# @return
# @export
#
# @examples
read_snippets <- function(file) {
  as_text <- file %>% readr::read_lines()

  # Vector with snippet names
  snippet_name <-
    as_text %>%
    stringr::str_subset("snippet ") %>%
    stringr::str_extract("(?<=snippet )(.*)")

  # Vector with snippet bodies
  snippet_body <-
    as_text %>%
    stringr::str_c(collapse = "\n") %>%
    stringr::str_split("snippet .*?(\r)?\n") %>% # Split at each snippet
    .[[1]] %>%
    .[-1]

  # %>%
  #   stringr::str_replace("(?<!\n)$", "\n")       # ensure ending with a new line

  # snippet_body <-
  # file %>%
  # readr::read_file() %>%
  # stringr::str_split("snippet .*?(\r)?\n") %>%    # Split at each snippet
  #   .[[1]] %>%
  #   .[-1] %>%
  #   stringr::str_replace("(?<!\n)$", "\n") %>%       # ensure ending with a new line
  #   stringr::str_replace_all("[\t\r\n]+$", "\n") %>% # FIXME: paskirties nepamenu
  #   stringr::str_replace_all("\r\n", "\n")           # different line endings

  tibble::tibble(name = snippet_name, body = snippet_body)
}

# Find conflicting snippets
#
# Show snippets in df_new, that have the same name but different definition (body)
#
# @param df_old
# @param df_new
#
# @return
# @export
#
# @examples
#
# if (FALSE) {
#
# # usethis::edit_rstudio_snippets("markdown")
# # usethis::edit_rstudio_snippets("r")
#
# # library(tidyverse)
# #
# # sn_old <- read_snippets(file = fs::path_home_r(".R/snippets/markdown.snippets"))
# # sn_new <- read_snippets(file = "install/.R/snippets/markdown.snippets")
# #
# # find_conflicting_snippets(sn_old, sn_new)
# # find_conflicting_snippets(sn_old, sn_new) %>% construct_snippet()
#
# }
#
#
find_conflicting_snippets <- function(df_old, df_new) {
  from_df_new <- dplyr::semi_join(df_new, df_old, by = "name")
  from_df_old <- dplyr::semi_join(df_old, df_new, by = "name")
  dplyr::bind_rows(
    .id = "file",
    original = dplyr::setdiff(from_df_old, from_df_new),
    new      = dplyr::setdiff(from_df_new, from_df_old)
  ) %>%
    dplyr::arrange(name, dplyr::desc(file))
}

# Construct a string for snippets from data frame of snippets
#
# @param .data
#
# @return
# @export
#
# @examples
construct_snippet <- function(.data) {
  stringr::str_glue_data(.data, "snippet {name}\n{body}")
}
# =========================================================================== ~
# Write snippet to a file
#
# @param snippets Data frame with columns `name` (for snippet names) and `body`
#        for definitions of snippets.
# @param type (character) Type of snippets file (r, markdown, etc.)
# @param in_conflict_keep (character) "original", "new", "both".
# @param instert_default_if_missing (logical) Insert file with the default
#        snippets, if file with snippets is missing.
# @param file (character) File name to write snippets to. If present, `type`
#        is ignored.
#
# @export
#
write_snippet <- function(snippets, type = NULL, in_conflict_keep = "original",
  instert_default_if_missing = TRUE, file = get_path_to_rs_snippet_file(type, create = TRUE)) {
  stop("check if this function works as expected.")

  # FIXME: instert_default_if_missing ???

  sn_old <- read_snippets(file)

  confict <- find_conflicting_snippets(df_old = sn_old, df_new = snippets)
  confict_names <- unique(confict$name)

  if (nrow(confict) > 0) {
    warning("Conflicting snippets: ", paste(confict_names, sep = ", "))
    warning("Keep snippets: ", in_conflict_keep)

    confict <-
      switch(
        in_conflict_keep,
        "original" = confict %>% dplyr::filter(file == "original"),
        "new"      = confict %>% dplyr::filter(file == "new"),
        "both"     = confict %>% dplyr::mutate(name = paste(name, "-", file)),
        stop("Unknown option in_conflict_keep = '", in_conflict_keep, "'")
      )
  }

  # Merge all snippets
  final <-
    dplyr::bind_rows(

      sn_old %>%
        dplyr::filter(!name %in% confict_names) %>%
        tibble::add_column(file = "original", .before = 1),

      confict,

      snippets %>%
        dplyr::filter(!name %in% confict_names) %>%
        tibble::add_column(file = "new")
    )

  final %>%
    construct_snippet() %>%
    readr::write_lines(path = file, sep = "")
}


# Merge files with RStudio snippets
#
# Merge files that end in "--{type}.snippets" into file named "{type}.snippets"
#
# @param type
# @param in_dir
# @param rm regexp for files to remove
#
# @return
# @export
#
# @examples
#
# snippets_dir <- "snippets/"
#
# merge_snippets(type = "r",        in_dir = snippets_dir)
# merge_snippets(type = "markdown", in_dir = snippets_dir)
#
# install_snippets_from_dir(type = "r",        from_dir = snippets_dir)
# install_snippets_from_dir(type = "markdown", from_dir = snippets_dir)
# #
# merge_snippets(type = "r",        in_dir = snippets_dir, rm = "-VG-snippets")
# merge_snippets(type = "markdown", in_dir = snippets_dir, rm = "-VG-snippets")
#
# update_snippets_in_snippets("r")
# update_snippets_in_snippets("markdown")
#
# # install_snippets_from_dir(type = "r",        from_dir = snippets_dir)
# # install_snippets_from_dir(type = "markdown", from_dir = snippets_dir)

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
        readr::write_lines(path = make_snippet_filename(type = type))
    }
  )
}

# ==========================================================================~~
# Internal function to update (coppy) snippets of package "snippets"
# into directorry accessible by users of the package.
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
#' Functions creates one file for one type of snippets and coppies it to the
#' direcctory accessible by the users of the package.
#'
#' @param type ...
#' @param snippets_dir ...
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
