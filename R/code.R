

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
get_path_to_snippet_file <- function(dir, type = get_default_snippet_types(), create = FALSE) {

  path <- fs::path(dir, make_snippet_filename(type = type))

  if (isTRUE(create) && !file.exists(path)) {
    fs::dir_create(fs::path_dir(path))
    fs::file_create(path)
  }
  path
}

#' @rdname get_path_to_snippet_file
#' @export
get_path_to_rs_snippet_file <- function(type = get_default_snippet_types(), create = FALSE) {
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


#' RStudio snippet file back-up
#'
#' - `backup_rs_snippets()` creates a back-up of snippets file
#' - `list_snippet_file_backups()` lists the names of current file with snippets
#'    and its back-ups.
#' - `restore_snippets_backup()` restores a back-up file.
#'
#' @inheritParams match_snippet_type
#' @return Invisibly returns the name of back-up copy. See [fs::file_copy()].
#' @export
#'
#' @examples
#' if (FALSE) {
#'
#' backup_rs_snippets("r")
#' backup_rs_snippets("markdown")
#'
#'
#' list_snippet_file_backups("r")
#'
#'
#' # USe name of an existing back-up file
#' restore_snippets_backup("r.snippets--backup-2019-10-31-01430")
#'
#' }
backup_rs_snippets <- function(type) {
  create_rs_snippets_dir()

  base_name   <- get_path_to_rs_snippet_file(type = type)
  backup_name <-
    paste0(base_name, "--backup-", format(Sys.time(), "%Y-%m-%d-%H%M%S"))

  new_backup <- fs::file_copy(base_name, backup_name)

  usethis::ui_done(stringr::str_c(
    "Current file was backed up: ",
    "\n{usethis::ui_path(tobe_replaced)} \u2192 {usethis::ui_path(new_backup)}"
  ))

  invisible(new_backup)
}

#' @rdname backup_rs_snippets
#' @export
list_snippet_file_backups <- function(type) {
  create_rs_snippets_dir()

  pattern <- make_snippet_filename(type = type)

  my_dir <- get_rs_snippets_dir()
  fs::dir_ls(my_dir, regexp = paste0("/", pattern))
}

#' @rdname backup_rs_snippets
#'
#' @param filename (character) The name of snippets back-up file.
#'         E.g., `"r.snippets--backup-2019-10-31-01430"`.
#' @param backup (logical) If `TRUE`, current file with snippets will be
#'        backed up.
#'
#' @export
# filename <- "r.snippets--backup-2019-10-31-01430"
restore_snippets_backup <- function(filename, backup = TRUE) {


  withr::with_dir(
    get_rs_snippets_dir(),
    {
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      filename     <- fs::path_file(filename)
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
      tobe_replaced     <- make_snippet_filename(type = type)
      tobe_replaced_str <- usethis::ui_path(tobe_replaced)

      if (isTRUE(backup)) {
        backup_rs_snippets(type = type)

      } else {
        usethis::ui_oops("Current file was not backed up: {tobe_replaced_str}")
      }
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      if (file.copy(filename, tobe_replaced, overwrite = TRUE)) {
        usethis::ui_done("Previous back-up was restored {filename_str} \u2192 {tobe_replaced_str}.")

      } else {
        usethis::ui_oops("Failed to restore back-up {filename_str}.")
      }
      # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    })
}

#' Replace snippets file
#'
#' @param type The type of RStudio snippet.
#' @param from_dir The directory with replacement file.
#' @param backup (logical) Indication if a back-up copy should be created.
#'
#' @export
#'
#' @examples
#' if (FALSE) {
#'
#' # Replace your R and Markdown snippets with those in package "snippets":
#' replace_snippets_file("r",        backup = TRUE)
#' replace_snippets_file("markdown", backup = TRUE)
#'
#' # Check if back-up copies exist:
#' list_snippet_file_backups("r")
#' list_snippet_file_backups("markdown")
#' }

replace_snippets_file <- function(type = get_default_snippet_types(),
  from_dir = get_pkg_snippets_dir("custom"), backup = TRUE) {

  replacement <- get_path_to_snippet_file(dir = from_dir, type = type)
  if (!file.exists(replacement)) {
    stop("The replacement file was not found: \n", fs::path_abs(replacement))
  }

  original <- get_path_to_rs_snippet_file(type = type)

  # Create a back-up copy
  if (backup && file.exists(original)) {
    backup_name <-
      paste0(original, "--backup-", format(Sys.time(), "%Y-%m-%d-%H%M%S"))

    if (file.copy(from = original, to = backup_name)) {
      usethis::ui_done("Back-up created: {usethis::ui_path(backup_name)}")

    } else {
      usethis::ui_oops("Back-up not created: {usethis::ui_path(original)}")
    }
  }

  # Copy/Overwrite the file
  if (file.copy(from = replacement, to = original, overwrite = TRUE)) {
    usethis::ui_done("Snippets updated: {usethis::ui_path(original)}")

  } else {
    usethis::ui_oops("Snippets not changed: {usethis::ui_path(original)}")
  }
}

#' Edit file with RStudio snippets
#'
#' Open and edit file with RStudio snippets.
#' This function is imported from package \pkg{usethis}.
#'
#' @details
#' Files created by `edit_rstudio_snippets()`` will mask, not supplement,
#' the built-in default snippets. If you like the built-in snippets, copy
#' them and include with your custom snippets.
#'
#' @inheritParams match_snippet_type
#'
#' @seealso
#' [usethis::edit_rstudio_snippets()]
#'
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


get_pkg_snippets_dir <- function(...) {
  system.file("snippets", ... , package = "snippets")
}

# get_custom_snippets_path()
get_custom_snippets_path <- function() {
  dir(get_pkg_snippets_dir("custom"), pattern = ".snippets$")
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
    dplyr::arrange(name, desc(file))
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
# snippets_dir <- "inst/snippets/custom/"
#
# merge_snippets("r",        in_dir = snippets_dir)
# merge_snippets("markdown", in_dir = snippets_dir)
#
# replace_snippets_file("r",        from_dir = snippets_dir)
# replace_snippets_file("markdown", from_dir = snippets_dir)
#
# merge_snippets("r",        in_dir = snippets_dir, rm = "-VG-snippets")
# merge_snippets("markdown", in_dir = snippets_dir, rm = "-VG-snippets")

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
        stringr::str_subset(pattern = "^# ", negate = TRUE) %>%
        readr::write_lines(path = make_snippet_filename(type = type))
    }
  )
}
