# ======================================================================== ~~~~
# Manage snippets -------------------------------------------------------- ====
# ======================================================================== ~~~~

# Add snippets    ======================================================== ~~~~
# Considerations:
#    1) Create file, if it does not exist
#    2) Rename old snippets

# library(tidyverse)
#
# "%>%" <- magrittr::`%>%`



#' Extract snippent name from string
#'
#' Extract snippent name from strings that start with "snippet".
#'
#' @param str (character)
#'       Vector of strings. Usually the result of `readr::read_lines()`.
#'
#' @return Character vector with snippet names.
#'
#' @examples
#' text <- readr::read_lines(
#' '
#' snippet space
#' 	&${1:nbsp};${0}
#'
#' snippet nbsp
#' 	&${1:nbsp};${0}
#' ')
#'
#' get_snippet_name(text)
#'
#' @noRd
# @export

get_snippet_name <- function(str) {
  # Vector with snippet names
  str %>%
    stringr::str_subset("^snippet ") %>%
    stringr::str_extract("(?<=^snippet )(.*)")
}


#' Read snippet names
#'
#' @param file Paths to text files with snippets.
#'
#' @return Character vector with snippet names.
#'
#' @noRd
# @export

read_snippet_names <- function(file) {
  file %>% readr::read_lines() %>% get_snippet_name()
}


#' Read snippets into data frame
#'
#' @param input (character)
#'        Either a file name or a character vector with with a snippet.
#'
#' @return File/Text with snippets parsed into data frame (`tibble`).
#' @export
#'
#' @examples
#'
#' library(snippets)
#'
#' file1 <- system.file("test1/html.snippets", package = "snippets")
#' read_snippets(file1)
#'
#' file2 <- system.file("test2/html.snippets", package = "snippets")
#' read_snippets(file2)
#'
#' file3 <- path_to_snippets_files_of_pkg("snippets")[1]
#' read_snippets(file3)
#'
#'
#' text <- (
#' '
#' snippet space
#' 	&${1:nbsp};${0}
#'
#' snippet nbsp
#' 	&${1:nbsp};${0}
#' ')
#'
#' read_snippets(text)
#'
read_snippets <- function(input) {

  as_text <- readr::read_lines(input)

  tibble::tibble(
    snippet = as_text # %>% stringr::str_trunc(20)
    ,
    keyword = as_text %>% stringr::str_detect("^\\w+? ") %>% ifelse("keyword", ""),
    comment = as_text %>% stringr::str_detect("^#")      %>% ifelse("comment", ""),
    body    = as_text %>% stringr::str_detect("^\t")     %>% ifelse("body",    ""),
    empty   = as_text %>% stringr::str_detect("^$")      %>% ifelse("empty",   "")
  ) %>%
    tidyr::unite("contents", keyword:empty, sep = "") %>%
    dplyr::mutate(line_type =
        dplyr::case_when(
          contents %in% c("comment", "keyword") ~ "meta",
          contents == "empty"                   ~ NA_character_,
          TRUE                                  ~ contents)
    ) %>%
    tidyr::fill(line_type, .direction = "downup") %>%
    dplyr::mutate(
      snippet_beggins =
        line_type == "meta" & dplyr::lag(line_type, default = "body") == "body",
      no = cumsum(snippet_beggins)
    ) %>%
    dplyr::group_by(no) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      name  = purrr::map_chr(data, ~ get_snippet_name(.$snippet))
      , snippet = purrr::map(data, ~ structure(.$snippet, class = "glue"))
      # , full  = purrr::map_chr(data, ~ stringr::str_c(.$snippet, collapse = "\n"))
      , body  = purrr::map_chr(data,
        ~ stringr::str_c(.$snippet[.$line_type == "body"], collapse = "\n")
      )
      # , body  = purrr::map_chr(data, ~ stringr::str_c(
      #   stringr::str_remove(.$snippet[.$line_type == "body"], "^\t"), collapse = "\n")
      # )
      # , meta  = purrr::map_chr(data, ~ stringr::str_c(
      #   stringr::str_remove(.$snippet[.$line_type == "meta"], "^\t"), collapse = "\n")
      # )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(name, no, snippet, dplyr::everything())

}



#' Find conflicting snippets
#'
#' Show snippets in df_new, that have the same name but different definition (body)
#'
#' @param df_old ...
#' @param df_new ...
#'
#' @return
#'
#' @noRd
#'
#' @examples
#'
#' if (FALSE) {
#'
#' # usethis::edit_rstudio_snippets("markdown")
#' # usethis::edit_rstudio_snippets("r")
#'
#'  library(tidyverse)
#'
#'  sn_old <- read_snippets(file = fs::path_home_r(".R/snippets/markdown.snippets"))
#'  sn_new <- read_snippets(file = "snippets/markdown.snippets")
#'
#'  find_conflicting_snippets(sn_old, sn_new)
#'  find_conflicting_snippets(sn_old, sn_new) %>% construct_snippet()
#'
#' }
#'
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

#' Construct a string for snippets from data frame of snippets
#'
#' @param .data
#'
#' @return
#'
#' @noRd
construct_snippet <- function(.data) {
  stringr::str_glue_data(.data, "snippet {name}\n{body}")
}
# =========================================================================== ~
#' Write snippet to file
#'
#'
#' @param snippets
#'        Data frame with columns `name` (for snippet names) and `body` for
#'        definitions of snippets.
#' @param type (character)
#'        Type of snippets file (r, markdown, etc.)
#' @param in_conflict_keep (character)
#'        One of "original", "new", "both".
#' @param instert_default_if_missing (logical)
#'        Insert file with the default snippets, if file with snippets is
#'        missing.
#' @param file (character)
#'        File name to write snippets to. If present, `type` is ignored.
#'
#' @noRd
#
write_snippet <- function(snippets, type = NULL, in_conflict_keep = "original",
  instert_default_if_missing = TRUE, file = path_to_rs_snippets_file(type, create = TRUE)) {
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
        readr::write_lines(path = make_snippet_filename(type = type))
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
#' Functions creates one file for one type of snippets and coppies it to the
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
