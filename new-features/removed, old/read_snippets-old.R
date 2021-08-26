#' Read snippets to tibble.
#'
#' @param file
#'
#' @return
#'
#' @noRd
# @export

read_snippets <- function(file) {
  as_text <- readr::read_lines(file)

  # Vector with snippet names
  snippet_name <- get_snippet_name(as_text)

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
