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
