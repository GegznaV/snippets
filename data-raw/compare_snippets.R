
# ============================================================================
#


#
#' Compare snippets
#'
#' @param str_current
#' @param str_new
#'
#' @return
#'
#'
#' @examples
#'
#' file1 <- system.file("test1/html.snippets",  package = "snippets")
#' file2 <- system.file("test2/html.snippets", package = "snippets")
#'
#' compare_snippets(readLines(file1), readLines(file2))
#'
#' compare_snippets(file1, file2)
compare_snippets <- function(str_current, str_new) {

  # library(diffobj)
  # i <- 1
  #
  # str_current <- xxx$data[[i]]$snippet
  # str_new     <- xxx$data[[i]]$snippet

  diffobj::diffChr(target = str_new, current = str_current, mode = "sidebyside",
    context = -1L, ignore.white.space = FALSE, tab.stops = 2,
    cur.banner = glue::glue("Snippet [currently installed]"),
    tar.banner = glue::glue("Snippet [new]")
    # cur.banner = glue::glue("Snippet {xxx$name[[i]]} [current]"),
    # tar.banner = glue::glue("Snippet {xxx$name[[i]]} [new]")
    # , pager = list(make.blocking = FALSE)
  )

  # write_lines(xxx$body, "data-raw/file1.txt")
  # write_lines(xxx$def,  "data-raw/file2.txt")
  # diffFile("data-raw/file1.txt", "data-raw/file2.txt", mode = "sidebyside",
  #   context = -1L, ignore.white.space = FALSE, tab.stops = 2)
  #
  #
  # url.base <- "https://raw.githubusercontent.com/wch/r-source"
  # f1 <- file.path(url.base, "29f013d1570e1df5dc047fb7ee304ff57c99ea68/README")
  # f2 <- file.path(url.base, "daf0b5f6c728bd3dbcd0a3c976a7be9beee731d9/README")
  #
  # diffFile(f1, f2, mode = "sidebyside", context = -1L)
  #
  # diffChr(readLines(f1), readLines(f2), mode = "sidebyside", context = -1L)
}
