# ======================================================================== ~~~~
# Snippet types ---------------------------------------------------------- ====
# ======================================================================== ~~~~

#' Get default snippet types.
#'
#' @return Character vector with possible snippet type values.
#' @export
#'
#' @concept snippet types
#' @examples
#' get_default_snippet_types()

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

get_default_snippet_types <- function() {
  c(
    "r", "markdown", "c_cpp", "css", "html", "java", "javascript", "python",
    "sql", "stan", "tex"
  )
}

#' Return correct snippet type.
#'
#' @param type (character)
#'        A character vector of candidate values.
#'        One of `"r"`, `"markdown"`, `"c_cpp"`, `"css"`, `"html"`, `"java"`,
#'         `"javascript"`, `"python"`, `"sql"`, `"stan`", `"tex"`.
#'        May be unambiguously truncated.
#'        Defaults to `"r"`.
#'
#' @param several.ok (logical)
#'        Specify if `type` should be allowed to have more than one element.
#'        Defaults to `FALSE`.
#'        See also [base::match.arg()]
#'
#' @return (sting) Correct snippet type in lower case. By default returns `"r"`.
#' @export
#'
#' @concept snippet types
#' @examples
#' # Defaults to "r":
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
