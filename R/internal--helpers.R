# Detect operating system (OS)
get_os_type <- function() {
  sys_info <- Sys.info()
  if (!is.null(sys_info)) {
    os <- sys_info["sysname"]
    if (os == "Darwin") {os <- "mac"}
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin",   R.version$os)) {os <- "mac"}
    if (grepl("linux-gnu", R.version$os)) {os <- "linux"}
  }
  unname(tolower(os))
}
