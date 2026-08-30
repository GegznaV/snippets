# Detect operating system (OS)
# get_os_type <- function() {
#   sys_info <- Sys.info()
#   if (!is.null(sys_info)) {
#     os <- sys_info["sysname"]
#     if (os == "Darwin") {os <- "mac"}
#   } else {
#     os <- .Platform$OS.type
#     if (grepl("^darwin",   R.version$os)) {os <- "mac"}
#     if (grepl("linux-gnu", R.version$os)) {os <- "linux"}
#   }
#   unname(tolower(os))
# }

get_os_type <- function() {
  sysname <- Sys.info()[["sysname"]]

  if (!is.null(sysname) && nzchar(sysname)) {
    os <- switch(sysname,
      "Darwin"  = "mac",
      "Windows" = "windows",
      "Linux"   = "linux",
      tolower(sysname)
    )
  } else {
    os_type <- tolower(.Platform$OS.type)

    if (identical(os_type, "windows")) {
      os <- "windows"
    } else if (grepl("darwin", R.version$os, ignore.case = TRUE)) {
      os <- "mac"
    } else if (grepl("linux", R.version$os, ignore.case = TRUE)) {
      os <- "linux"
    } else {
      os <- os_type
    }
  }

  as.character(os)
}
