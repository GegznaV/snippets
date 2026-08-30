local_rstudio_config_dir <- function() {
  config_dir <- tempfile("rstudio-config-")

  withr::local_envvar(c(
    APPDATA = config_dir,
    XDG_CONFIG_HOME = config_dir,
    RSTUDIO_CONFIG_HOME = config_dir
  ), .local_envir = parent.frame())

  invisible(config_dir)
}
