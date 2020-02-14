
# Create back-ups ------------------------------------------------------------

create_backup_copy <- function(file = NULL, backup_subdir = "",
  of_what = ifelse(backup_subdir == "", "file(s)", backup_subdir),
  backup_id = get_backup_id()) {

  f_exist <- fs::file_exists(file)

  if (any(f_exist)) {
    current_files_e <- file[f_exist]
    backup_files    <-
      construct_backup_path(
        current_files_e,
        backup_subdir = backup_subdir,
        backup_id = backup_id
      )

    create_backup_dir(backup_subdir)

    fs::file_copy(current_files_e, backup_files)

    usethis::ui_done(paste0(
      "Back up copy of {crayon::green(of_what)} was created ",
      "in {usethis::ui_path(unique(fs::path_dir(backup_files)))}"
    ))

    # FIXME: verify that the back-up was successful

  } else {
    usethis::ui_info("There were no {crayon::green(of_what)} to back-up.")
    # FIXME: announce that no files were backed-up
  }
}


# Directory for back-ups -----------------------------------------------------

get_path_backup_dir <- function(...) {
  backup_dir <- Sys.getenv("R_SETTINGS_BACKUP_DIR")

  if (backup_dir == "") {
    backup_dir <- fs::path(Sys.getenv("R_USER"), ".R-backup")
  }

  fs::path(backup_dir, ...)
}


create_backup_dir <- function(...) {
  fs::dir_create(get_path_backup_dir(...))
}

get_backup_id <- function() {
  format(Sys.time(), "__backup_%y%m%d_%H%M%OS0")
}

construct_backup_path <- function(file = NULL, backup_subdir = "",
  backup_id = get_backup_id()) {

  base0 <- fs::path_file(file)
  ext   <- fs::path_ext(base0)
  base  <- fs::path_ext_remove(base0)

  fs::path(
    get_path_backup_dir(backup_subdir),
    fs::path_ext_set(paste0(base, backup_id), ext = ext)
  )
}

# Restore from back up  ------------------------------------------------------

remove_backup_id <- function(str) {
  stringr::str_remove(str, "__backup_\\d{6}_\\d{6}")
}

extract_filename_to_restore <- function(path = NULL) {
  # path <- "D:/Dokumentai/.R-backup/keybindings/addins__backup_200211_223335.json"
  path %>%
    fs::path_file() %>%
    remove_backup_id()
}

restore_from_backup <- function(..., backup = TRUE) {
  # FIXME: not implemented
  stop("not implemented")
  # original_name <- extract_filename_to_restore()
}
