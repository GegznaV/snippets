test_that("get_path_rstudio_snippets_dir() works", {

  # Correct output class
  expect_is(get_path_rstudio_snippets_dir(rstudio_version = "1.3.1073"), "character")

  # When run not via RStudio
  expect_error(get_path_rstudio_snippets_dir(), "RStudio not running")
})
