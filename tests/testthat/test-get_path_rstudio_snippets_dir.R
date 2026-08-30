test_that("get_path_rstudio_snippets_dir() works", {
  temp_config_dir <- local_rstudio_config_dir()

  # Correct output class
  expect_is(get_path_rstudio_snippets_dir(rstudio_version = "1.3.1073"), "character")

  # Works when RStudio is not running
  expect_equal(
    get_path_rstudio_snippets_dir(),
    fs::path(temp_config_dir, "snippets")
  )
})
