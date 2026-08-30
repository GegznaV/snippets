test_that("path_rstudio_snippets_file() works", {
  local_rstudio_config_dir()

  # Correct output class
  expect_is(
    path_rstudio_snippets_file(create = FALSE, rstudio_version = "1.3.1073"),
    "character"
  )

  # Works when RStudio is not running
  expect_is(path_rstudio_snippets_file(create = FALSE), "character")
})
