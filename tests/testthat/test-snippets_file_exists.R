test_that("snippets_file_exists() works", {
  # Correct output class
  expect_is(snippets_file_exists("r",   rstudio_version = "1.3.1073"),   "logical")
  expect_is(snippets_file_exists("css", rstudio_version = "1.3.1073"), "logical")

  # Incorrect snippet type
  expect_error(snippets_file_exists("xxx", rstudio_version = "1.3.1073"))

  # When run not via RStudio
  expect_error(snippets_file_exists("r"), "RStudio not running")
})
