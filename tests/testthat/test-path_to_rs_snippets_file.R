test_that("path_to_rs_snippets_file() works", {

  # Correct output class
  expect_is(
    path_to_rs_snippets_file(create = FALSE, rstudio_version = "1.3.1073"),
    "character"
  )

  # When run not via RStudio
  expect_error(path_to_rs_snippets_file(create = FALSE), "RStudio not running")
})
