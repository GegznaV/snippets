test_that("open_rstudio_snippets_file() fails where needed", {
  expect_error(
    open_rstudio_snippets_file(rstudio_version = "1.3.1073"),
    'argument "type" is missing'
  )
  expect_error(
    open_rstudio_snippets_file("xxx", rstudio_version = "1.3.1073"),
    "should be one of"
  )
})
