test_that("snippets_file_exists() works", {
  # Correct output class
  expect_is(snippets_file_exists("r"),   "logical")
  expect_is(snippets_file_exists("css"), "logical")

  # Incorrect snippet type
  expect_error(snippets_file_exists("xxx"))
})
