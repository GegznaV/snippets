test_that("path_to_snippets_files_of_pkg() works", {

  # Missing package name
  expect_error(path_to_snippets_files_of_pkg())

  # Correct output class
  expect_is(path_to_snippets_files_of_pkg("snippets"), "character")

  # No dir for snippets
  expect_equal(path_to_snippets_files_of_pkg("testthat"), NULL)
})
