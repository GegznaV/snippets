test_that("path_to_snippets_dir_of_pkg() works", {

  # Missing package name
  expect_error(path_to_snippets_dir_of_pkg())

  # Correct output class
  expect_is(path_to_snippets_dir_of_pkg("snippets"), "character")

  # No snippets
  expect_equal(path_to_snippets_dir_of_pkg("testthat"), "")
})
