test_that("get_path_snippet_files_of_pkg() works", {
  # Missing package name
  expect_error(get_path_snippet_files_of_pkg())

  # Correct output class
  expect_is(get_path_snippet_files_of_pkg("snippets"), "character")

  # No dir for snippets
  expect_equal(get_path_snippet_files_of_pkg("testthat"), NULL)
})
