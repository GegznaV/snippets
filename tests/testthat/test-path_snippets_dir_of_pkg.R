test_that("get_path_snippets_dir_of_pkg() works", {
  # Missing package name
  expect_error(get_path_snippets_dir_of_pkg())

  # Correct output class
  expect_is(get_path_snippets_dir_of_pkg("snippets"), "character")

  # No snippets
  expect_equal(get_path_snippets_dir_of_pkg("testthat"), "")
})
