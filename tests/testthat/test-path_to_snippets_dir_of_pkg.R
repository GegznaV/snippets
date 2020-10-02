test_that("get_path_to_rs_snippets_dir() works", {

  # Missing package name
  expect_error(get_path_to_rs_snippets_dir())

  # Correct output class
  expect_is(get_path_to_rs_snippets_dir("snippets"), "character")

  # No snippets
  expect_equal(get_path_to_rs_snippets_dir("testthat"), "")
})
