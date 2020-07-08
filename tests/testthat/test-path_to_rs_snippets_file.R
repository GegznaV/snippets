test_that("path_to_rs_snippets_file() works", {

  # Correct output class
  expect_is(path_to_rs_snippets_file(create = FALSE), "character")

})
