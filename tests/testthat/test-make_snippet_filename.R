test_that("make_snippet_filename() works", {
  expect_equal(make_snippet_filename(),    "r.snippets")

  expect_equal(make_snippet_filename("r"), "r.snippets")
  expect_equal(make_snippet_filename("R"), "r.snippets")

  expect_equal(make_snippet_filename("m"), "markdown.snippets")

})
