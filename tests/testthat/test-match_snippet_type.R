test_that("match_snippet_type() works", {
  expect_equal(match_snippet_type(), "r")

  expect_equal(match_snippet_type("r"), "r")
  expect_equal(match_snippet_type("R"), "r")

  expect_equal(match_snippet_type("m"), "markdown")

  expect_error(match_snippet_type(c("r", "m")))

  expect_equal(match_snippet_type(c("r", "m"), several.ok = TRUE), c("r", "markdown"))
})
