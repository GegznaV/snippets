test_that("get_snippet_name() works", {
  # Missing input
  expect_error(get_snippet_name())

  text <- readr::read_lines(
'
snippet space
	&${1:nbsp};${0}

snippet nbsp
	&${1:nbsp};${0}
')

  # Correct output class
  expect_is(get_snippet_name(""),   "character")
  expect_is(get_snippet_name(text), "character")

  # Correct output value
  expect_length(get_snippet_name(""), 0)
  expect_length(get_snippet_name("snippet spacer"), 1)
  expect_length(get_snippet_name(text), 2)

  expect_equal(get_snippet_name(""), character(0))
  expect_equal(get_snippet_name("snippet spacer"), "spacer")
  expect_equal(get_snippet_name(text), c("space", "nbsp"))
})
