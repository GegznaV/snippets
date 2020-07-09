test_that("read_snippets() works", {

  file1 <- system.file("test1/html.snippets", package = "snippets")

  file2 <- system.file("test2/html.snippets", package = "snippets")

  text <- (
    '
snippet space
	&${1:nbsp};${0}

snippet nbsp
	&${1:nbsp};${0}
')

  # No input
  expect_error(read_snippets())

  # Is output class correct?
  expect_is(read_snippets(file1), "data.frame")
  expect_is(read_snippets(file2), "data.frame")
  expect_is(read_snippets(text), "data.frame")
})
