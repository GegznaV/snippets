test_that("open_rs_snippets_file() fails where needed", {
  expect_error(open_rs_snippets_file(), 'argument "type" is missing')
  expect_error(open_rs_snippets_file("xxx"), 'should be one of')
})
