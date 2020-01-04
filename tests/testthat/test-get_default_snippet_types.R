test_that("get_default_snippet_types() works", {
  rez <-  c("r", "markdown", "c_cpp", "css", "html", "java", "javascript",
    "python", "sql", "stan", "tex")
  expect_equal(get_default_snippet_types(), rez)
})
