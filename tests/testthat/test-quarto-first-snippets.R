test_that("bundled Markdown snippets are Quarto-first", {
  snippet_file <- system.file("snippets", "markdown.snippets", package = "snippets")
  snippet_text <- readLines(snippet_file, warn = FALSE)

  expect_true(any(grepl("^# Quarto code cells", snippet_text)))
  expect_true(any(grepl("^snippet ---qmd$", snippet_text)))
  expect_true(any(grepl("^snippet fig-rmd$", snippet_text)))
  expect_false(any(grepl("^snippet setup-lt$", snippet_text)))
  expect_false(any(grepl("^snippet setupm$", snippet_text)))
})

test_that("bundled R snippets use current R and Quarto patterns", {
  snippet_file <- system.file("snippets", "r.snippets", package = "snippets")
  snippet_text <- readLines(snippet_file, warn = FALSE)

  expect_false(any(grepl("%>%", snippet_text, fixed = TRUE)))
  expect_true(any(grepl("^snippet pipe-arg$", snippet_text)))
  expect_true(any(grepl("^snippet select-where$", snippet_text)))
  expect_true(any(grepl("^snippet summarize-across$", snippet_text)))
  expect_true(any(grepl("^snippet fig$", snippet_text)))
  expect_true(any(grepl("^snippet tbl$", snippet_text)))

  funn_line <- match("snippet funn", snippet_text)
  expect_true(grepl("\\(", snippet_text[funn_line + 1L], fixed = TRUE))
})