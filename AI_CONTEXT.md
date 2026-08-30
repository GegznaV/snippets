# snippets package AI context

This document maps the package structure and the main responsibilities of the
code so AI tools can work locally without reading generated content.

## Scope and conventions

- Source package code lives in `R/`.
- Generated package artifacts live in `man/` and `docs/`; update them through
  the appropriate R tooling rather than by hand.
- Prefer package-qualified calls such as `fs::path()` and the base pipe `|>`
  in new code.
- RStudio does not need to be running for file-based snippet operations.
  Headless automatic path resolution uses the RStudio 1.3-or-newer layout.

## Top-level package layout

- `DESCRIPTION` -- package metadata, dependencies, and roxygen settings.
- `NAMESPACE` -- generated exports; update through roxygen2.
- `README.Rmd` and `README.md` -- user-facing overview and examples.
- `R/` -- package implementation source.
- `tests/testthat/` -- focused testthat regression tests.
- `inst/snippets/` -- snippet files installed by `install_snippets_from_package()`.
- `snippets/` -- source snippet collections maintained with the package.
- `man/` and `docs/` -- generated documentation; do not edit by hand.
- `_tmp/` and `new-features/` -- scratch and exploratory material, not core
  package logic.

## Source code map

### Snippet discovery and paths

- `R/snippets--files-and-dirs.R` -- resolves the RStudio snippets directory
  and opens it or individual files.
- `R/snippets--files-and-dirs--internal.R` -- constructs snippet filenames and
  paths, creates the target directory, and locates bundled package snippets.
- `R/snippets--snippet-types.R` -- supported snippet types and validation.

### Installation and backup

- `R/snippets--install.R` -- installs snippets from a package or directory.
- `R/snippets--backup.R` -- creates, lists, and cleans snippet backups.

### Supporting code

- `R/internal--helpers.R` and `R/internal--prepare-snippets.R` -- internal
  helpers for preparing snippet content.
- `R/snippets-package.R` -- package-level metadata.

## Test structure

- `tests/testthat/test-get_path_rstudio_snippets_dir.R` -- snippet directory
  resolution, including headless use.
- `tests/testthat/test-path_rstudio_snippets_file.R` -- RStudio snippet file
  paths.
- `tests/testthat/test-snippets_file_exists.R` -- existence checks and snippet
  type validation.
- Keep tests focused and avoid overwriting a developer's real RStudio snippet
  directory; use explicit versions or temporary directories where practical.

## Suggested traversal order

1. Read `DESCRIPTION` and `AGENTS.md`.
2. Inspect the source file nearest the requested behavior.
3. Add or update focused tests in `tests/testthat/`.
4. Regenerate roxygen output when documentation comments change.
5. Avoid direct edits to generated artifacts and exploratory directories.