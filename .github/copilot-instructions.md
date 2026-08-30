# Copilot instructions for the snippets package

## Preferred R patterns

- Use `pkg::fun()` for external functions.
- Prefer `|>` over `%>%` in new code.
- Use tidyverse-style readability, not strict spacing dogma.
- Keep examples in roxygen blocks as `if (interactive())` instead of
  `\dontrun{\donttest{ ... }}`.

## Package-specific rules

- Keep snippet installation, backup, and path-resolution functions usable
  without a running RStudio session whenever they only operate on files.
- Use `rstudioapi` only for actions that require the active RStudio IDE, such
  as navigating to a file.
- Treat the RStudio 1.3-or-newer user configuration layout as the headless
  automatic default.
- Avoid tests that overwrite a user's real snippet files or backup directory.
- Document user-facing functions with roxygen2 comments and realistic examples.

## Future maintenance

- Update this file when adding shared package conventions or workflows.
- Prefer small, verifiable fixes and targeted tests over broad refactors.
- Regenerate roxygen output when comments change.

## Related context files

- Package overview: [AI_CONTEXT.md](../AI_CONTEXT.md)
- Repository rules: [AGENTS.md](../AGENTS.md)
- Main source directory: [R](../R)
- Test suite: [tests/testthat](../tests/testthat)