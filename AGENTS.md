# Package working rules for this repository

## Coding conventions

- Prefer package-qualified calls such as `fs::path()` instead of `library()`
  or `require()`.
- Prefer the base pipe `|>` over `%>%` for new code.
- Keep tidyverse-style formatting while favoring small, explicit functions and
  early returns.
- Keep RStudio-independent file operations usable from Rscript and other
  headless R sessions. Reserve `rstudioapi` calls for behavior that genuinely
  requires a running RStudio session.

## R package maintenance

- Keep roxygen2 documentation in sync with function behavior.
- Use `if (interactive()) { ... }` for executable roxygen examples.
- Export only intended user-facing functions and document their parameters.
- Do not hand-edit generated `NAMESPACE`, `man/`, or `docs/` output.

## Verification checklist

- Run targeted tests for changed behavior.
- Regenerate roxygen documentation when function comments change.
- For path or installation changes, test outside a running RStudio session.
- Do not let tests modify a developer's real snippet files or backups.

## Related files

- Package structure guide: [AI_CONTEXT.md](AI_CONTEXT.md)
- Copilot guidance: [.github/copilot-instructions.md](.github/copilot-instructions.md)
- Main package code: [R](R)
- Tests: [tests/testthat](tests/testthat)