---
name: run-tests
description: Run the aelectoral2 testthat suite and summarize pass/fail, reminding about the Google Drive skip convention.
---

# Run tests

Run the package's test suite and report results.

```bash
Rscript -e 'devtools::test()'
```

## Reading the output

- A test failing with a Google Drive / network error usually means it's missing
  `skip_if_no_drive()` (see `tests/testthat/helper-remote.R`) — not a real
  regression. Flag it, don't "fix" it by hardcoding credentials or mocking Drive.
- Pure-function tests (`test-colores.R`, `test-partidos.R`, `test-rentabilidad.R`,
  `test-production-usage.R`) must stay green offline; if one fails, it's a real bug.
- `test-remote-ags-workflow.R` is Drive-dependent and expected to skip outside an
  authenticated environment.

## If you touched roxygen comments

Run `devtools::document()` first (or let it happen automatically — see project
hooks) so `NAMESPACE`/`man/` are in sync before `devtools::test()` runs, since a
stale NAMESPACE can mask or cause spurious failures.

## Full check (only when asked)

`devtools::check()` runs a full R CMD check (much slower, builds the package).
Use `devtools::test()` for routine iteration.
