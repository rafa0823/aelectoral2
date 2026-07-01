# CLAUDE.md

Guidance for Claude Code / AI agents working in this repository.

## What this is

`aelectoral2` — Morant Consultores' R package for processing, analyzing and
visualizing Mexican electoral data. Object-oriented (R6) core: `Electoral`,
`ElectoralSHP`, `Tablero`, `Graficas`. See `README.md` for the user-facing overview
and `GEMINI.md` for the extended architecture / ISO 27001 notes.

## Commands

```r
devtools::load_all()    # load for interactive development
devtools::document()    # regenerate NAMESPACE + man/ from roxygen (run after editing roxygen)
devtools::test()        # run the test suite
devtools::check()       # full R CMD check
```

Run from a shell: `Rscript -e 'devtools::test()'`.

## Repository layout

- `R/` — core logic and R6 class definitions (`clases.R` holds the R6 classes;
  `rentabilidad.R` the profitability model; `visualizaciones.R`, `partidos.R`,
  `criterios.R`, `auxiliar_colores.R`, `remote_data.R`, `lectura.R`).
- `man/` — generated docs (do not edit by hand; regenerate via `document()`).
- `data/` — packaged `.rda` datasets (e.g. `paleta`, `claves`, `diccionario`).
- `data-raw/` — scripts that build the datasets.
- `tests/testthat/` — unit + integration tests.
- `vignettes/` — `flujo_electoral.Rmd`, `flujo_tablero.Rmd`.

## Data strategy (important)

Heavy electoral/shapefile/census data is **not** committed. Loading is hybrid:
local `inst/` → persistent cache (`rappdirs::user_cache_dir("aelectoral2")`) →
Google Drive (via `googledrive`). Configure the Drive root with
`options(aelectoral2.drive_root = "folder_name")`. Remote access needs
`googledrive::drive_auth()`.

## Testing conventions

- Prefer **pure-function** unit tests that need no network/data.
- Tests that require Google Drive must call `skip_if_no_drive()`
  (`tests/testthat/helper-remote.R`) so the suite stays green offline and in CI.
- `Config/testthat/edition: 3`.

## Rules

- **Never** use hardcoded absolute paths — use `system.file()` / config-driven paths.
- **Never** merge directly to production branches. All changes go through feature
  branches and Pull Requests (ISO 27001 change management — see `GEMINI.md`).
- Validate external inputs; avoid `eval(parse())` on untrusted input.
- After changing roxygen comments, run `devtools::document()` and commit the
  regenerated `NAMESPACE` / `man/` together.
