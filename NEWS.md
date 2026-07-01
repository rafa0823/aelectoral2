# aelectoral2 0.2.0

First production baseline for Morant Consultores.

## New features

- **Rentabilidad model** (`R/rentabilidad.R`): electoral profitability scoring and
  classification with `ejecutar_modelo_rentabilidad()`, `clasificar_rentabilidad()`,
  `params_rentabilidad_default()`, and parameter optimization helpers
  (`optimizar_params()`, `objetivo_rentabilidad()`, `vec_a_params()`,
  `params_a_vec()`, `extraer_anios_eleccion()`).
- **Remote data strategy**: hybrid local-first / cache / remote (Google Drive)
  loading via `fetch_remote_data()` and a persistent cache, keeping the installed
  package lightweight.

## Infrastructure & quality

- Real `DESCRIPTION` metadata: descriptive title/description, modern `Authors@R`
  block, and version bump to 0.2.0.
- Offline-safe test suite: unit tests for the pure functions (rentabilidad, party
  helpers, color helpers); Google-Drive integration tests now skip cleanly when
  unauthenticated or offline (`tests/testthat/helper-remote.R`).
- Continuous integration via GitHub Actions (`R-CMD-check`).
- Added `README.md`, `NEWS.md` and `CLAUDE.md`.
- Regenerated documentation and `NAMESPACE`.

# aelectoral2 0.1.0

- Initial internal version: `Electoral`, `ElectoralSHP`, `Tablero` and `Graficas`
  R6 classes for Mexican electoral analysis.
