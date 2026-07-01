# aelectoral2

<!-- badges: start -->
[![R-CMD-check](https://github.com/morant-consultores/aelectoral2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/morant-consultores/aelectoral2/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`aelectoral2` is Morant Consultores' R package for processing, analyzing and
visualizing Mexican electoral data. It provides an object-oriented (R6) framework
for working with complex electoral datasets across multiple geographic levels
(sections, municipalities and districts).

## Features

- **Vote processing** — coalition vote splitting, candidacy handling, special and
  foreign votes, winners and relative results.
- **Spatial analysis** — join electoral results with shapefiles (`sf`) at section,
  municipality and district levels.
- **Rentabilidad model** — electoral profitability (`rentabilidad`) scoring and
  classification (`SA`, `SF`, `IR` indices) with optimizable parameters.
- **Visualization** — maps, bar charts, violin plots, Sankey diagrams and mosaic
  plots via the `Graficas` class and `ggplot2`.

## Installation

The package depends on two GitHub-only packages (declared in `Remotes`), so install
with a tool that resolves them:

```r
# install.packages("pak")
pak::pak("morant-consultores/aelectoral2")

# or
# install.packages("remotes")
remotes::install_github("morant-consultores/aelectoral2")
```

## Data strategy

To keep the installed package lightweight, large electoral, shapefile and census
resources are loaded through a **hybrid local-first / cache / remote** strategy:

1. **Local-first** — checks the installed package (`inst/`).
2. **Cache-second** — checks the persistent cache
   (`rappdirs::user_cache_dir("aelectoral2")`).
3. **Remote-third** — authenticates via `googledrive` and downloads from the company
   Shared Drive into the cache.

The Drive root folder can be configured with
`options(aelectoral2.drive_root = "folder_name")`. Remote access requires
`googledrive::drive_auth()`.

## Quick start

```r
library(aelectoral2)

# 1. Initialize electoral data
elec <- Electoral$new(eleccion = "pm_21", entidad = "mex",
                      partidos = c("morena", "pan", "pri"))

# 2. Process results (split coalitions, calculate winners)
elec$partido("pm_21")
elec$calcular_ganador("bd_partido", "pm_21")

# 3. Handle spatial data
shp <- ElectoralSHP$new(unidad = "secc_22", entidad = "mex")

# 4. Join and visualize
elec$fusionar_shp(shp = shp$shp$secc_22_mex, base = "bd_partido")
```

See the vignettes (`vignette(package = "aelectoral2")`) for full workflows:
`flujo_electoral` and `flujo_tablero`.

## Core architecture

| Class          | Responsibility                                                        |
| -------------- | --------------------------------------------------------------------- |
| `Electoral`    | Primary data processing: loading, coalition splitting, winners, color |
| `ElectoralSHP` | Spatial data: load shapefiles and join with electoral results         |
| `Tablero`      | Replicate section-level analysis across municipalities / districts    |
| `Graficas`     | High-level visualization tools                                        |

## Development

```r
devtools::load_all()    # load for interactive development
devtools::document()    # update NAMESPACE + man/ from roxygen
devtools::test()        # run the test suite
devtools::check()       # full R CMD check
```

Tests requiring Google Drive skip automatically when unauthenticated or offline
(see `tests/testthat/helper-remote.R`).

### Contributing

Per Morant's ISO 27001 change-management rules, all changes go through feature
branches and are merged into `develop` / `master` **only via Pull Request** — never
merge directly to production branches.

## License

MIT © Morant Consultores. See [LICENSE.md](LICENSE.md).
