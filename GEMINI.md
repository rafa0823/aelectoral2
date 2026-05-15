# aelectoral2: R Package for Mexican Electoral Analysis

`aelectoral2` is an R package designed to process, analyze, and visualize Mexican electoral data. It provides a structured, object-oriented framework for handling complex electoral datasets at multiple geographic levels.

## ISO 27001 Compliance Rules

The following rules must be followed to ensure compliance with ISO 27001 standards for secure software development:

1.  **Secure Software Development Life Cycle (SSDLC):** All code changes must follow the defined lifecycle: Research -> Strategy -> Execution (Plan, Act, Validate). Security requirements must be considered at each stage.
2.  **Environment Separation:** Maintain a clear distinction between Development, Testing, and Production environments.
    *   Development work must occur in feature branches or the `develop` branch.
    *   Production data must be anonymized or masked if used in development/test environments.
3.  **Path Portability:** NEVER use hardcoded absolute paths (e.g., `~/Google Drive/` or `C:\Users\...`). Use relative paths (via `system.file()` or `here::here()`) or configuration-driven paths.
4.  **Secure Coding Practices:**
    *   Validate all external inputs (e.g., `eleccion`, `entidad` parameters).
    *   Avoid using `eval(parse())` or similar functions on untrusted input.
    *   Sanitize any input used to construct file paths or database queries.
5.  **Documentation:** All production code must be fully documented using Roxygen2. This includes all public fields and methods in R6 classes.
6.  **Change Management:** All changes must be made through feature branches. Merges into `develop` and `master` (or `main`) MUST be performed via Pull Requests (or equivalent formal review and approval process in this environment). **NEVER merge directly to production branches.**
7.  **Dependency Management:** Regularly review and update package dependencies. Avoid adding unnecessary dependencies.
9.  **Remote Data Strategy:** The package uses a hybrid loading strategy to minimize local weight (~2GB reduced to few MBs).
    *   **Local-First:** Checks the installed package (`inst/`).
    *   **Cache-Second:** Checks the persistent local cache (`rappdirs::user_cache_dir("aelectoral2")`).
    *   **Remote-Third:** If missing, authenticates via `googledrive` and downloads from the company Shared Drive to the local cache.
    *   **Configuration:** The Drive root folder can be configured via `options(aelectoral2.drive_root = "folder_name")`.

## Core Architecture


The package utilizes R6 classes for stateful data management and a clean API:

1.  **`Electoral`**: The primary class for data processing.
    *   Loads electoral data from internal RDA files.
    *   Handles coalition vote splitting (`$partido()`, `$candidato()`).
    *   Manages special votes and foreign votes.
    *   Calculates relative results, winners, and color gradients.
2.  **`ElectoralSHP`**: Manages spatial data (shapefiles).
    *   Loads geographic data for sections, municipalities, and districts.
    *   Joins spatial data with electoral results.
3.  **`Tablero`**: Replicates analysis across different geographic levels.
    *   Allows scaling section-level analysis to municipalities or districts automatically.
4.  **`Graficas`**: High-level visualization tools.
    *   Generates maps, bar charts, violin plots, Sankey diagrams, and mosaic plots.

## Technologies

*   **Language:** R
*   **OOP:** R6
*   **Data Wrangling:** `dplyr`, `tidyr`, `purrr`, `stringr`, `rlang`
*   **Spatial:** `sf`, `leaflet`, `ggsflabel`
*   **Visualization:** `ggplot2`, `RColorBrewer`, `shades`, `ggrepel`
*   **Workflow:** `devtools`, `roxygen2`, `testthat`, `knitr`

## Development Workflow

### Building and Documenting
Standard `devtools` commands should be used:
*   `devtools::document()`: Update NAMESPACE and man files from Roxygen comments.
*   `devtools::load_all()`: Load the package for interactive development.
*   `devtools::install()`: Install the package locally.

### Testing
Testing is handled via `testthat`:
*   `devtools::test()`: Run all package tests.
*   Tests are located in `tests/testthat/`.

### Data Management
*   **Raw Data:** Processing scripts are in `data-raw/`.
*   **Internal Data:** Cleaned data is stored as RDA files in `data/` and `inst/` (electoral, shp, censo).

## Usage Example

```r
library(aelectoral2)

# 1. Initialize electoral data
elec <- Electoral$new(eleccion = "pm_21", entidad = "mex", partidos = c("morena", "pan", "pri"))

# 2. Process results (split coalitions, calculate winners)
elec$partido("pm_21")
elec$calcular_ganador("bd_partido", "pm_21")

# 3. Handle spatial data
shp <- ElectoralSHP$new(unidad = "secc_22", entidad = "mex")

# 4. Join and Visualize
elec$fusionar_shp(shp = shp$shp$secc_22_mex, base = "bd_partido")
# (Use Graficas class or direct ggplot2 for mapping)
```

## Project Structure

*   `R/`: Core logic and R6 class definitions.
*   `data-raw/`: Scripts for data cleaning and preparation.
*   `inst/`: Static data resources (electoral results, shapefiles, census data).
*   `man/`: Generated documentation.
*   `tests/`: Unit tests.
*   `vignettes/`: In-depth examples and workflows.
