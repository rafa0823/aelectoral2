---
name: new-r6-method
description: Add a method or field to one of aelectoral2's R6 classes (Electoral, ElectoralSHP, Tablero, Graficas) in R/clases.R following existing conventions.
---

# Add an R6 method or field

All four classes live in `R/clases.R` (1100+ lines): `Electoral`, `ElectoralSHP`,
`Tablero`, `Graficas`. Read the target class's `public = list(...)` block first —
each class documents itself inline, so match its existing style rather than
inventing a new one.

## Conventions observed in this file

- **Fields** are declared with a `#' @field <name> <description>` line directly
  above the field inside `public = list(...)`.
- **Methods** get a roxygen block directly above the `function(...)` assignment:
  `#' @description`, one `#' @param` per argument, `#' @return`, and
  `#' @examples`. Only the class's primary/constructor method typically carries
  `#' @export` — check whether the class itself is exported before adding it
  elsewhere.
- Methods are plain closures assigned as list elements
  (`nombre_metodo = function(...) { ... }`), not separate `R6::R6Class` calls.
- Spanish is used for parameter/field descriptions throughout — match that,
  don't switch to English mid-file.
- Private state (if any) goes in a `private = list(...)` block; check whether
  the target class already has one before adding one.

## Workflow

1. Locate the class and the right spot (near related methods, not appended at
   the end) with `grep -n "^Electoral <-\|^ElectoralSHP <-\|^Tablero <-\|^Graficas <-" R/clases.R`.
2. Write the method/field with a full roxygen block in the existing style.
3. Run `devtools::document()` to regenerate `NAMESPACE`/`man/` — the project's
   PostToolUse hook does this automatically after edits to `R/*.R`, but confirm
   it ran (check `git status` for changed `man/*.Rd` or `NAMESPACE`).
4. Run the test suite (see the `run-tests` skill) — add or update a testthat
   test if the method is a pure function; if it needs Google Drive data, gate
   it with `skip_if_no_drive()`.
5. Never hand-edit `NAMESPACE` or `man/*.Rd` — they're generated.
