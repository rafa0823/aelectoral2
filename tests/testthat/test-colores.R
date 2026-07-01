# Unit tests for color helpers that rely only on the packaged `paleta` dataset.

test_that("asociar_colores() returns a named vector for the requested parties", {
  cols <- asociar_colores(c("morena", "pan"))

  expect_type(cols, "character")
  expect_setequal(names(cols), c("morena", "pan"))
  # Los colores deben ser hex validos.
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
})

test_that("asociar_colores() ignores parties absent from the palette", {
  cols <- asociar_colores(c("morena", "no_existe_xyz"))

  expect_true("morena" %in% names(cols))
  expect_false("no_existe_xyz" %in% names(cols))
})
