test_that("tiene las columnas requeridas", {
  # Proceso electoral 2018
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_18", "distritol_18")
  bd <- Electoral$new("dl_18", "jal", llaves = c("seccion", "municipio_18", "distritol_18"))
  c("gb_18", "pm_18") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "jal"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  len <- setdiff(columnas, bd)

  expect_length(len, 0)
  # Proceso electoral 2021
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_21", "distritol_21")
  bd <- Electoral$new("dl_21", "jal", llaves = c("seccion", "municipio_21", "distritol_21"))
  c("pm_21") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "jal"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  len <- setdiff(columnas, bd)

  expect_length(len, 0)
})
test_that("las municipio, seccion y distrito tienen la longitud correcta", {
  # Proceso electoral 2018
  bd <- Electoral$new("dl_18", "jal", llaves = c("seccion", "municipio_18", "distritol_18"), extranjero = F)
  c("gb_18", "pm_18") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "jal"))

  bd <- bd$bd |>
    mutate(across(-contains("ele"), ~gsub(" ", "", .x)))

  seccion <- bd |>
    count(n = nchar(seccion)) |>
    pull(n)

  mun <- bd |>
    count(n = nchar(municipio_18)) |>
    pull(n)

  dl <- bd |>
    count(n = nchar(distritol_18)) |>
    pull(n)

  expect_equal(seccion, 7)
  expect_equal(mun, 6)
  expect_equal(dl, 6)

  bd <- Electoral$new("dl_21", "jal", llaves = c("seccion", "municipio_21", "distritol_21"))
  c("pm_21") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "jal"))

  bd <- bd$bd |>
    mutate(across(-contains("ele"), ~gsub(" ", "", .x)))

  seccion <- bd |>
    count(n = nchar(seccion)) |>
    pull(n)

  mun <- bd |>
    count(n = nchar(municipio_21)) |>
    pull(n)

  dl <- bd |>
    count(n = nchar(distritol_21)) |>
    pull(n)

  expect_equal(seccion, 7)
  expect_equal(mun, 6)
  expect_equal(dl, 6)
})
