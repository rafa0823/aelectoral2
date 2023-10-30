test_that("tiene las columnas requeridas", {
  # Proceso electoral 2016
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_16", "nombre_municipio_16",
                "distritol_16", "nombre_distritol_16")

  bd <- Electoral$new("dl_16", "hgo", llaves = c("seccion", "municipio_16", "distritol_16"),
                      extranjero = FALSE)
  c("gb_16", "pm_16") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "hgo"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_true(setequal(bd, columnas))

  # Proceso electoral 2021
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_18", "nombre_municipio_18",
                "distritol_18", "nombre_distritol_18")
  bd <- Electoral$new("dl_18", "hgo", llaves = c("seccion", "municipio_18", "distritol_18"))
  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_true(setequal(bd, columnas))

  columnas <- c("estado", "nombre_estado", "seccion", "municipio_20", "nombre_municipio_20",
                "distritol_20", "nombre_distritol_20")
  bd <- Electoral$new("pm_20", "hgo", llaves = c("seccion", "municipio_20", "distritol_20"))
  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_true(setequal(bd, columnas))
})

test_that("las municipio, seccion y distrito tienen la longitud correcta", {
  # Proceso electoral 2016
  bd <- Electoral$new("dl_16", "hgo", llaves = c("seccion", "municipio_16", "distritol_16"),
                      extranjero = FALSE)
  c("gb_16", "pm_16") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "hgo"))

  bd <- bd$bd |>
    mutate(across(-contains(c("ele", "nombre")), ~gsub(" ", "", .x)))

  seccion <- bd |>
    count(n = nchar(seccion)) |>
    pull(n)

  mun <- bd |>
    count(n = nchar(municipio_16)) |>
    pull(n)

  dl <- bd |>
    count(n = nchar(distritol_16)) |>
    pull(n)

  expect_equal(seccion, 7)
  expect_equal(mun, 6)
  expect_equal(dl, 6)

  #Proceso electoral 2021
  bd <- Electoral$new("dl_18", "hgo", llaves = c("seccion", "municipio_18", "distritol_18"))

  bd <- bd$bd |>
    mutate(across(-contains(c("ele", "nombre")), ~gsub(" ", "", .x)))

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

  bd <- Electoral$new("pm_20", "hgo", llaves = c("seccion", "municipio_20", "distritol_20"))

  bd <- bd$bd |>
    mutate(across(-contains(c("ele", "nombre")), ~gsub(" ", "", .x)))

  seccion <- bd |>
    count(n = nchar(seccion)) |>
    pull(n)

  mun <- bd |>
    count(n = nchar(municipio_20)) |>
    pull(n)

  dl <- bd |>
    count(n = nchar(distritol_20)) |>
    pull(n)

  expect_equal(seccion, 7)
  expect_equal(mun, 6)
  expect_equal(dl, 6)
})
