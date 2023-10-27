test_that("tiene las columnas requeridas", {
  # Proceso electoral 2016
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_16", "nombre_municipio_16",
                "distritol_16", "nombre_distritol_16")

  bd <- Electoral$new("dl_16", "tamps", llaves = c("seccion", "municipio_16", "distritol_16"),
                      extranjero = FALSE)
  c("gb_16", "pm_16") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "tamps"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_true(setequal(bd, columnas))

  # Proceso electoral 2018
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_18", "nombre_municipio_18",
                "distritol_18", "nombre_distritol_18")
  bd <- Electoral$new("pm_18", "tamps", llaves = c("seccion", "municipio_18", "distritol_18"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_true(setequal(bd, columnas))

  columnas <- c("estado", "nombre_estado", "seccion", "municipio_19", "nombre_municipio_19",
                "distritol_19", "nombre_distritol_19")
  bd <- Electoral$new("dl_19", "tamps", llaves = c("seccion", "municipio_19", "distritol_19"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_true(setequal(bd, columnas))

  columnas <- c("estado", "nombre_estado", "seccion", "municipio_21", "nombre_municipio_21",
                "distritol_21", "nombre_distritol_21")
  bd <- Electoral$new("pm_21", "tamps", llaves = c("seccion", "municipio_21", "distritol_21"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_true(setequal(bd, columnas))
})

test_that("las municipio, seccion y distrito tienen la longitud correcta", {
  # Proceso electoral 2016
  bd <- Electoral$new("dl_16", "tamps", llaves = c("seccion", "municipio_16", "distritol_16"),
                      extranjero = FALSE)
  c("gb_16", "pm_16") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "tamps"))

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
  bd <- Electoral$new("pm_18", "tamps", llaves = c("seccion", "municipio_18", "distritol_18"),
                      extranjero = F)

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

  bd <- Electoral$new("dl_19", "tamps", llaves = c("seccion", "municipio_19", "distritol_19"),
                      extranjero = F)

  bd <- bd$bd |>
    mutate(across(-contains(c("ele", "nombre")), ~gsub(" ", "", .x)))

  seccion <- bd |>
    count(n = nchar(seccion)) |>
    pull(n)

  mun <- bd |>
    count(n = nchar(municipio_19)) |>
    pull(n)

  dl <- bd |>
    count(n = nchar(distritol_19)) |>
    pull(n)

  expect_equal(seccion, 7)
  expect_equal(mun, 6)
  expect_equal(dl, 6)

  bd <- Electoral$new("pm_21", "tamps", llaves = c("seccion", "municipio_21", "distritol_21"))

  bd <- bd$bd |>
    mutate(across(-contains(c("ele", "nombre")), ~gsub(" ", "", .x)))

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
