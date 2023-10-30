test_that("tiene las columnas requeridas", {
  # Proceso electoral 2016
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_16", "nombre_municipio_16",
                "distritol_16", "nombre_distritol_16")

  bd <- Electoral$new("dl_16", "oaxaca", llaves = c("seccion", "municipio_16", "distritol_16"))
  c("gb_16", "pm_16") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "oaxaca"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_true(setequal(bd, columnas))

  # Proceso electoral 2018
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_18", "nombre_municipio_18",
                "distritol_18", "nombre_distritol_18")
  bd <- Electoral$new("dl_18", "oaxaca", llaves = c("seccion", "municipio_21", "distritol_21"))
  c("pm_18") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "oaxaca"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_true(setequal(bd, columnas))
})

test_that("las municipio, seccion y distrito tienen la longitud correcta", {
  # Proceso electoral 2016
  bd <- Electoral$new("dl_16", "oaxaca", llaves = c("seccion", "municipio_16", "distritol_16"))
  c("gb_16", "pm_16") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "oaxaca"))

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
  bd <- Electoral$new("dl_18", "oaxaca", llaves = c("seccion", "municipio_21", "distritol_21"))
  c("pm_18") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "oaxaca"))

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
})
