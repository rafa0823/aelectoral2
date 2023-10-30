test_that("tiene las columnas requeridas", {
  # Proceso electoral 2017
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_17", "nombre_municipio_17",
                "distritol_17", "nombre_distritol_17")

  bd <- Electoral$new("dl_17", "coah", llaves = c("seccion", "municipio_17", "distritol_17"))
  c("gb_17", "pm_17") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "coah"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_setequal(bd, columnas)

  # Proceso electoral 2018
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_18", "nombre_municipio_18",
                "distritol_18", "nombre_distritol_18")
  bd <- Electoral$new("pm_18", "coah", llaves = c("seccion", "municipio_18", "distritol_18"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_setequal(bd, columnas)

  #Proceso electoral 20

  columnas <- c("estado", "nombre_estado", "seccion", "municipio_20", "nombre_municipio_20",
                "distritol_20", "nombre_distritol_20")
  bd <- Electoral$new("dl_20", "coah", llaves = c("seccion", "municipio_20", "distritol_20"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_setequal(bd, columnas)

  #Proceso electoral 21

  columnas <- c("estado", "nombre_estado", "seccion", "municipio_21", "nombre_municipio_21",
                "distritol_21", "nombre_distritol_21")
  bd <- Electoral$new("pm_21", "coah", llaves = c("seccion", "municipio_21", "distritol_21"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_setequal(bd, columnas)
})

test_that("las municipio, seccion y distrito tienen la longitud correcta", {
  # Proceso electoral 2016
  bd <- Electoral$new("dl_17", "coah", llaves = c("seccion", "municipio_17", "distritol_17"))
  c("gb_17", "pm_17") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "coah"))

  bd <- bd$bd |>
    mutate(across(-contains(c("ele", "nombre")), ~gsub(" ", "", .x)))

  seccion <- bd |>
    count(n = nchar(seccion)) |>
    pull(n)

  mun <- bd |>
    count(n = nchar(municipio_17)) |>
    pull(n)

  dl <- bd |>
    count(n = nchar(distritol_17)) |>
    pull(n)

  expect_equal(seccion, 7)
  expect_equal(mun, 6)
  expect_equal(dl, 6)

  bd <- Electoral$new("pm_18", "coah", llaves = c("seccion", "municipio_18", "distritol_18"))

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

  bd <- Electoral$new("dl_20", "coah", llaves = c("seccion", "municipio_20", "distritol_20"))

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

  bd <- Electoral$new("pm_21", "coah", llaves = c("seccion", "municipio_21", "distritol_21"))

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
