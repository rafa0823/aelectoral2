test_that("tiene las columnas requeridas", {
  # Proceso electoral 2015
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_15", "nombre_municipio_15",
                "distritol_15", "nombre_distritol_15")

  bd <- Electoral$new("dl_15", "mex", llaves = c("seccion", "municipio_15", "distritol_15"))
  c("pm_15") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "mex"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_true(setequal(bd, columnas))

  # Proceso electoral 2021
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_17", "nombre_municipio_17",
                "distritol_17", "nombre_distritol_17")
  bd <- Electoral$new("gb_17", "mex", llaves = c("seccion", "municipio_17", "distritol_17"))
  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_true(setequal(bd, columnas))

  columnas <- c("estado", "nombre_estado", "seccion", "municipio_23", "nombre_municipio_23",
                "distritol_23", "nombre_distritol_23")
  bd <- Electoral$new("gb_23", "mex", llaves = c("seccion", "municipio_23", "distritol_23"))
  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_true(setequal(bd, columnas))
})

test_that("las municipio, seccion y distrito tienen la longitud correcta", {
  # Proceso electoral 201
  bd <- Electoral$new("pm_15", "mex", llaves = c("seccion", "municipio_15", "distritol_15"),
                      extranjero = F)
  c("dl_15") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "mex"))

  bd <- bd$bd |>
    mutate(across(-contains(c("ele", "nombre")), ~gsub(" ", "", .x)))

  seccion <- bd |>
    count(n = nchar(seccion)) |>
    pull(n)

  mun <- bd |>
    count(n = nchar(municipio_15)) |>
    pull(n) |>
    (\(.) .[1])()

  dl <- bd |>
    count(n = nchar(distritol_15)) |>
    pull(n) |>
    (\(.) .[1])()

  expect_equal(seccion, 7)
  expect_equal(mun, 6)
  expect_equal(dl, 6)

  #Proceso electoral 2017
  bd <- Electoral$new("gb_17", "mex", llaves = c("seccion", "municipio_17", "distritol_17"))

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

  #Proceso 2018
  bd <- Electoral$new("dl_18", "mex", llaves = c("seccion", "municipio_18", "distritol_18"))
  c("pm_18") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "mex"))

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

  #Proceso 2021
  bd <- Electoral$new("dl_21", "mex", llaves = c("seccion", "municipio_21", "distritol_21"))
  c("pm_21") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "mex"))

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

  bd <- Electoral$new("gb_23", "mex", llaves = c("seccion", "municipio_23", "distritol_23"))

  bd <- bd$bd |>
    mutate(across(-contains(c("ele", "nombre")), ~gsub(" ", "", .x)))

  seccion <- bd |>
    count(n = nchar(seccion)) |>
    pull(n)

  mun <- bd |>
    count(n = nchar(municipio_23)) |>
    pull(n)

  dl <- bd |>
    count(n = nchar(distritol_23)) |>
    pull(n)

  expect_equal(seccion, 7)
  expect_equal(mun, 6)
  expect_equal(dl, 6)
})
