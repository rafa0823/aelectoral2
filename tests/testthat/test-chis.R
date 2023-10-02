test_that("tiene las columnas requeridas", {
  # Proceso electoral 2018
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_18", "nombre_municipio_18",
                "distritol_18", "nombre_distritol_18")

  bd <- Electoral$new("dl_18", "chis", llaves = c("seccion", "municipio_18", "distritol_18"))
  c("gb_18", "pm_18") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "chis"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_setequal(bd, columnas)

  # Proceso electoral 2021
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_21", "nombre_municipio_21",
                "distritol_21", "nombre_distritol_21")
  bd <- Electoral$new("dl_21", "chis", llaves = c("seccion", "municipio_21", "distritol_21"))
  c("pm_21") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "chis"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_setequal(bd, columnas)
})

test_that("las municipio, seccion y distrito tienen la longitud correcta", {
  # Proceso electoral 2016
  bd <- Electoral$new("dl_18", "chis", llaves = c("seccion", "municipio_18", "distritol_18"),
                      extranjero = FALSE)
  c("gb_18", "pm_18") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "chis"))

  bd <- bd$bd |>
    mutate(across(-contains(c("ele", "nombre")), ~gsub(" ", "", .x)))

  seccion <- bd |>
    count(n = nchar(seccion)) |>
    pull(n)

  mun <- bd |>
    count(n = nchar(municipio_18)) |>
    pull(n) |>
    (\(.) .[1])()

  dl <- bd |>
    count(n = nchar(distritol_18)) |>
    pull(n) |>
    (\(.) .[1])()

  expect_equal(seccion, 7)
  expect_equal(mun, 6)
  expect_equal(dl, 6)

  #Proceso electoral 2021
  bd <- Electoral$new("dl_21", "chis", llaves = c("seccion", "municipio_21", "distritol_21"),
                      extranjero = FALSE)
  c("pm_21") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "chis"))

  bd <- bd$bd |>
    mutate(across(-contains(c("ele", "nombre")), ~gsub(" ", "", .x)))

  seccion <- bd |>
    count(n = nchar(seccion)) |>
    pull(n)

  mun <- bd |>
    count(n = nchar(municipio_21)) |>
    pull(n) |>
    (\(.) .[1])()

  dl <- bd |>
    count(n = nchar(distritol_21)) |>
    pull(n) |>
    (\(.) .[1])()

  expect_equal(seccion, 7)
  expect_equal(mun, 6)
  expect_equal(dl, 6)
})
