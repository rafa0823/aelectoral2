test_that("tiene las columnas requeridas", {
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_17", "nombre_municipio_17",
                "distritol_17", "nombre_distritol_17")

  bd <- Electoral$new("pm_17", "ver", llaves = c("seccion", "municipio_17", "distritol_17"),
                      extranjero = FALSE)

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_setequal(bd, columnas)


  # Proceso electoral 2016
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_18", "nombre_municipio_18",
                "distritol_18", "nombre_distritol_18")

  bd <- Electoral$new("dl_18", "ver", llaves = c("seccion", "municipio_18", "distritol_18"),
                      extranjero = FALSE)
  c("gb_18") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "ver"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_setequal(bd, columnas)

  # Proceso electoral 2021
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_21", "nombre_municipio_21",
                "distritol_21", "nombre_distritol_21")
  bd <- Electoral$new("dl_21", "ver", llaves = c("seccion", "municipio_21", "distritol_21"))
  c("pm_21") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "ver"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_setequal(bd, columnas)
})
test_that("las municipio, seccion y distrito tienen la longitud correcta", {
  bd <- Electoral$new("pm_17", "ver", llaves = c("seccion", "municipio_17", "distritol_17"),
                      extranjero = FALSE)

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
  # Proceso electoral 2016
  bd <- Electoral$new("dl_18", "ver", llaves = c("seccion", "municipio_18", "distritol_18"))
  c("gb_18") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "ver"))

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

  #Proceso electoral 2021
  bd <- Electoral$new("dl_21", "ver", llaves = c("seccion", "municipio_21", "distritol_21"),
                      extranjero = F)
  c("pm_21") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "ver"))

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
