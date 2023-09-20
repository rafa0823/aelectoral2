test_that("tiene las columnas requeridas", {
  # Proceso electoral 2016
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_16", "nombre_municipio",
                "distritol_16", "nombre_distritol")
  bd <- Electoral$new("dl_16", "ags", llaves = c("seccion", "municipio_16", "distritol_16"))
  c("gb_16", "pm_16") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "ags"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()

  expect_equal(bd, columnas)
  # Proceso electoral 2018-2019
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_18", "nombre_municipio",
                "distritol_18", "nombre_distritol")
  bd <- Electoral$new("dl_18", "ags", llaves = c("seccion", "municipio_18", "distritol_18"))
  c("pm_19") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "ags"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()
  expect_equal(bd, columnas)

  # Proceso electoral 2021
  columnas <- c("estado", "nombre_estado", "seccion", "municipio_21", "nombre_municipio",
                "distritol_21", "nombre_distritol")
  bd <- Electoral$new("dl_21", "ags", llaves = c("seccion", "municipio_21", "distritol_21"))
  c("pm_21") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "ags"))

  bd <- bd$bd |>
    select(-contains("ele")) |>
    names()
  expect_equal(bd, columnas)
})
test_that("las municipio, seccion y distrito tienen la longitud correcta", {
  # Proceso electoral 2016
  bd <- Electoral$new("dl_16", "ags", llaves = c("seccion", "municipio", "distritol"))
  c("gb_16", "pm_16") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "ags"))

  bd <- bd$bd |>
    mutate(across(-contains("ele"), ~gsub(" ", "", .x)))

  seccion <- bd |>
    count(n = nchar(seccion)) |>
    pull(n)

  mun <- bd |>
    count(n = nchar(municipio)) |>
    pull(n)

  dl <- bd |>
    count(n = nchar(distritol)) |>
    pull(n)

  expect_equal(seccion, 7)
  expect_equal(mun, 6)
  expect_equal(dl, 6)

  # Proceso electoral 2018-2019
  bd <- Electoral$new("dl_18", "ags", llaves = c("seccion", "municipio", "distritol"))
  c("pm_19") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "ags"))

  bd <- bd$bd |>
    mutate(across(-contains("ele"), ~gsub(" ", "", .x)))

  seccion <- bd |>
    count(n = nchar(seccion)) |>
    pull(n)

  mun <- bd |>
    count(n = nchar(municipio)) |>
    pull(n)

  dl <- bd |>
    count(n = nchar(distritol)) |>
    pull(n)

  expect_equal(seccion, 7)
  expect_equal(mun, 6)
  expect_equal(dl, 6)

  #Preceso electoral 2021

  bd <- Electoral$new("dl_21", "ags", llaves = c("seccion", "municipio_21", "distritol_21"))
  c("pm_21") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "ags"))

  bd <- bd$bd |>
    mutate(across(-contains("ele"), ~gsub(" ", "", .x)))

  seccion <- bd |>
    count(n = nchar(seccion)) |>
    pull(n)

  mun <- bd |>
    count(n = nchar(municipio)) |>
    pull(n)

  dl <- bd |>
    count(n = nchar(distritol)) |>
    pull(n)

  expect_equal(seccion, 7)
  expect_equal(mun, 6)
  expect_equal(dl, 6)
})
test_that("resultados coinciden", {
  bd <- Electoral$new("dl_16", "ags", llaves = c("seccion", "municipio", "distritol"))
  c("pm_16", "gb_16", "dl_18", "pm_19", "dl_21", "pm_21") |>
    walk(~bd$agregar_bd(eleccion = .x, entidad = "ags"))

  bd <- bd$bd
  expect_equal(bd$ele_pan_dl_16[1], 762)
  expect_equal(bd$ele_pri_pt_panal_pm_16[1], 0)
  expect_equal(bd$ele_pri_pt_gb_16[1], 0)
  expect_equal(bd$ele_pri_dl_18[1], 380)
  expect_equal(bd$ele_morena_pm_19[1], 68)
  expect_equal(bd$ele_morena_dl_21[1], 49)
  expect_equal(bd$ele_morena_pm_21[1], 61)
})
