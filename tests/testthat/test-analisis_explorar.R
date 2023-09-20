test_that("revisar cálculo por partido", {
  mex <- Electoral$new(entidad = "mex", eleccion = "gb_17", llaves = "municipio_17")
  mex$partido(eleccion = "gb_17", nivel = "seccion")

  c("df_18","dl_18", "pm_18", "pr_18",
    "pm_21", "df_21", "dl_21") |>
    walk(~{
      mex$agregar_bd(eleccion = .x, entidad = "mex")
      mex$partido(eleccion = .x, nivel = "seccion")
    })

  expect_equal(mex$bd_partido[["gb_17"]]$ele_morena_gb_17[2], 337)
  expect_equal(mex$bd_partido[["pm_18"]]$ele_pan_pm_18[34], 94)

  chis <- Electoral$new(entidad = "chis", "pm_21")
  chis$partido(eleccion = "pm_21", nivel = "seccion")

  expect_equal(chis$bd_partido[[1]]$ele_rsp_pm_21[2], 12)
})
test_that("revisar calculo relativo", {
  mex <- Electoral$new(entidad = "mex", eleccion = "gb_17", llaves = "municipio_17")
  mex$partido(eleccion = "gb_17", nivel = "seccion")

  c("df_18","dl_18", "pm_18", "pr_18",
    "pm_21", "df_21", "dl_21") |>
    walk(~{
      mex$agregar_bd(eleccion = .x, entidad = "mex")
      mex$partido(eleccion = .x, nivel = "seccion")
    })

  relativo <- mex$bd_partido |>
    imap(~{
      aelectoral2::calcular_votos_relativos(.x, partido = c("morena", "pan", "pri", "prd"), eleccion = .y, grupo = seccion)
    }) |>
    reduce(full_join, join_by(seccion))

  nominal <- mex$bd_partido[["dl_21"]] |>
    filter(seccion == "15_0648") |>
    pull(ele_nominal_dl_21)

  morena <- mex$bd_partido[["dl_21"]] |>
    filter(seccion == "15_0648") |>
    pull(ele_morena_dl_21)

  expect_equal(pull(filter(relativo, seccion == "15_0648"), ele_morena_dl_21), morena/nominal)
})
test_that("revisión cálculo votos totales", {
  mex <- Electoral$new(entidad = "mex", eleccion = "dl_21", llaves = "municipio_17")
  mex$todas[[1]]
  res <- calcular_votos_totales(mex$todas[[1]], "mc", eleccion = "dl_21") |>
    pull(1)
  expect_equal(res, sum(mex$todas[[1]]$ele_mc_dl_21))
})
test_that("número de secciones no cambia al convertir en partido", {
  bd1 <- Electoral$new(eleccion = "pm_17", entidad = "ver")
  expectativa <- bd1$bd |>
    count(seccion) |>
    nrow()

  bd2 <- bd1$partido(nivel = "seccion", eleccion = "pm_17")[[1]] |>
    count(seccion) |>
    nrow()

  expect_equal(bd2, expectativa)
})
test_that("número de secciones no cambia al agregar bd", {
  exp <- Electoral$new(entidad = "mex", eleccion = "gb_17", llaves = "municipio_17")
  exp <- exp$bd |>
    count(seccion) |>
    nrow()
  mex <- Electoral$new(entidad = "mex", eleccion = "gb_17", llaves = "municipio_17")
  mex$partido(eleccion = "gb_17", nivel = "seccion")
  c("df_18","dl_18", "pm_18", "pr_18",
    "pm_21", "df_21", "dl_21") |>
    walk(~{
      mex$agregar_bd(eleccion = .x, entidad = "mex")
      mex$partido(eleccion = .x, nivel = "seccion")
    })

  bd <- mex$bd_partido |>
    reduce(full_join, join_by(seccion)) |>
    filter(!is.na(ele_morena_gb_17)) |>
    count(seccion) |>
    nrow()

  expect_equal(bd, exp)
})
test_that("corroborar ganador sea correcto", {
  mex <- Electoral$new("gb_17", "mex", llaves = "municipio_17")
  mex <- mex$bd |>
    summarise(across(contains("ele_"), \(x) sum(x, na.rm = TRUE)), .by = nombre_municipio_17) |>
    ganador_eleccion(eleccion = "gb_17") |>
    select(nombre_municipio_17, ganador_gb_17)

  expect_equal(mex$ganador_gb_17[2], "pri")
  expect_equal(mex$ganador_gb_17[3], "morena")
  expect_equal(mex$ganador_gb_17[4], "pri")
  expect_equal(mex$ganador_gb_17[126], "morena")
})
