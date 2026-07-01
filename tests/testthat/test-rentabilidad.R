# Unit tests for the pure functions in R/rentabilidad.R.
# These run entirely offline (no Google Drive / package data required).

test_that("params_rentabilidad_default() returns the expected parameter set", {
  params <- params_rentabilidad_default()

  expect_type(params, "list")
  expect_true(all(c(
    "bastion_seguro_min", "ventaja_comoda_min", "retencion_sensible_min",
    "flip_inmediato_maxloss", "flip_probable_maxloss", "flip_paquete_maxloss",
    "w_SA_margen", "w_SF_cercania", "peso_IR_SA", "peso_IR_SF"
  ) %in% names(params)))

  # Umbrales positivos deben estar ordenados: retencion < ventaja < bastion.
  expect_lt(params$retencion_sensible_min, params$ventaja_comoda_min)
  expect_lt(params$ventaja_comoda_min, params$bastion_seguro_min)

  # Pesos SA, SF y mezcla IR suman 1.
  expect_equal(with(params, w_SA_margen + w_SA_winstreak + w_SA_varinv + w_SA_tamano), 1)
  expect_equal(with(params, w_SF_cercania + w_SF_momentum + w_SF_turnout_gap + w_SF_tamano), 1)
  expect_equal(params$peso_IR_SA + params$peso_IR_SF, 1)
})

test_that("params_a_vec() and vec_a_params() round-trip the optimized parameters", {
  params <- params_rentabilidad_default()

  vec <- params_a_vec(params)
  expect_length(vec, 16)

  recovered <- vec_a_params(vec)

  optimized_keys <- c(
    "retencion_sensible_min", "ventaja_comoda_min", "bastion_seguro_min",
    "flip_inmediato_maxloss", "flip_probable_maxloss", "flip_paquete_maxloss",
    "w_SA_margen", "w_SA_winstreak", "w_SA_varinv", "w_SA_tamano",
    "w_SF_cercania", "w_SF_momentum", "w_SF_turnout_gap", "w_SF_tamano",
    "peso_IR_SA", "peso_IR_SF"
  )

  for (k in optimized_keys) {
    expect_equal(recovered[[k]], params[[k]], tolerance = 1e-6,
                 info = paste("mismatch on", k))
  }
})

test_that("extraer_anios_eleccion() parses 2-digit years and errors when none match", {
  obj <- list(info = list(
    elecciones_agregadas = c("pm_18", "pm_21", "pm_24", "dl_21", "gb_24")
  ))

  expect_equal(extraer_anios_eleccion(obj, "pm"), c(2018L, 2021L, 2024L))
  expect_equal(extraer_anios_eleccion(obj, "dl"), 2021L)
  expect_error(extraer_anios_eleccion(obj, "sen"), "No se encontraron elecciones")
})

test_that("clasificar_rentabilidad() adds the classification columns and scores", {
  df <- tibble::tibble(
    margen_3   = c(0.15, -0.03, 0.06),
    fortaleza  = c(0.20, -0.10, 0.04),
    slope_pct  = c(0.05, -0.03, 0.01),
    delta_23   = c(0.04, -0.02, 0.03),
    delta_13   = c(0.06, -0.04, 0.05),
    turnout_3  = c(0.60,  0.55, 0.58),
    votos_3    = c(1000,   500,  800),
    win_streak = c(3L,     0L,   2L),
    var_morena = c(0.01,   0.05, 0.02)
  )

  out <- clasificar_rentabilidad(df, params_rentabilidad_default())

  expect_true(all(c(
    "momentum", "cuadrante", "clase_afines", "clase_flip", "SA", "SF", "IR"
  ) %in% names(out)))
  expect_equal(nrow(out), nrow(df))

  # Fila fuerte + al alza con margen >= bastion => Q1 y A1.
  expect_match(out$cuadrante[1], "^Q1")
  expect_equal(out$clase_afines[1], "A1: Bastion seguro")

  # El índice de rentabilidad debe ser numérico y finito.
  expect_true(is.numeric(out$IR))
  expect_true(all(is.finite(out$IR)))
})
