# Unit tests for pure data-wrangling helpers (no external data required).

test_that("eliminar_votoExtranjero() drops foreign sections (seccion == '0000')", {
  bd <- tibble::tibble(
    seccion = c("0001", "0000", "0002", "0000"),
    votos   = c(10, 5, 20, 7)
  )

  out <- suppressMessages(aelectoral2:::eliminar_votoExtranjero(bd))

  expect_equal(nrow(out), 2)
  expect_false(any(out$seccion == "0000"))
})

test_that("eliminar_especiales() drops special polling stations and validates input", {
  bd <- tibble::tibble(
    tipo_casilla = c("B", "S", "C", "S"),
    votos        = c(10, 5, 20, 7)
  )

  out <- suppressMessages(aelectoral2:::eliminar_especiales(bd))
  expect_equal(nrow(out), 2)
  expect_false(any(out$tipo_casilla == "S"))

  # Sin la columna tipo_casilla debe fallar de forma controlada.
  expect_error(
    aelectoral2:::eliminar_especiales(tibble::tibble(votos = 1:3)),
    "tipo_casilla"
  )
})

test_that("ganador() picks the party with the most votes per level", {
  bd <- tibble::tibble(
    seccion          = c("0001", "0002"),
    ele_morena_pm_21 = c(100, 30),
    ele_pan_pm_21    = c(50, 80)
  )

  out <- aelectoral2:::ganador(bd, "seccion", "pm_21")

  expect_true("ganador_pm_21" %in% names(out))
  expect_equal(out$ganador_pm_21, c("ele_morena_pm_21", "ele_pan_pm_21"))
})
