test_that("Production usage example works for CDMX 2024", {
  # This test replicates the user's provided usage example
  # to ensure end-to-end functionality for 2024 data.
  
  entidad <- "cdmx"
  elecciones <- c("pm_24", "gb_24", "dl_24") # Subset for speed
  partidos <- c("morena", "pan", "pri", "mc", "prd", "pvem", "pt", "panal", "total")

  # 1. Initialize
  expect_message(
    bd <- Electoral$new(
      elecciones[1],
      entidad = entidad,
      partidos = partidos,
      extranjero = FALSE,
      especiales = FALSE
    ),
    "Initializing Electoral object"
  )
  
  # 2. Process first election
  bd$partido(elecciones[1])
  bd$voto_relativo("bd_partido", elecciones[1])
  bd$calcular_ganador("bd_partido", elecciones[1])
  bd$obtener_degradado_ganador(base = "bd_partido", eleccion = elecciones[1])

  # 3. Add other elections
  purrr::walk(elecciones[-1], ~ {
    bd$agregar_bd(.x)
    bd$partido(.x)
    bd$voto_relativo("bd_partido", .x)
    bd$calcular_ganador("bd_partido", .x)
    bd$obtener_degradado_ganador(base = "bd_partido", eleccion = .x)
  })

  expect_true(all(elecciones %in% bd$elecciones_agregadas))
  expect_true(length(bd$bd_partido) == length(elecciones))

  # 4. Spatial Join
  shp <- ElectoralSHP$new(unidad = "secc_24", entidad = entidad)
  
  # 5. Colapse and Join
  bd$colapsar_base("bd_partido")
  bd$fusionar_shp(shp = shp$shp$secc_24_cdmx, base = "bd_partido")
  
  expect_true("sf" %in% class(bd$shp$seccion))
  expect_true(nrow(bd$shp$seccion) > 0)
})
