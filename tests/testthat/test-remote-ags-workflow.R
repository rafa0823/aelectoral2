test_that("Remote workflow works for Aguascalientes (ags)", {
  # This test verifies the complete workflow for ags using remote data.
  # REQUIRES: 
  # 1. Google Drive authentication.
  # 2. 'aelectoral_data/electoral/ags/pm_21.rda' on Drive.
  # 3. 'aelectoral_data/electoral/ags/dl_21.rda' on Drive.
  # 4. 'aelectoral_data/shp/secc_21/ags/01.rda' on Drive.
  skip_if_no_drive()

  entidad <- "ags"
  elecciones <- c("pm_21", "dl_21")
  partidos <- c("pan", "pri", "prd", "morena", "total")

  # To truly test remote fetching, we should ensure local files are NOT found.
  # However, we don't want to delete package files in a test.
  # Instead, we rely on the fact that if they are missing in the local R session's
  # system.file path (which they won't be if already installed), it triggers remote.
  # For a manual test, rename inst/electoral/ags and inst/shp/secc_21/01.rda.

  message("--- Starting AGS Remote Workflow Test ---")

  # 1. Initialize
  bd <- Electoral$new(
    eleccion = elecciones[1],
    entidad = entidad,
    partidos = partidos,
    extranjero = FALSE,
    especiales = FALSE
  )
  
  # 2. Process first election
  bd$partido(elecciones[1])
  bd$voto_relativo("bd_partido", elecciones[1])
  bd$calcular_ganador("bd_partido", elecciones[1])

  # 3. Add second election
  bd$agregar_bd(elecciones[2])
  bd$partido(elecciones[2])
  bd$voto_relativo("bd_partido", elecciones[2])
  bd$calcular_ganador("bd_partido", elecciones[2])

  expect_true(all(elecciones %in% bd$elecciones_agregadas))
  expect_true(length(bd$bd_partido) == 2)

  # 4. Spatial Join (This will test remote shp fetching if missing)
  # Ensure we use the new standardized fetcher
  shp <- ElectoralSHP$new(unidad = "secc_21", entidad = entidad)
  
  # 5. Colapse and Join
  bd$colapsar_base("bd_partido")
  bd$fusionar_shp(shp = shp$shp$secc_21_ags, base = "bd_partido")
  
  expect_true("sf" %in% class(bd$shp$seccion))
  expect_gt(nrow(bd$shp$seccion), 0)
  
  message("--- AGS Remote Workflow Test Completed Successfully ---")
})
