library(tidyverse)
library(leaflet)

cdmx_secc <- Electoral$new(eleccion = "df_21", entidad = "cdmx")
cdmx_secc$partido("df_21")
cdmx_secc$obtener_degradado_ganador(base = "bd_partido",
                                    colores_nombrados = set_names(paleta$colores,paleta$partidos),
                                    eleccion = "df_21")

c("dl_21", "pm_21") |>
  walk(~{
    cdmx_secc$agregar_bd(eleccion = .x)
    cdmx_secc$partido(eleccion = .x)
    # cdmx_secc$voto_relativo(base = "bd_partido", partidos = paleta$partidos, eleccion = .x)
    # cdmx_secc$calcular_ganador(base = "bd_partido", partidos = paleta$partidos, eleccion = .x)
    cdmx_secc$obtener_degradado_ganador(base = "bd_partido",
                                        colores_nombrados = set_names(paleta$colores,paleta$partidos),
                                        eleccion = .x)
  })

cdmx_secc$colapsar_base("bd_partido")

shp <- ElectoralSHP$new(unidad = "secc_22", entidad = "cdmx")

cdmx_secc$fusionar_shp(shp = shp$shp$secc_22_cdmx |>
                         filter(distritol_22 == "09_18"),
                       bd = "bd_partido")

cdmx_secc$shp
