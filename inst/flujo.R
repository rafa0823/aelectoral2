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

shp <- ElectoralSHP$new(unidad = "secc_22", entidad = "cdmx")

cdmx_secc$colapsar_base("bd_partido", filtro = shp$shp$secc_22_cdmx |>
                          filter(distritol_22 == "09_18") |>
                          as_tibble() |>
                          select(seccion))
# cdmx_secc$calcular_irs |> debug()
cdmx_secc$calcular_irs(ano = "2020", base = "bd_partido")

cdmx_secc$fusionar_shp(shp = shp$shp$secc_22_cdmx,
                       bd = "bd_partido")

tablero <- Tablero$new(info_seccion = cdmx_secc)

tablero$info_seccion$shp
