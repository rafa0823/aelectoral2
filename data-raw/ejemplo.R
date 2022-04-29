devtools::load_all()
# devtools::install()
# library(aelectoral2)
library(tidyr)
bd <- Electoral$new("df_21", entidad = "mex",
                    llaves = c("seccion", "distritof", "distritol", "municipio"),
                    extranjero = T, especial = "repartir")

c("df_18", "df_15","pr_18", "cp_22") %>% purrr::walk(~{
  bd$agregar_bd(.x, entidad = "mex")
})


# Agregar pm_21 con extraordinaria ----------------------------------------
bd$agregar_bd("pm_21", entidad = "mex",extraordinaria = c(eleccion = "pmext_21", entidad = "mex"))
# Agregar bds auxiliares ---------------------------------------------


cat <- cat_utm_22 %>% filter(estado == 15) %>%
  distinct(estado, seccion, unidad_territorial, sede) %>%
  mutate(seccion = paste(estado, seccion, sep = "_")) %>% select(-estado)

bd$agregar_manual(cat, by = "seccion")

# Agregar regiones --------------------------------------------------------

reg <- regiones %>% select(region, municipio)

# reg %>% anti_join(bd$bd, by = c("municipio" = "nombre_municipio_pm_21"))
bd$agregar_manual(reg, by = c("nombre_municipio_pm_21" = "municipio"))

# Agregar presidentes municipales -----------------------------------------

presidentes <- presidentes_mpos_mex %>% select(1:3) %>%
  mutate(nombre_municipio = stringr::str_replace(nombre_municipio,"CASTANEDA", "CASTAÑEDA"))

bd$agregar_manual(presidentes, by = c("nombre_municipio_pm_21" = "nombre_municipio"))
nrow(bd$bd)


# Repartir coalicion partido ----------------------------------------------

bd$partido(nivel = "distritof_21",eleccion = "df_21")
bd$partido(nivel = "distritof_18",eleccion = "df_18")

bd$bd_partido$df_21
bd$bd_partido$df_18


# Coalición candidato -----------------------------------------------------
ja <- readr::read_delim("data-raw/df_21.csv",delim = "|",skip = 1,
                        locale = readr::locale(encoding = "CP1252"))
al_df_21 <- ja %>%
  filter(PARTIDO_CI %in% c("PAN_PRI_PRD", "PVEM_PT_MORENA")) %>%
  transmute(distritof_21 = paste(formatC(ESTADO, width = 2, flag = 0),
                              formatC(DISTRITO, width = 2, flag = 0),sep = "_"),
            coalicion = tolower(PARTIDO_CI))

usethis::use_data(al_df_21, overwrite = T)

bd$candidato(al_df_21,nivel = "distritof_21", "df_21")

bd$bd_candidato

