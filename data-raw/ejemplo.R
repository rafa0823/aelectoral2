devtools::load_all()
# devtools::install()
# library(aelectoral2)
library(tidyr)
bd <- Electoral$new("df_21", entidad = "mex", extranjero = T)


bd$agregar_bd("df_18", entidad = "mex",llaves = c("seccion", "distritof", "distritol", "municipio"))


# Agregar pm_21 con extraordinaria ----------------------------------------
bd$agregar_bd("pm_21", entidad = "mex",extraordinaria = c(eleccion = "pmext_21", entidad = "mex"))
# Agregar consulta popular 22 ---------------------------------------------


bd$agregar_bd("cp_22", entidad = "mex")
data(cat_utm_22)
cat <- cat_utm_22 %>% filter(estado == 15) %>%
  distinct(seccion, unidad_territorial, sede)

bd$agregar_manual(cat, by = "seccion")

# Agregar regiones --------------------------------------------------------
data(regiones)
reg <- regiones %>% select(region, municipio)

# reg %>% anti_join(bd$bd, by = c("municipio" = "nombre_municipio_pm_21"))
bd$agregar_manual(reg, by = c("nombre_municipio_pm_21" = "municipio"))

# Agregar presidentes municipales -----------------------------------------

data("presidentes_mpos_mex")
presidentes <- presidentes_mpos_mex %>% select(1:3) %>%
  mutate(nombre_municipio = stringr::str_replace(nombre_municipio,"CASTANEDA", "CASTAÑEDA"))

presidentes %>% anti_join(bd$bd, by = c("nombre_municipio" = "nombre_municipio_pm_21"))
bd$agregar_manual(presidentes, by = c("nombre_municipio_pm_21" = "nombre_municipio"))
nrow(bd$bd)


# Repartir coalicion partido ----------------------------------------------

bd$partido(nivel = "distritof_21",eleccion = "df_21")
bd$partido(nivel = "distritof_18",eleccion = "df_18")

bd$bd_partido$df_21
bd$bd_partido$df_18


# Coalición candidato -----------------------------------------------------

alianzas_df18 <- readr::read_csv("data-raw/df_18_edomex.csv") %>%
  mutate(distritof_18 = formatC(distritof_18, width = 2, flag = 0))

undebug(repartir_candidato)
bd$candidato(alianzas_df18,nivel = "distritof_18", "df_18")

bd$bd_candidato

