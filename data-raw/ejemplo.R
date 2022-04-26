devtools::load_all()
# devtools::install()
# library(aelectoral2)
library(tidyr)
bd <- Electoral$new("df_21", entidad = "mex", extranjero = T)


bd$agregar_bd("df_18", entidad = "mex",llaves = c("seccion", "distritof", "distritol", "municipio"))


# Agregar pm_21 con extraordinaria ----------------------------------------
bd$agregar_bd("pm_21", entidad = "mex",extraordinaria = c(eleccion = "pmext_21", entidad = "mex"))

bd$bd %>% names
bd$bd %>% nrow
# Agregar consulta popular 22 ---------------------------------------------


bd$agregar_bd("cp_22", entidad = "mex")

data(cat_utm_22)
cat <- cat_utm_22 %>% filter(estado == 15) %>%
  distinct(seccion, unidad_territorial, sede)

bd$bd %>% nrow
bd$agregar_manual(cat, by = "seccion")
bd$bd %>% nrow

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


