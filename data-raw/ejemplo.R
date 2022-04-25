devtools::load_all()
# devtools::install()
# library(aelectoral2)
library(tidyr)
bd <- Electoral$new("df_21", entidad = "mex", extranjero = T)

bd$agregar_bd("df_18", entidad = "mex",llaves = c("seccion", "distritof", "distritol"))
bd$agregar_bd("pr_18", entidad = "mex")
