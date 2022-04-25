devtools::load_all()
# devtools::install()
# library(aelectoral2)
bd <- Electoral$new("df_21", entidad = "mex", extranjero = F)

bd$agregar_bd("df_18", entidad = "mex")
bd$agregar_bd("pr_18", entidad = "mex")
bd$bd %>% nrow


