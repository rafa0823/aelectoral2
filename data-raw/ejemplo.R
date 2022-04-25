devtools::load_all()
# devtools::install()
# library(aelectoral2)
bd <- Electoral$new("df_21", entidad = "mex", extranjero = F)

bd$agregar_bd("df_18", entidad = "mex")
bd$agregar_bd("pr_18", entidad = "mex")

bd$todas$df_21
bd$bd
bd$agregar_variables(eleccion = "df_21", variables = c("estado","nombre_estado", "distritof_21"))
bd$agregar_variables(eleccion = "df_18", variables = c("distritof_18"))
bd$bd
