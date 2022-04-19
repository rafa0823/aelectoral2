devtools::load_all()

ejemplo <- Electoral$new(inicial = "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos",
  ano = 2011, tipo = "Local", eleccion = "Municipio", entidad = "michoacan", nivel = "casilla")


ejemplo2 <- Electoral$new(inicial = "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos",
                         ano = 2012, tipo = "Federal", eleccion = "Presidencia", nivel = "casilla")


ejemplo2$eliminar_especiales()


