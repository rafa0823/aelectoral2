devtools::load_all()

ejemplo <- Electoral$new(inicial = "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos",
  ano = 2011, tipo = "Local", eleccion = "Municipio", entidad = "michoacan", nivel = "casilla")


ejemplo2 <- Electoral$new(inicial = "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos",
                         ano = 2012, tipo = "Federal", eleccion = "Presidente", nivel = "casilla")

ejemplo3 <- Electoral$new(inicial = "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos",
                          ano = 2015, tipo = "Federal", eleccion = "Diputados", nivel = "casilla")


ejemplo3$eliminar_especiales()
ejemplo2$eliminar_votoExtranjero()
