devtools::load_all()

ejemplo <- Electoral$new(inicial = "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos",
  ano = 2011, tipo = "Local", eleccion = "Municipio", entidad = "michoacan")


ejemplo2 <- Electoral$new(inicial = "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos",
                         ano = 2018, tipo = "Federal", eleccion = "Presidente")

ejemplo3 <- Electoral$new(inicial = "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos",
                          ano = c(2012, 2015, 2018),
                          tipo = rep("Federal",3),
                          eleccion = c("Diputado"))


ejemplo2$eliminar_especiales()
ejemplo2$eliminar_votoExtranjero()


ejemplo2$bd %>% slice(1:10) %>% view
ejemplo3$bd %>% filter(ext_contigua != 0) %>% slice(1:10) %>% view

# sandbox -----------------------------------------------------------------

sep <- ejemplo2$bd %>% select(-matches("[[:digit:]]")) %>% names %>% strsplit("_") %>% discard(~length(.x) == 1)
aliados <- sep %>% do.call(c,.) %>% table() %>% as_tibble() %>%
  filter(n > 1, !`.` %in% c("casilla", "id", "tipo", "votos", "acta", "clave","nombre"))

ejemplo2$bd %>% select(contains)

ejemplo2$bd %>% select(contains(aliados %>% pull(1))) %>%
  select(-contains(c("estado", "nombre_estado", "num_acta_impreso", "observaciones")))
