
#' Clase R6 que construye un conjunto de bases electorales comparables
#'
#' @description
#' Se inicia con la base de una elección en específico y a ella se le pueden ir agregando elecciones que se juntan por seccion
#'
#' @details
#' Este conjunto de bases electorales pueden ser de distintas elecciones, candidatos, años, tipos de eleccion; se puede decidir la forma en la se reparten los votos en el extranjero y las casillas especiales; así como agregar distintas variables geográficas como el municipio o el estado.


Electoral <- R6::R6Class("Electoral",
                         public = list(bd = NA,
                                       todas = NULL,
                                       bd_partido = list(),
                                       bd_candidato = list(),
                                       eleccion = NA_character_,
                                       entidad = NA_character_,
                                       tipo_eleccion = NA_character_,
                                       extranjero = NA,
                                       especiales = NA,
                                       llaves = NULL,

                                       #' #' Initialize: Obtener base de datos electoral
                                       #' @description
                                       #' Lo que hace es leer una base de datos electoral, darle formato el formato documentado para unirla o compararla con otras elecciones y la resume por sección.
                                       #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                                       #' @param entidad Cuando es nacional es "nac", cuando es local se pone la abreviatura oficial, por ejemplo "chis", "dgo", "mex".
                                       #' @param llaves Son las claves cartográficas de los niveles. Por default la unidad mínima es sección y está acompañada de estado.
                                       #' @param tipo_eleccion Por default es "MR" refiriéndose a mayoría relativa.
                                       #' @param extranjero Se refiere a si se desea incluir los votos en el extrajero, entendidos como sección 0000. El default es TRUE
                                       #' @param especiales Las casillas especiales se pueden "eliminar", "repartir" o dejar como están es el parámetro default.
                                       #'
                                       #' @return Un data frame con la elección seleccionada
                                       #' @export
                                       #' @examples
                                       #' bd <- Electoral$new("df_21", entidad = "dgo",
                                       #' llaves = c("seccion", "distritof", "distritol", "municipio"),
                                       #' extranjero = T, especial = "repartir")

                                       initialize = function(eleccion, entidad, llaves = "seccion",
                                                             tipo_eleccion = "MR",
                                                             extranjero = T, especiales = NULL){
                                         self$eleccion <- eleccion
                                         self$entidad <- entidad
                                         self$extranjero <- extranjero
                                         self$especiales <- especiales
                                         self$llaves <- c("estado", llaves)
                                         self$tipo_eleccion <- tipo_eleccion

                                         self$obtener_bd()

                                         self$todas <- list(self$bd) %>% purrr::set_names(eleccion)

                                         if(!self$extranjero){
                                           self$eliminar_votoExtranjero()
                                         }

                                         self$bd <- self$bd %>% self$accion_especiales(self$especiales)

                                         self$bd <- self$bd %>% reducir(NULL, self$llaves)

                                       },
                                       print = function(){

                                         mensaje <- cat(
                                           glue::glue("Entidad: {self$entidad} \nElecciones agregadas: {paste(self$todas %>% names, collapse = ', ')}

Variables cartográficas agregadas en bd: {paste(self$llaves, collapse = ', ')}

Tipo de elección: {self$tipo_eleccion}
{if(self$extranjero) 'Se mantiene el voto en el extranjero' else 'Se elimina el voto en el extranjero'}
Criterio de casillas especiales: {if(is.null(self$especiales)) 'ninguna acción especial realizada' else self$especiales}

{if(length(self$bd_partido)> 0)  paste('Elecciones repartidas por partido:', paste(names(self$bd_partido), collapse = ', ')) else ''}
{if(length(self$bd_candidato)> 0) paste('Elecciones repartidas por candidato:', paste(names(self$bd_candidato), collapse = ', ')) else ''}
                                                      ")
                                         )

                                         return(mensaje)
                                       },
                                       #' @description
                                       #'Función basada en leer_base()
                                       #' @return tibble de la bd

                                       obtener_bd = function(){
                                         self$bd <- leer_base(eleccion = self$eleccion,
                                                              entidad = self$entidad,
                                                              tipo_eleccion = self$tipo_eleccion)
                                       },

                                       #' @description
                                       #' Esta función te indica cuales fueron las alianzas de la eleccion indicada
                                       #' @param nivel Nivel en el que se determinan las alianzas dependiendo de la unidad en la que se realiza la elección.
                                       #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                                       #'
                                       #' @return La lista de coaliciones que hubieron en la elección señalada.
                                       #' @examples
                                       coalicion = function(nivel, eleccion){
                                         if(!eleccion %in% names(self$todas)) stop("Favor de agregar la elección primero con el método agregar_bd")

                                         self$partido(nivel, eleccion)

                                         al <- leer_alianza(nivel, eleccion, self$entidad, self$bd)

                                         self$candidato(al, nivel, eleccion)
                                       },

                                       #' @description
                                       #' Reparte los voto de acuerdo con las coaliciones
                                       #' @param  nivel En el que se determinan las alianzas
                                       #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                                       #'
                                       #' @return Elección con los votos repartidos por alianza
                                       #' @examples
                                       partido = function(nivel, eleccion){
                                         aux_c <- self$bd %>% repartir_coalicion(nivel = nivel, eleccion = eleccion)

                                         self$bd_partido <- self$bd_partido %>%
                                           append(list(aux_c) %>% purrr::set_names(eleccion))
                                       },
                                       #' @description
                                       #' Reparte los votos por candidato de la elección, para ello necesita saber qué candidatos fueron en alianza y cual fue.
                                       #' @param alianzas  Proviene de coalicion() en la que se indican las alianzas de la elección
                                       #' @param nivel En el que se determinan las alianzas
                                       #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                                       #'
                                       #' @return Una base datos con los votos repartidos por candidato.
                                       #' @examples
                                       candidato = function(alianzas, nivel, eleccion){
                                         aux_c <- repartir_candidato(bd = self$bd_partido[[eleccion]],
                                                                     alianzas, nivel, eleccion)

                                         self$bd_candidato <- self$bd_candidato %>%
                                           append(list(aux_c) %>% purrr::set_names(eleccion))
                                       },

                                       #'@description
                                       #'Agrega una base de datos de la elección señalada y se la pega a la elección que se haya leído con obtener_bd.
                                       #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                                       #' @param entidad Cuando es nacional es "nac", cuando es local se pone la abreviatura oficial, por ejemplo "chis", "dgo", "mex".
                                       #'
                                       #' @return Tibble de la base de datos con la nueva elección resumidas por sección
                                       #' @examples
                                       #'  bd$agregar_bd(eleccion = "pm_21",entidad = "mex", extraordinaria = c(eleccion = "pmext_21", entidad = "mex"))

                                       agregar_bd = function(eleccion, entidad){

                                         add <- leer_base(eleccion = eleccion,
                                                          entidad = entidad, tipo_eleccion = self$tipo_eleccion)

                                         self$todas <- self$todas %>% append(list(add) %>% purrr::set_names(eleccion))

                                         add <- add %>% self$accion_especiales(self$especiales)

                                         if(!self$extranjero){
                                           add <- add %>% eliminar_votoExtranjero()
                                         }

                                         add <- add %>% reducir(self$bd, self$llaves)

                                         self$bd <- self$bd %>% full_join(
                                           add, by = c("estado", "seccion")
                                         )

                                       },

                                       #' @description
                                       #'Basada en la función full_join, se juntan bases de datos.
                                       #'
                                       #' @param bd base de datos que que se quiere juntar
                                       #' @param by variable por la que se une las bds
                                       #'
                                       #' @return
                                       #' @examples
                                       #' bd %>%  agregar_manual(df_21, "seccion")
                                       agregar_manual = function(bd, by){
                                         self$bd <- self$bd %>% full_join(
                                           bd, by = by
                                         )
                                       },

                                       #' @description
                                       #' Para dterminar lo que se va a hacer con las casillas especiales
                                       #' @param bd Base de datos electoral
                                       #' @param accion se puede dejar como están, repartir o eliminar
                                       #'
                                       #' @return Tibble de la bd
                                       #' @examples
                                       accion_especiales = function(bd, accion){
                                         if(!is.null(accion)){
                                           if(accion == "eliminar"){
                                             bd <- eliminar_especiales(bd)
                                           }

                                           if(accion == "repartir"){
                                             bd <- repartir_especiales(bd)
                                           }
                                         }
                                         return(bd)
                                       },

                                       #' @description
                                       #' Elimina el voto en el extanjero. Esta función filtra las secciones que son 0000.
                                       #'
                                       #' @return Devuelve el tibble de la elección sin los votos en el extanjero
                                       #' @examples
                                       eliminar_votoExtranjero = function(){
                                         self$bd <- eliminar_votoExtranjero(self$bd)
                                       }
                         ))


#' Clase R6 para leer y unir shapefiles
#'
#' @description
#' Se inicia leyendo un shp
#'
#' @details
#' Al shp leído se le pude agregar otrabase de datos

ElectoralSHP <- R6::R6Class("ElectoralSHP",
                            public = list(
                              shp = list(),
                              entidades = NULL,
                              #'@description
                              #'Lee un shapefile
                              #' @param unidad si es de municipio, estado, distrito, seccion, etc.
                              #' @param entidad el estado de donde es
                              #'
                              #' @return Una lista con shapefiles
                              #' @export
                              #' @examples
                              initialize = function(unidad, entidad){
                                self$entidades <- entidad
                                aux <- leer_shp(unidad, entidad)
                                self$shp <- self$shp %>% append(list(aux) %>% purrr::set_names(paste(unidad, entidad, sep = "_")))
                              },
                              print = function(){
                                cat(glue::glue("Entidad(es): {paste(self$entidades, collapse = ', ')} \n\n Shps agregados: {paste(names(self$shp), collapse = ', ')}"))
                              },
                              agregar_shp = function(unidad, entidad = NULL){
                                if(!entidad %in% self$entidades) self$entidades <- self$entidades %>% append(entidad)
                                aux <- leer_shp(unidad, entidad)
                                self$shp <- self$shp %>% append(list(aux) %>% purrr::set_names(paste(unidad, entidad, sep = "_")))
                              },
                              #'@description
                              #' Junta shapefiles
                              #' @param nivel Si la base que se va a juntar es por seccion, municipio, distrito, etc
                              #' @param bd base de datos que se le quiere pegar al shp
                              #'
                              #' @examples
                              juntar_bd = function(nivel, bd){
                                self$shp[[nivel]] <- join_shp_bd(self$shp[[nivel]], bd)
                              }
                            ))
