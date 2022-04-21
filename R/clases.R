#' Title
#'
#' @param inicial
#' @param ano
#' @param tipo
#' @param eleccion
#' @param entidad
#' @param normal
#' @param nivel
#'
#' @return
#' @export
#' @import dplyr
#' @examples

Electoral <- R6::R6Class("Electoral",
                         public = list(bd = NA,
                                       inicial = NA_character_,
                                       ano = NA_integer_,
                                       tipo = NA_character_,
                                       eleccion = NA_character_,
                                       entidad = NA_character_,
                                       normal = T,
                                       nivel = NA_character_,
                                       initialize = function(inicial = "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/Resultados definitivos",
                                                             ano, tipo, eleccion, entidad, normal = T, nivel = "casilla"){
                                         self$inicial <- inicial
                                         self$ano <- ano
                                         self$tipo <- tipo
                                         self$eleccion <- eleccion
                                         if(self$tipo == "Local") self$entidad <- entidad
                                         self$normal <- normal
                                         self$nivel <- nivel

                                         self$obtener_bd()

                                         self$join_bd()

                                       },
                                       obtener_bd = function(){
                                         self$bd <- leer_base(inicial = self$inicial, ano = self$ano, tipo = self$tipo, eleccion = self$eleccion,
                                                              entidad = self$entidad, normal = self$normal, nivel = self$nivel) %>%
                                           limpiar_base()
                                         #indica si hay columnas con números pues probablemente sea un error manual
                                         revisar_nombres(self$bd)
                                       },
                                       join_bd = function(){
                                         #caso federal
                                         if(n_distinct(self$tipo) == 1 & self$tipo == "Federal"){
                                           if(n_distinct(self$ano) == 1){ # por casilla

                                             self$bd <- self$bd %>% imap(~{
                                               sufijo(.x, .y,"clave_casilla")
                                             }) %>%
                                               reduce(full_join,by = "clave_casilla")

                                           } else{ #por sección
                                             self$bd <- self$bd %>% imap(~{
                                               .x %>% group_by(seccion) %>% tidyr::nest() %>%
                                                 sufijo(.y,"seccion")
                                                 }) %>%
                                               reduce(full_join, by = "seccion")

                                           }
                                         }
                                         # caso local
                                         if(n_distinct(self$tipo) == 1 & self$tipo == "Local"){

                                         }
                                       },
                                       eliminar_especiales = function(){
                                         if(self$nivel == "casilla"){
                                           self$bd <- eliminar_especiales(self$bd)
                                         } else{
                                           warning(glue::glue("Ha elegido una base de datos que no es por casilla."))
                                         }
                                       },
                                       eliminar_votoExtranjero = function(){
                                         self$bd <- eliminar_votoExtranjero(self$bd)
                                       }
                         ))
