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
                                       eleccion = NA_character_,
                                       entidad = NA_character_,
                                       initialize = function(eleccion, entidad){
                                         self$eleccion <- eleccion
                                         self$entidad <- entidad

                                         self$obtener_bd()

                                       },
                                       obtener_bd = function(){
                                         self$bd <- leer_base(eleccion = self$eleccion,
                                                              entidad = self$entidad)# %>%
                                           # limpiar_base()
                                         #indica si hay columnas con números pues probablemente sea un error manual
                                         # revisar_nombres(self$bd)
                                       },
                                       agregar_bd = function(){
                                         #caso federal
                                         if(n_distinct(self$tipo) == 1 & self$tipo == "Federal"){
                                           if(n_distinct(self$ano) == 1){ # por casilla

                                             self$bd <- self$bd %>% imap(~{
                                               sufijo(.x, .y, geograficas$columna)
                                             }) %>%
                                               reduce(full_join)

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
