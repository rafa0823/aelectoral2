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
                                       todas = NULL,
                                       eleccion = NA_character_,
                                       entidad = NA_character_,
                                       extranjero = T,
                                       especiales = T,
                                       llave = NULL,
                                       initialize = function(eleccion, entidad){
                                         self$eleccion <- eleccion
                                         self$entidad <- entidad

                                         self$obtener_bd()

                                         self$todas <- list(self$bd)

                                       },
                                       obtener_bd = function(){
                                         self$bd <- leer_base(eleccion = self$eleccion,
                                                              entidad = self$entidad)
                                       },
                                       agregar_bd = function(eleccion, entidad, llave = "seccion"){
                                         llave <- match.arg(llave, "seccion")
                                         add <- leer_base(eleccion = eleccion,
                                                   entidad = entidad)
                                         self$todas <- self$todas %>% append(list(add))

                                         if(is.null(self$llave)){
                                           self$bd <- self$bd %>% reducir(llave)
                                           self$llave <- llave
                                         }

                                         self$bd <- self$bd %>% full_join(
                                           add %>% reducir(llave), by = llave
                                         )

                                       },
                                       eliminar_especiales = function(){
                                         if("tipo_casilla" %in% names(self$bd)){
                                           self$bd <- eliminar_especiales(self$bd)
                                           self$especiales <- F
                                         } else{
                                           warning(glue::glue("Ha elegido una base de datos que no es por casilla."))
                                         }
                                       },
                                       eliminar_votoExtranjero = function(){
                                         self$bd <- eliminar_votoExtranjero(self$bd)
                                         self$extranjero <- F
                                       }
                         ))
