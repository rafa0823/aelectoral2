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
                                       extranjero = NA,
                                       especiales = NA,
                                       llaves = NULL,
                                       initialize = function(eleccion, entidad, extranjero = T, especiales = T){
                                         self$eleccion <- eleccion
                                         self$entidad <- entidad
                                         self$extranjero <- extranjero
                                         self$especiales <- especiales

                                         self$obtener_bd()
                                         self$todas <- list(self$bd) %>% purrr::set_names(eleccion)

                                         if(!self$extranjero){
                                           self$eliminar_votoExtranjero()
                                         }

                                         if(!self$especiales){
                                           self$eliminar_especiales()
                                         }


                                       },
                                       obtener_bd = function(){
                                         self$bd <- leer_base(eleccion = self$eleccion,
                                                              entidad = self$entidad)
                                       },
                                       agregar_variables = function(eleccion, variables){
                                         agregar_variables(self, eleccion, variables)
                                       },
                                       agregar_bd = function(eleccion, entidad, llaves = NULL){
                                         # llave <- match.arg(llave, "seccion")
                                         add <- leer_base(eleccion = eleccion,
                                                          entidad = entidad)
                                         self$todas <- self$todas %>% append(list(add) %>% purrr::set_names(eleccion))

                                         if(is.null(self$llaves)){
                                           self$llaves <- llaves
                                           self$bd <- self$bd %>% reducir(self$llaves)
                                         }

                                         if(!is.null(llaves)){
                                           self$llaves <- self$llaves %>% append(llaves)
                                         }

                                         if(!self$especiales){
                                           add <- add %>% eliminar_especiales()
                                         }

                                         if(!self$extranjero){
                                           add <- add %>% eliminar_votoExtranjero()
                                         }

                                         self$bd <- self$bd %>% full_join(
                                           add %>% reducir(self$llaves), by = "seccion"
                                         )

                                       },
                                       agregar_manual = function(bd, by){
                                         self$bd <- self$bd %>% full_join(
                                           bd, by = by
                                         )
                                       },
                                       eliminar_especiales = function(){
                                         self$bd <- eliminar_especiales(self$bd)
                                       },
                                       eliminar_votoExtranjero = function(){
                                         self$bd <- eliminar_votoExtranjero(self$bd)
                                       }
                         ))
