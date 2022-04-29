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
                                       bd_partido = list(),
                                       bd_candidato = list(),
                                       eleccion = NA_character_,
                                       entidad = NA_character_,
                                       extranjero = NA,
                                       especiales = NA,
                                       llaves = NULL,
                                       initialize = function(eleccion, entidad, llaves = "seccion", extranjero = T, especiales = NULL){
                                         self$eleccion <- eleccion
                                         self$entidad <- entidad
                                         self$extranjero <- extranjero
                                         self$especiales <- especiales
                                         self$llaves <- c("estado", llaves)

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

{if(self$extranjero) 'Se mantiene el voto en el etranjero' else 'Se elimina el voto en el extranjero'}
Criterio de casillas especiales: {if(is.null(self$especiales)) 'ninguna acción especial realizada' else self$especiales}

{if(length(self$bd_partido)> 0)  paste('Elecciones repartidas por partido:', paste(names(self$bd_partido), collapse = ', ')) else ''}
{if(length(self$bd_candidato)> 0) paste('Elecciones repartidas por candidato:', paste(names(self$bd_candidato), collapse = ', ')) else ''}
                                                      ")
                                         )

                                         return(mensaje)
                                       },
                                       obtener_bd = function(){
                                         self$bd <- leer_base(eleccion = self$eleccion,
                                                              entidad = self$entidad)
                                       },
                                       partido = function(nivel, eleccion){
                                         aux_c <- self$bd %>% repartir_coalicion(nivel = nivel, eleccion = eleccion)

                                         self$bd_partido <- self$bd_partido %>%
                                           append(list(aux_c) %>% purrr::set_names(eleccion))
                                       },
                                       candidato = function(alianzas, nivel, eleccion){
                                         aux_c <- repartir_candidato(bd = self$bd_partido[[eleccion]],
                                                                     alianzas, nivel, eleccion)

                                         self$bd_candidato <- self$bd_candidato %>%
                                           append(list(aux_c) %>% purrr::set_names(eleccion))
                                       },
                                       agregar_bd = function(eleccion, entidad, extraordinaria = NULL){
                                         # llave <- match.arg(llave, "seccion")
                                         add <- leer_base(eleccion = eleccion,
                                                          entidad = entidad)

                                         self$todas <- self$todas %>% append(list(add) %>% purrr::set_names(eleccion))

                                         if(!is.null(extraordinaria)){
                                           ext <- leer_base(eleccion = extraordinaria[["eleccion"]],
                                                            entidad = extraordinaria[["entidad"]])

                                           self$todas <- self$todas %>% append(list(ext) %>%
                                                                                 purrr::set_names(extraordinaria[["eleccion"]]))
                                         }


                                         add <- add %>% self$accion_especiales(self$especiales)

                                         if(!is.null(extraordinaria)){
                                           ext <- ext %>% self$accion_especiales(self$especiales)
                                         }

                                         if(!self$extranjero){
                                           add <- add %>% eliminar_votoExtranjero()
                                           if(!is.null(extraordinaria)){
                                             ext <- ext %>% eliminar_votoExtranjero()
                                           }
                                         }

                                         if(!is.null(extraordinaria)){
                                           ext_r <- ext %>% reducir(self$bd, self$llaves) %>%
                                             mutate(!!rlang::sym(glue::glue("extraordinaria_{extraordinaria[['eleccion']]}")) := T)

                                           add <- add %>% reducir(self$bd, self$llaves) %>% mutate(!!rlang::sym(glue::glue("extraordinaria_{extraordinaria[['eleccion']]}")) := F) %>%
                                             anti_join(
                                               ext_r, by = "seccion"
                                             ) %>%
                                             bind_rows(
                                               ext_r
                                             )
                                         } else{
                                           add <- add %>% reducir(self$bd, self$llaves)
                                         }

                                         self$bd <- self$bd %>% full_join(
                                           add, by = c("estado", "seccion")
                                         )

                                       },
                                       agregar_manual = function(bd, by){
                                         self$bd <- self$bd %>% full_join(
                                           bd, by = by
                                         )
                                       },
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
                                       eliminar_votoExtranjero = function(){
                                         self$bd <- eliminar_votoExtranjero(self$bd)
                                       }
                         ))
