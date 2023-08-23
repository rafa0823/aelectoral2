#' #' Title
#' #'
#' #' @param bd base de datos de cualquier elección en crudo (sin ningún procesamiento)
#' #' @param identificadores nivel de observación para el que se aplica el análisis
#' #' @param partido los partidos identificados para cada elección. Calculados con la función detectar partidos
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#'
#'
#' repartir_coalicion <- function(bd, identificadores, partido){
#'   # names(tempo %>% select(matches("e{1,}")))
#'   res <- bd %>%
#'     select(any_of({{identificadores}}),
#'            matches(glue::glue("_{partido}\\b")) | matches(glue::glue("_{partido}_"))) %>%
#'     pivot_longer(!any_of({{identificadores}}),
#'                  names_to="partido_coalicion",
#'                  values_to="nvotos") %>%
#'
#'     mutate(npartidos=stringr::str_count(partido_coalicion, pattern = "_"),
#'            votos_repartidos=nvotos/npartidos) %>%
#'     group_by(across({{identificadores}})) %>%
#'     summarise("ele_{partido}":=sum(votos_repartidos, na.rm=T))
#'
#'   return(res)
#' }

#' Title
#'
#' @param bd base de datos de cualquier elección en crudo (sin ningún procesamiento)
#'
#' @return
#' @export
#'
#' @examples
detectar_partidos <- function(bd){
  res <- names(bd %>% dplyr::select(dplyr::contains("ele_")))[stringr::str_count(
    names(bd %>%
            dplyr::select(dplyr::contains("ele_"))), "_")==1] %>%
    stringr::str_remove(pattern = "ele_")
  return(res)
}




#' Title
#'
#' @param bd base de datos ya procesada
#' @param eleccion_nivel nivel de las elección a la que se le pone el sufijo (df,dl,pm)
#' @param eleccion_year año de la eleción de interés (15,18,21)
#'
#' @return
#' @export
#'
#' @examples
insertar_sufijo <- function(bd, eleccion_nivel, eleccion_year){
  res <- bd %>%
    dplyr::rename_with(~paste0(.x,(glue::glue("_{eleccion_nivel}_{eleccion_year}"))),
                 .cols = dplyr::contains("ele_"))
  return(res)
}
