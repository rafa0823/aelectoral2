
#' Detecta partidos
#'
#' @param bd base de datos de cualquier elección en crudo (sin ningún procesamiento)
#'
#' @return vector de partidos
#'
detectar_partidos <- function(bd){
  res <- names(bd %>% dplyr::select(dplyr::contains("ele_")))[stringr::str_count(
    names(bd %>%
            dplyr::select(dplyr::contains("ele_"))), "_")==1] %>%
    stringr::str_remove(pattern = "ele_")
  return(res)
}

#' Inserta los sufijos a los datos
#'
#' @param bd base de datos ya procesada
#' @param eleccion_nivel nivel de las elección a la que se le pone el sufijo (df,dl,pm)
#' @param eleccion_year año de la eleción de interés (15,18,21)
#'
#' @return base de datos
#' @export
insertar_sufijo <- function(bd, eleccion_nivel, eleccion_year){
  res <- bd %>%
    dplyr::rename_with(~paste0(.x,(glue::glue("_{eleccion_nivel}_{eleccion_year}"))),
                 .cols = dplyr::contains("ele_"))
  return(res)
}
