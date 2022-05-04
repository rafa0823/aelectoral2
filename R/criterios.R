#' Title
#'
#' @param bd
#'
#' @return
#' @export
#'
#' @examples
eliminar_especiales <- function(bd){
  if("tipo_casilla" %in% names(bd)){
    aux <- bd %>% filter(tipo_casilla != "S")

    message(glue::glue("Se eliminaron {nrow(bd)-nrow(aux)} casillas especiales"))
  } else{
    stop(glue::glue("Ha elegido una base de datos que no es por casilla. O sólo falta la variable tipo_casilla"))
  }

  return(aux)
}

#' Title
#'
#' @param bd
#'
#' @return
#' @export
#'
#' @examples
repartir_especiales <- function(bd){
  if("tipo_casilla" %in% names(bd)){
    nominal <- grep("nominal", names(bd), value = T)
    aux <- bd %>%
      mutate(
             especial_p = 750*sum(tipo_casilla == "S")/n(),
             !!rlang::sym(nominal) := if_else(tipo_casilla == "S", 750, !!rlang::sym(nominal)) - especial_p
             ) %>%  select(-especial_p)

  } else{
    stop(glue::glue("Ha elegido una base de datos que no es por casilla. O sólo falta la variable tipo_casilla"))
  }

  return(aux)
}

#' Title
#'
#' @param bd
#'
#' @return
#' @export
#'
#' @examples
eliminar_votoExtranjero <- function(bd){
  aux <- bd %>%  filter(seccion != "0000")
  message(glue::glue("Se eliminaron {nrow(bd)-nrow(aux)} casillas de voto extranjero"))
  return(aux)
}
