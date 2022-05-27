#' Elimina las casillas especiales (tipo_casilla = "S")
#'
#' @param bd Base de datos a la que se le eliminan las casilla especiales
#'
#' @return Data frame sin casillas especiales (tipo_casilla = "S")
#' @examples  eliminar_especiales(bd)
eliminar_especiales <- function(bd){
  if("tipo_casilla" %in% names(bd)){
    aux <- bd %>% filter(tipo_casilla != "S")

    message(glue::glue("Se eliminaron {nrow(bd)-nrow(aux)} casillas especiales"))
  } else{
    stop(glue::glue("Ha elegido una base de datos que no es por casilla. O sólo falta la variable tipo_casilla"))
  }

  return(aux)
}

#' Reparte votos en tipo_casilla = "S"
#'
#' @param bd Base de datos a repartir
#'
#' @return Data frame con las casillas especiales (tipo_casilla = "S") repartidas
#' @examples repartir_especiales(bd)
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

#' Se eliminan los votos que están en las secciones = 0000
#'
#' @param bd Base de datos a la que se le eliminan los votos en el extranjero
#' @return Data frame sin las secciones iguales a 0000
#' @examples bd %>%  eliminar_votoExtranjero()
eliminar_votoExtranjero <- function(bd){
  aux <- bd %>%  filter(seccion != "0000")
  message(glue::glue("Se eliminaron {nrow(bd)-nrow(aux)} casillas de voto extranjero"))
  return(aux)
}
