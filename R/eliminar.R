eliminar_especiales <- function(bd){
  if("tipo_casilla" %in% names(bd)){
    aux <- bd %>% filter(tipo_casilla != "S")

    message(glue::glue("Se eliminaron {nrow(bd)-nrow(aux)} casillas especiales"))
  } else{
    warning(glue::glue("Ha elegido una base de datos que no es por casilla."))
  }

  return(aux)
}

eliminar_votoExtranjero <- function(bd){
  aux <- bd %>%  filter(seccion != 0)
  message(glue::glue("Se eliminaron {nrow(bd)-nrow(aux)} casillas de voto extranjero"))
  return(aux)
}
