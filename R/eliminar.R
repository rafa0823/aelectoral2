eliminar_especiales <- function(bd){
  aux <- bd %>% filter(tipo_casilla != "S")

  message(glue::glue("Se eliminaron {nrow(bd)-nrow(aux)} casillas"))
  return(aux)
}

eliminar_votoExtranjero <- function(bd){
  aux <- bd %>%  filter(seccion != 0)
  message(glue::glue("Se eliminaron {nrow(bd)-nrow(aux)} casillas"))
  return(aux)
}
