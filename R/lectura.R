#' Title
#'
#' @param ano
#' @param tipo
#' @param eleccion
#' @param entidad
#' @param normal
#' @param nivel
#' @param inicial
#'
#' @import dplyr
#' @return
#' @export
#'
#' @examples
#'
leer_base <- function(inicial, ano, tipo, eleccion, entidad, normal, nivel){
  wd <- glue::glue("{inicial}/{tipo}/{ano}")
  if(tipo == "Federal") wd <- glue::glue("{wd}/{eleccion}_{nivel}.csv")
  if(tipo == "Local"){
    wd <- glue::glue("{wd}/{eleccion}/{entidad}_")
    if(normal) wd <- glue::glue("{wd}normal_{nivel}.csv") else wd <- glue::glue("{wd}extraordinaria_{nivel}.csv")
  }
  return(readr::read_csv(wd))
}

limpiar_base <- function(bd){
  # quitar enters
  bd <- bd %>% janitor::clean_names() %>%
    mutate(across(where(is.character),
                  ~stringr::str_replace_all(string = .x,pattern = "\\n",replacement = " ")))

  return(bd)
}

revisar_nombres <- function(bd){
  nombres <- bd %>% select(-contains("independiente")) %>% #en el caso de independientes sí pueden haber números
    select(matches("[[:digit:]]")) %>% names
  if(length(nombres)>0) warning(glue::glue("\n Revisar las columnas {paste(nombres, collapse = ', ')} \n\n Las columnas que tienen números es porque los nombres están repetidos y readr les agrega el número de la columna.
                                         \n Esto es por un error manual y lo deberá arreglar Andrés."))
  return(nombres)
}

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
