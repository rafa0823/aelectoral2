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
leer_base <- function(eleccion, entidad){

  return(res)
}

limpiar_base <- function(bd){

  bd <- bd %>% map(~{
    aux <- .x %>%
      select(-matches(basura$columna)) %>%
      mutate(across(any_of(geograficas$columna), ~as.character(.x))) #variables geográficas character
    aux %>%
      mutate(across(all_of(names(aux)[is.na(match(names(aux), geograficas$columna))]), ~as.numeric(.x))) %>%
      mutate(across(where(is.character),
                    ~stringr::str_replace_all(string = .x,pattern = "\\n",replacement = " ")))# quitar enters
  })

  return(bd)
}

revisar_nombres <- function(bd){
  bd %>% imap(~{
    nombres <- .x %>% select(-contains("independiente")) %>% #en el caso de independientes sí pueden haber números
      select(matches("[[:digit:]]")) %>% names
    if(length(nombres)>0) warning(glue::glue("Base: {.y} \n Revisar las columnas {paste(nombres, collapse = ', ')} \n\n Las columnas que tienen números es porque los nombres están repetidos y readr les agrega el número de la columna.
                                         \n Esto es por un error manual y lo deberá arreglar Andrés."))
  })

}

sufijo <- function(bd, nombre, geo){
  aux <- nombre %>% str_split("/") %>% pluck(1)
  s <- glue::glue("{substr(aux[3],1,1)}{substr(aux[1],1,1)}_{substr(aux[2],3,4)}") %>% tolower
  bd <- bd %>% rename_at(vars(-any_of(geo)), function(x) paste(x, s, sep = "_"))
  return(bd)
}
