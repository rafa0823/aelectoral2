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

  res <- wd %>% map(~readr::read_csv(.x)) %>% purrr::set_names(glue::glue("{tipo}/{ano}/{eleccion}"))
  return(res)
}

limpiar_base <- function(bd){
  # quitar enters
  bd <- bd %>% map(~{
    .x %>% janitor::clean_names() %>%
      mutate(across(where(is.character),
                    ~stringr::str_replace_all(string = .x,pattern = "\\n",replacement = " ")))
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

sufijo <- function(bd, nombre, nivel){
  aux <- nombre %>% str_split("/") %>% pluck(1)
  s <- glue::glue("{substr(aux[3],1,1)}{substr(aux[1],1,1)}_{substr(aux[2],3,4)}") %>% tolower
  bd <- bd %>% rename_at(vars(-!!rlang::sym(nivel)),function(x) paste(x, s, sep = "_"))
  return(bd)
}
