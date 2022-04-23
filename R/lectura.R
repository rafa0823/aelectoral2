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
  estado <- if_else(grepl("df_|pr_",eleccion), "nac",entidad)
  res <- readr::read_rds(system.file(glue::glue("electoral/{estado}_{eleccion}.rda"),
                                     package = "aelectoral2",
                                     mustWork = TRUE)) %>% tibble::as_tibble()
  if(estado == "nac") {
    nombre <- diccionario %>% filter(abreviatura == !!entidad) %>% pull(id_estado)
    res <- res %>% filter(estado == !!nombre)
    }
  return(res)
}

reducir <- function(bd, llave){
  bd %>% group_by(across(all_of(llave))) %>%
    summarise(across(starts_with("ele_"), ~sum(.x,na.rm = T))) %>% ungroup
}

