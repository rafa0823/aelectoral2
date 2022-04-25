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

reducir <- function(bd, llaves){
  llaves_bd <- NULL
  for( i in seq_along(llaves)){
    llaves_bd <- llaves_bd %>% append(names(bd)[grepl(llaves[i], names(bd))])
  }

  bd %>% group_by(across(all_of(llaves_bd))) %>%
    summarise(across(starts_with("ele_"), ~sum(.x,na.rm = T))) %>% ungroup
}

agregar_variables <- function(self, eleccion, variables){
  vars <- self$todas %>% purrr::pluck(eleccion) %>% distinct(seccion, across(all_of(variables)))
  self$bd <- self$bd %>% left_join(vars) %>% relocate(variables, .after = 1)
}
