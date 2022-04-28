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
  estado <- if_else(grepl("df_|pr_|cp_",eleccion), "nac",entidad)
  res <- readr::read_rds(system.file(glue::glue("electoral/{estado}_{eleccion}.rda"),
                                     package = "aelectoral2",
                                     mustWork = TRUE)) %>% tibble::as_tibble()
  if(estado == "nac") {
    if(entidad != "nacional"){
      nombre <- diccionario %>% filter(abreviatura == !!entidad) %>% pull(id_estado)
      res <- res %>% filter(estado == !!nombre)
    }}

  return(res)
}

reducir <- function(bd, llaves){
  llaves_bd <- NULL

  for( i in seq_along(llaves)){
    llaves_bd <- llaves_bd %>% append(names(bd)[grepl(llaves[i], names(bd))])
  }

  bd %>% group_by(across(all_of(llaves_bd))) %>%
    summarise(across(c(starts_with("ele_"), starts_with("cp_")), ~sum(.x,na.rm = T))) %>% ungroup
}
