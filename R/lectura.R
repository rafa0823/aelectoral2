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
  return(readr::read_csv(wd) %>% janitor::clean_names())
}
