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
  estado <- if_else(grepl("_df_|_pr_",eleccion), "nac",entidad)
  res <- readr::read_rds(system.file(glue::glue("electoral/{estado}_{eleccion}.rda"),
                                     package = "aelectoral2",
                                     mustWork = TRUE))
  if(estado == "nac") {
    nombre <- diccionario %>% filter(abreviatura == !!entidad) %>% pull(estado)
    res <- res %>% filter(estado == !!nombre)
    }
  return(res)
}

