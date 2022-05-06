
#' Title
#'
#' @param eleccion
#' @param entidad
#' @param tipo_eleccion
#'
#' @return
#' @export
#'
#' @import sf
#' @examples
leer_base <- function(eleccion, entidad, tipo_eleccion){
  estado <- if_else(grepl("df_|pr_|cp_",eleccion), "nac",entidad)
  res <- readr::read_rds(system.file(glue::glue("electoral/{estado}_{eleccion}.rda"),
                                     package = "aelectoral2",
                                     mustWork = TRUE)) %>% tibble::as_tibble()
  if(estado == "nac") {
    if(entidad != "nacional"){
      nombre <- diccionario %>% filter(abreviatura == !!entidad) %>% pull(id_estado)
      res <- res %>% filter(estado == !!nombre)
    }}

  if("mr_rp" %in% names(res)) res <- filter(res, mr_rp == !!tipo_eleccion)

  return(res)
}

#' Title
#'
#' @param bd
#' @param completa
#' @param llaves
#'
#' @return
#' @export
#'
#' @examples
reducir <- function(bd, completa, llaves){

  llaves_bd <- NULL

  for( i in seq_along(llaves)){
    agregar <- names(bd)[grepl(llaves[i], names(bd))]

    if(!is.null(completa)){
      if(all(is.na(match(agregar, names(completa))))) llaves_bd <- llaves_bd %>% append(agregar)
    } else{
      llaves_bd <- llaves_bd %>% append(agregar)
    }

  }
  if(! "estado" %in% llaves_bd) llaves_bd <- llaves_bd %>% append("estado")
  if(! "seccion" %in% llaves_bd) llaves_bd <- llaves_bd %>% append("seccion")

  bd %>% group_by(across(all_of(llaves_bd))) %>%
    summarise(across(c(starts_with("ele_"), starts_with("cp_")), ~sum(.x,na.rm = T))) %>% ungroup %>%
    # pegar estado a las llaves que no contenga la palabra nombre
    mutate(across(all_of(grep(pattern = "nombre_", invert = T, value = T, llaves_bd[is.na(match(llaves_bd, "estado"))])), ~paste(estado,.x,sep = "_")))
}

leer_shp <- function(unidad, entidad){
  if(entidad == "nacional") id <- diccionario %>% pull(id_estado) %>% stringr::str_pad(width = 2, pad = "0") else{
    id <- diccionario %>% filter(abreviatura %in% entidad) %>% pull(id_estado) %>% stringr::str_pad(width = 2, pad = "0")
  }

  res <- id %>% purrr::map(~{
    readr::read_rds(system.file(glue::glue("shp/{unidad}/{.x}.rda"),
                                     package = "aelectoral2",
                                     mustWork = TRUE)) %>% sf::st_transform(sf::st_crs(4326))
    }) %>% bind_rows()
}
