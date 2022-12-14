
#' Función para leer una base de datos
#' Basada en la funcion read_rds del paquete readr
#'
#' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
#' @param entidad Cuando es nacional es "nac", cuando es local se pone la abreviatura oficial, por ejemplo "chis", "dgo", "mex".
#' @param tipo_eleccion Por default es "MR" refiriéndose a mayoría relativa.
#'
#' @return tibble de la base electoral
#' @examples leer_base(eleccion = eleccion,entidad = entidad, tipo_eleccion = self$tipo_eleccion)
leer_base <- function(eleccion, entidad, tipo_eleccion){
  estado <- if_else(grepl("df_|pr_|cp_",eleccion), "nacional",entidad)
  res <- readr::read_rds(system.file(glue::glue("electoral/{estado}/{eleccion}.rda"),
                                     package = "aelectoral2",
                                     mustWork = TRUE)) %>% tibble::as_tibble()
  if(estado == "nacional") {
    if(entidad != "nacional"){
      nombre <- aelectoral2::diccionario %>% filter(abreviatura == !!entidad) %>% pull(id_estado) %>% stringr::str_pad(width = 2, pad = "0")
      res <- res %>% filter(estado == !!nombre)
    }}

  if("mr_rp" %in% names(res)) res <- filter(res, mr_rp == !!tipo_eleccion)

  return(res)
}



#' Función para leer la base de datos de alianzas por partido y por nivel
#' También transforma la bd de alianzas al nivel de la base electoral.
#'
#' @param nivel Nivel en el que se determinan las alianzas dependiendo de la unidad en la que se realiza la elección.
#' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
#' @param entidad Cuando es nacional es "nac", cuando es local se pone la abreviatura oficial, por ejemplo "chis", "dgo", "mex".
#' @param bd_e Base de datos electoral a la ue se le van a pegar las coaliciones por partido.
#'
#' @return Regresa un data frame de alianzas
#' @examples leer_alianza(nivel, eleccion, self$entidad, self$bd)

leer_alianza <- function(nivel, eleccion, entidad, bd_e){
  estado <- if_else(grepl("df_|pr_",eleccion), "nacional",entidad)

  if(estado == "nacional") {
    res <- readr::read_rds(system.file(glue::glue("alianzas/{estado}/{eleccion}.rda"),
                                       package = "aelectoral2",
                                       mustWork = TRUE)) %>% tibble::as_tibble()

    if(entidad != "nacional"){
      nombre <- aelectoral2::diccionario %>% filter(abreviatura == !!entidad) %>% pull(id_estado) %>%
        stringr::str_pad(width = 2, pad = "0")
      res <- res %>% filter(estado == !!nombre)
    }} else{
      res <- readr::read_rds(system.file(glue::glue("alianzas/{estado}/{eleccion}.rda"),
                                         package = "aelectoral2",
                                         mustWork = TRUE)) %>% tibble::as_tibble()
    }

  res <- res %>% select(-any_of(c("eleccion", "nombre_estado", "candidatura_comun")))

  nivel_sep <- stringr::str_split(names(res)[2], pattern = "_") %>% pluck(1,1)

  w <- switch(nivel_sep, municipio = 3, distritof = 2, distritol = 2, estado = 2)

  alianzas <- res %>% transmute(
    !!rlang::sym(names(res)[2]) := paste(stringr::str_pad(estado, width = 2, pad = "0"),
                                 stringr::str_pad(!!rlang::sym(names(res)[2]), width = w, pad = "0"),
                                 sep = "_"),
    coalicion = coaliciones
  )

  if(!nivel %in% names(alianzas)) {

    alianzas <- alianzas %>% left_join(bd_e %>% distinct(!!rlang::sym(names(alianzas)[1]), !!rlang::sym(nivel)))
  }


  return(alianzas)
}
#' Base de datos que resume agrupando por las llaves
#'Basada en la función summarise
#'
#' @param bd Base de datos que se quiere reducir
#' @param completa base de datos electoral
#' @param llaves Son las claves cartográficas de los niveles. Por default la unidad mínima es sección y está acompañada de estado.
#'
#' @return Data frame
#' @examples add %>% reducir(self$bd, self$llaves)
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

#' Lee un shapefile
#' @param unidad si es de municipio, estado, distrito, seccion, etc.
#' @param entidad el estado de donde es
#'
#' @return shp
#' @examples leer_shp(unidad, entidad)
leer_shp <- function(unidad, entidad){
  if(entidad == "nacional") id <- aelectoral2::diccionario %>% pull(id_estado) %>% stringr::str_pad(width = 2, pad = "0") else{
    id <- aelectoral2::diccionario %>% filter(abreviatura %in% !!entidad) %>% pull(id_estado) %>% stringr::str_pad(width = 2, pad = "0")
  }

  res <- id %>% purrr::map(~{
    readr::read_rds(system.file(glue::glue("shp/{unidad}/{.x}.rda"),
                                package = "aelectoral2",
                                mustWork = TRUE)) %>% sf::st_transform(sf::st_crs(4326))
  }) %>% bind_rows()
}

#' Para juntar un shapefile con otra base de datos
#' Funcion basada en left_join
#' @param shp Base de datos de tipo shp
#' @param bd Base de datos que se va a unir con el shapefile
#'
#' @return Un shp unido con bd
#' @examples join_shp_bd(secc_21, df_21)
join_shp_bd <- function(shp, bd){
  shp %>% left_join(bd)
}
