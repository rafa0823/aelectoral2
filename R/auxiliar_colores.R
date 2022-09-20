
#' Title
#'
#' @param bd base con resultados electorales y una columna adicional con el ganador de cada sección
#' @param eleccion elección elegida para analizar
#' @param colores_nombrados vector compuesto con los nombres de partidos y colores que le corresponden
#' @param grupo nivel de observación de la gráfica (secciones, municipios, distritos)
#' @param saturacion nivel de saturación de la paleta de color
#'
#' @return
#' @export
#'
#' @examples
colorear_ganador_degradado <- function(bd,eleccion, colores_nombrados, grupo, saturacion=.9){
  # Partidos
  partidos <- names(colores_nombrados)

  # Homologar colores
  colores_saturados <- colores_nombrados %>%  shades::saturation(saturacion)
  names(colores_saturados) <- names(colores_nombrados)

  # Calcular ganador y máximo de votación
  bd <- bd %>%
    select({{grupo}},matches(glue::glue("ele_{partidos}_{eleccion}"))) %>% na.omit() %>%
    rowwise()
  bd <- bd %>%
    mutate(ganador=list(which.max(c_across(cols = starts_with("ele_")))) %>%
             map_chr(~names(bd)[[.x+1]]) ,
           max_votacion=max(c_across(cols = starts_with("ele_")))
    ) %>%
    ungroup() %>%
    mutate(ganador= stringr::str_remove(ganador, "ele_") %>%
             stringr::str_remove(., pattern = glue::glue("_{eleccion}")))
  # Funciones de color
  funciones_color <- map(unique(bd$ganador),
                         ~colorRamp(colors = c("white",colores_saturados[[.x]]), space = "Lab") %>%
                           leaflet::colorNumeric(domain = c(0, max(bd$max_votacion))))
  names(funciones_color) <- unique(bd$ganador)

  res <- bd %>% mutate(color_ganador=map2_chr(ganador, max_votacion,~funciones_color[[.x]](.y)))
  return(res)
}

#' Title
#'
#'
#' @param bd_larga base electoral en long
#' @param nombre partido de interés
#' @param variable unidad de interés que se desea analizar (votos, pocentaje, etc)
#' @param colores_nombrados vector compuesto con los nombres de partidos y colores que le corresponden
#' @param valor_maximo valor máximo que toma la generación de degradados
#'
#' @return
#' @export
#' @examples
degradar_color_partido <- function(bd_larga, nombre, variable,    colores_nombrados,    valor_maximo=1){
  partidos <- names(colores_nombrados)
  funciones_color <- map(partidos,
                         ~colorRamp(colors = c("white",colores_nombrados[[.x]]), space = "Lab") %>%
                           leaflet::colorNumeric(domain = c(0, valor_maximo)))
  names(funciones_color) <- unique(partidos)
  res <- bd_larga %>%
    mutate(color=map2_chr(!!enquo(nombre), !!enquo(variable),~funciones_color[[.x]](.y)))
  return(res)   }
