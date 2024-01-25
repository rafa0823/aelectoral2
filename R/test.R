#' Te trae el resultado de votos de una variable seleccionada con un patron de una bd electoral.
#'
#' @param bd Base de datos a testear
#' @param tipo si es por partido o por candidato
#' @param patron la variable de la que se quiere el resultado
#'
#' @return El resultado electoral con el método señalado

t_nac <- function(bd, tipo, patron){
  tipo <- match.arg(tipo, c("candidato","partido"))

  aux <- bd %>%
    summarise(across(starts_with("ele_"), ~sum(.x,na.rm = T))) %>%
    pivot_longer(everything()) %>%
    mutate(partidos = str_count(name,"_")-2,
           dividido = value/partidos
    )
  if(tipo == "partido"){
    res <- aux %>%
      filter(grepl(patron,name)) %>%
      summarise(sum(dividido))
  }
  if(tipo == "candidato"){
    res <- aux %>%
      filter(grepl(patron, name)) %>%
      summarise(sum(value))
  }
  return(res)
}

#' Devuelve el resultado electoral por partido o por candidato de una variable seleccionada con un patron de un estado en particular en un nivel seleccionado
#'
#' @param bd Base de datos a testear
#' @param tipo si es por partido o por candidato
#' @param patron la variable de la que se quiere el resultado
#' @param estado Número de la entidad del ine
#' @param nivel Nivel en el que se determinan las alianzas dependiendo de la unidad en la que se realiza la elección.
#' @param seleccion que selección de acuerdo con el nivel se requiere
#'
#' @return  Resultado electoral por nivel
t_nivel <- function(bd, tipo, patron, estado, nivel, seleccion){
  tipo <- match.arg(tipo, c("candidato","partido"))



  aux <- bd %>%
    filter(estado == !!estado) %>%
    group_by(!!rlang::sym(nivel)) %>%
    summarise(across(starts_with("ele_"), ~sum(.x,na.rm = T))) %>%
    filter(!!rlang::sym(nivel) == seleccion) %>% select(-rlang::sym(nivel)) %>%
    pivot_longer(everything()) %>%
    mutate(partidos = str_count(name,"_")-2,
           dividido = value/partidos
    )

  if(tipo == "partido"){
    res <- aux %>%
      filter(grepl(patron,name)) %>%
      summarise(sum(dividido))
  }
  if(tipo == "candidato"){
    res <- aux %>%
      filter(grepl(patron, name)) %>%
      summarise(!!rlang::sym(seleccion) :=sum(value))
  }
  return(res)
}
