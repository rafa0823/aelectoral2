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
