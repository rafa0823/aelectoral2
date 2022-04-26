repartir_coalicion <- function(bd, nivel, eleccion){
  aux <- bd %>% group_by(across(all_of(nivel))) %>%
    summarise(across(starts_with("ele_"), ~sum(.x,na.rm = T))) %>%
    filter(!is.na(!!rlang::sym(nivel))) %>%
    select(-contains("total"),-contains("nominal")) %>%
    select(all_of(nivel), contains(eleccion))

  division <- aux %>%
    pivot_longer(-nivel) %>% mutate(
      partidos = stringr::str_split(gsub(pattern = glue::glue("ele_|_{eleccion}|_cc"),"",name), "_"),
      num_partidos = purrr::map_int(partidos,~length(.x)),
      partido = value %/% num_partidos,
      residuo = value %% num_partidos
    )

  total <- division %>% split(.[[nivel]]) %>%
    purrr::map(~{

      partidos <- .x %>% filter(num_partidos == 1) %>%
        tidyr::unnest(partidos) %>%
        select(-residuo)
      alianzas <- .x %>% filter(num_partidos > 1)

      for(i in seq_len(nrow(alianzas))){
        al <- alianzas %>% slice(i)
        modif <- partidos %>%
          filter(partidos %in% (al$partidos %>% purrr::pluck(1))) %>%
          mutate(ranking = dense_rank(-partido),
                 partido = partido + al$partido + (ranking <= al$residuo)
          ) %>% select(-ranking)
        partidos <- partidos %>% anti_join(modif, by = c(nivel, "name")) %>% bind_rows(modif)

      }

      return(partidos)
    }) %>% bind_rows()

  total <- total %>% select(all_of(nivel), name, partido) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "partido")

  return(total)
}

repartir_coalicion2 <- function(bd, nivel, eleccion){

  aux <- bd %>% group_by(across(all_of(nivel))) %>%
    summarise(across(starts_with("ele_"), ~sum(.x,na.rm = T))) %>%
    filter(!is.na(!!rlang::sym(nivel))) %>%
    select(-contains("total"),-contains("nominal")) %>%
    select(all_of(nivel), contains(eleccion))

  division <- aux %>%
    pivot_longer(-nivel) %>% mutate(
      partidos = stringr::str_split(gsub(pattern = glue::glue("ele_|_{eleccion}|_cc"),"",name), "_"),
      num_partidos = purrr::map_int(partidos,~length(.x)),
      partido = value %/% num_partidos,
      residuo = value %% num_partidos
    )

  total <- division %>% group_by(across(all_of(nivel))) %>% tidyr::nest() %>%
    mutate(d2 = purrr::map(data, ~{
      partidos <- .x %>% filter(num_partidos == 1) %>%
        tidyr::unnest(partidos) %>%
        select(-residuo)
      alianzas <- .x %>% filter(num_partidos > 1)



      for(i in seq_len(nrow(alianzas))){
        al <- alianzas %>% slice(i)
        modif <- partidos %>%
          filter(partidos %in% (al$partidos %>% purrr::pluck(1))) %>%
          mutate(ranking = dense_rank(-partido),
                 partido = partido + al$partido + (ranking <= al$residuo)
          ) %>% select(-ranking)
        partidos <- partidos %>% anti_join(modif, by = c("name")) %>% bind_rows(modif)

      }

      return(partidos)
    })
    )

  total <- total %>% select(-data) %>% tidyr::unnest(d2) %>% select(all_of(nivel), name, partido) %>%
    tidyr::pivot_wider(names_from = "name", values_from = "partido")

  return(total)
}

ganador <- function(bd, nivel, eleccion){
  aux <- bd %>% group_by(across(all_of(nivel))) %>% summarise(across(starts_with("ele_"), ~sum(.x,na.rm = T))) %>%
    filter(!is.na(!!rlang::sym(nivel)))

  g <- aux %>%
    select(-contains("total"),-contains("nominal")) %>%
    select(contains(eleccion)) %>%
    rowwise() %>%
    transmute(!!rlang::sym(glue::glue("ganador_{eleccion}")) := names(.)[which.max(na.omit(c_across(everything())))]) %>%
    ungroup

  aux <- aux %>%
    # select(contains(nivel)) %>%
    bind_cols(g)
  return(aux)

}

