#' Para repartir votos por coalición
#'
#' @param bd Base de datos que se quiere repartir
#' @param nivel Nivel en el que se determinan las alianzas dependiendo de la unidad en la que se realiza la elección.
#' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
#'
#' @return Base de datos repartida
#' @export
repartir_coalicion <- function(bd, nivel, eleccion){
  if(sum(is.na(bd[[nivel]]))>0) bd <- bd %>% mutate(!!rlang::sym(nivel) := tidyr::replace_na(!!rlang::sym(nivel),"E"))

  pre <- bd %>%
    group_by(across(all_of(nivel))) %>%
    summarise(across(starts_with("ele_"), ~sum(.x,na.rm = T))) %>%
    filter(!is.na(!!rlang::sym(nivel))) %>%
    select(all_of(nivel), contains(eleccion)) %>%
    tidyr::pivot_longer(-nivel) %>%
    mutate(
      alianza = gsub(pattern = glue::glue("ele_|_{eleccion}|_cc"),"",name),
      partidos = stringr::str_split(alianza, "_"),
      num_partidos = purrr::map_int(partidos,~length(.x)),
      partido = value %/% num_partidos,
      residuo = value %% num_partidos
    )

  r <- pre %>%
    filter(num_partidos == 1) %>%
    group_by(across(all_of(nivel))) %>%
    mutate(rango = dense_rank(-partido)) %>%
    ungroup()

  total <- pre %>%
    tidyr::unnest(partidos) %>%
    left_join(r %>% select(all_of(nivel), alianza, rango),
              by = c(nivel, "partidos" = "alianza")) %>%
    group_by(across(all_of(nivel)), alianza) %>%
    mutate(partido = partido + (residuo >= dense_rank(rango)) * (residuo > 0)) %>%
    group_by(across(all_of(nivel)), partidos, .drop = T) %>%
    summarise(partido = sum(partido, na.rm = T)) %>%
    tidyr::pivot_wider(names_from = partidos, values_from = partido)

  total <- total %>%
    rename_with(.cols = -all_of(nivel),~glue::glue("ele_{.x}_{eleccion}")) %>%
    ungroup()

  return(total)
}

#' Reparte los votos por candidato tomando en cuenta si los candidatos fueron con alianzas o en candidatura común con más de un partido o no.
#'
#' @param bd base de datos electoral que se quiere repartir por candidatos (tomando en cuenta alianzas)
#' @param al alianzas
#' @param nivel Nivel en el que se determinan las alianzas dependiendo de la unidad en la que se realiza la elección.
#' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
#' @export
#' @return Base de datos repartida por candidato
repartir_candidato <- function(bd, al, nivel, eleccion){
  al <- al %>% na.omit
  partidos_alianza <- al %>% distinct(coalicion) %>% pull(coalicion)

  res <-  partidos_alianza %>%
    purrr::map(~{
      aux_n <- al %>% filter(coalicion == .x) %>% pull(nivel)
      partidos <- stringr::str_split(.x,"_") %>% purrr::pluck(1)
      p_vars <- paste("ele", partidos, eleccion, sep = "_")

      candidato_c <- bd %>%
        filter(!!rlang::sym(nivel) %in% aux_n) %>%
        rowwise() %>%
        transmute(!!rlang::sym(nivel),
                  !!rlang::sym(paste("cand",.x,eleccion, sep = "_")) := sum(c_across(all_of(p_vars)))) %>%
        ungroup

      sin_c <- bd %>%
        filter(!(!!rlang::sym(nivel) %in% aux_n)) %>%
        select(all_of(c(nivel, p_vars)))

      res <- candidato_c %>%
        {
          if(nrow(sin_c)> 0) full_join(., sin_c) else .
        }

    }) %>% purrr::reduce(full_join, by = nivel)


  res <- res %>% left_join(
    bd %>% select(-matches(partidos_alianza %>% stringr::str_split("_") %>% do.call(c,.)))
  ) %>%
    rename_with(~stringr::str_replace(.x, "ele_", "cand_"), starts_with("ele_")) %>%
    mutate(across(c(starts_with("cand_")), ~tidyr::replace_na(.x,0)))# %>%

  return(res)
}

#' Función para señalae quién fue el ganador por eleccion por nivel
#'
#' @param bd Base de datos electoral de la que se quiere sacar el ganador
#' @param nivel Nivel en el que se determinan las alianzas dependiendo de la unidad en la que se realiza la elección.
#' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
#'
#' @return tibble con la columna de ganador con los ganadores por nivel
ganador <- function(bd, nivel, eleccion){
  aux <- bd %>% group_by(across(all_of(nivel))) %>% summarise(across(c(starts_with("ele_"),starts_with("cand_")), ~sum(.x,na.rm = T))) %>%
    filter(!is.na(!!rlang::sym(nivel)))

  g <- aux %>%
    select(-contains("total"),-contains("nominal")) %>%
    select(contains(eleccion)) %>%
    rowwise() %>%
    transmute(!!rlang::sym(glue::glue("ganador_{eleccion}")) := names(.)[which.max(na.omit(c_across(everything())))]) %>%
    ungroup

  aux <- aux %>%
    bind_cols(g)
  return(aux)

}

