devtools::load_all()
# devtools::install()
# library(aelectoral2)
library(tidyr)
bd <- Electoral$new("df_21", entidad = "mex", extranjero = T)

bd$agregar_bd("df_18", entidad = "mex",llaves = c("seccion", "distritof", "distritol"))
bd$agregar_bd("pr_18", entidad = "mex")

bd$agregar_variables(eleccion = "df_21", variables = c("estado","nombre_estado"))

g <- bd$bd %>% ganador("distritof_21", "df_21")
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

aux <- bd$bd %>% group_by(across(all_of(nivel))) %>%
  summarise(across(starts_with("ele_"), ~sum(.x,na.rm = T))) %>%
  filter(!is.na(!!rlang::sym(nivel))) %>%
  select(-contains("total"),-contains("nominal")) %>%
  select(distritof_21, contains(eleccion))

ranking <- aux %>%
  pivot_longer(-distritof_21) %>%
  group_by(distritof_21) %>% mutate(ranking = min_rank(-value)) %>% ungroup

library(stringr)
ranking %>% mutate(num_partidos = str_count(name, "_") - 2,
                   partidos = str_split(gsub(pattern = "ele_|_df_21","",name), "_")
                   ) %>%
  mutate()


g %>% pivot_longer(c(-distritof_21, -ganador_df_21))


bd$todas$pr_18 %>% filter(seccion == "9999")
