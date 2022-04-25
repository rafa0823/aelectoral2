devtools::load_all()
# devtools::install()
# library(aelectoral2)

library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

mex <- Electoral$new(eleccion = "df_21", entidad = "mex",extranjero = F,especiales = T)


c("df_18", "df_15", "pr_18", "pm_21", "pm_18", "gb_17", "dl_18") %>%
  walk(~{
    mex$agregar_bd(eleccion = .x, entidad = "mex")
  })

mex$todas$df_18
mex$todas %>% names()
mex$agregar_variables(eleccion = "df_21", variables = c("estado","nombre_estado", "distritof_21"))
mex$agregar_variables(eleccion = "pm_21", variables = c( "distritol_21"))
mex$agregar_variables(eleccion = "gb_17", variables = c( "distritol_17"))
mex$agregar_variables(eleccion = "df_15", variables = c( "distritof_15"))
mex$bd

mex$agregar_variables(eleccion = "df_18", variables = c("estado","nombre_estado", "distritof_18"))
mex$agregar_variables(eleccion = "pm_18", variables = c( "distritol_18"))
mex$agregar_variables(eleccion = "dl_18", variables = c( "distritol_18"))
mex$agregar_variables(eleccion = "pm_21", variables = c( "distritol_21"))
mex$bd %>%  t_nivel("candidato", "ele_morena_df_21", estado = 15,
                    nivel = "seccion",  seleccion = "5934")

mex$bd %>%  t_nivel("candidato", "ele_mc_df_21", estado = 15,
                    nivel = "seccion",  seleccion = "3045")

mex$bd %>%  t_nivel("candidato", "ele_total_df_21", estado = 15, nivel = "distritof_21",
                    "2")



mex$bd %>%  t_nivel("candidato", "ele_pri_pvem_pr_18", estado = 15,
                    nivel = "seccion",  seleccion = "6316")

mex$bd %>%  t_nivel("candidato", "ele_total_pr_18", estado = 15,
                    nivel = "distritof_18",  seleccion = "18")

mex$bd %>% filter(seccion == "0000") %>% select(-distritof_21) %>% distinct() %>%
  bind_rows(
    mex$bd %>% filter(seccion != "0000")
  ) %>%
  t_nivel("candidato", "ele_total_pm_18", estado = 15,
          nivel = "estado",  seleccion = "15")

mex$bd %>%    t_nivel("candidato", "ele_total_pm_18", estado = 15,
                      nivel = "estado",  seleccion = "15")

mex$bd %>%  t_nivel("candidato", "ele_panal_pm_18", estado = 15,
                    nivel = "distritol_18",  seleccion = "21")

mex$bd %>%  t_nivel("candidato", "ele_pt_pm_18", estado = 15,
                    nivel = "seccion",  seleccion = "0546")


mex$bd %>%    t_nivel("candidato", "ele_total_gb_17", estado = 15,
                      nivel = "estado",  seleccion = "15")
6080096
6079799

mex$bd %>%  t_nivel("candidato", "ele_pri_gb_17", estado = 15,
                    nivel = "distritol_17",  seleccion = "23")


mex$bd %>%  t_nivel("candidato", "ele_total_gb_17", estado = 15,
                    nivel = "seccion",  seleccion = "0401")

mex$bd %>%    t_nivel("candidato", "ele_total_dl_18", estado = 15,
                      nivel = "estado",  seleccion = "15")

mex$bd %>%  t_nivel("candidato", "ele_pri_dl_18", estado = 15,
                    nivel = "distritol_18",  seleccion = "8")


mex$bd %>%  t_nivel("candidato", "ele_pvem_dl_18", estado = 15,
                    nivel = "seccion",  seleccion = "0057")
