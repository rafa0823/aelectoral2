devtools::load_all()
# devtools::install()
# library(aelectoral2)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)


mex <- Electoral$new(eleccion = "df_21", entidad = "mex",extranjero = T,especiales = T)
mex$agregar_bd("df_18", entidad = "mex",llaves = c("seccion", "distritof", "distritol"))
c("df_15", "pr_18", "pm_21", "pm_18", "gb_17", "dl_21","dl_18") %>%
  walk(~{
    mex$agregar_bd(eleccion = .x, entidad = "mex")
  })


mex$bd %>%  t_nivel("candidato", "ele_morena_df_21", estado = 15,
                    nivel = "seccion",  seleccion = "5934")

mex$bd %>%  t_nivel("candidato", "ele_mc_df_21", estado = 15,
                    nivel = "seccion",  seleccion = "3045")

mex$bd %>% mutate(estado = 15) %>%
  t_nivel("candidato", "ele_total_df_21", estado = 15, nivel = "distritof_21",
                    seleccion = "02")



mex$bd %>%  t_nivel("candidato", "ele_pri_pvem_pr_18", estado = 15,
                    nivel = "seccion",  seleccion = "6316")

mex$bd %>%  mutate(estado = 15) %>%  t_nivel("candidato", "ele_total_pr_18", estado = 15,
                    nivel = "distritof_18.x",  seleccion = "18")

mex$bd %>%mutate(estado = 15) %>%
  t_nivel("candidato", "ele_total_df_21", estado = 15,
          nivel = "estado",  seleccion = "15")

mex$bd %>% mutate(estado = 15) %>%     t_nivel("candidato", "ele_total_pm_18", estado = 15,
                      nivel = "estado",  seleccion = "15")

mex$bd %>%  t_nivel("candidato", "ele_panal_pm_18", estado = 15,
                    nivel = "distritol_18.x",  seleccion = "21")

mex$bd %>%  t_nivel("candidato", "ele_pt_pm_18", estado = 15,
                    nivel = "seccion",  seleccion = "0546")


mex$bd %>% mutate(estado = 15) %>%    t_nivel("candidato", "ele_total_gb_17", estado = 15,
                      nivel = "estado",  seleccion = "15")

6080096
6079799

mex$bd %>%  t_nivel("candidato", "ele_pri_gb_17", estado = 15,
                    nivel = "distritol_17",  seleccion = "23")


mex$bd %>%  t_nivel("candidato", "ele_total_gb_17", estado = 15,
                    nivel = "seccion",  seleccion = "0401")

mex$bd %>% mutate(estado = 15) %>%     t_nivel("candidato", "ele_total_dl_18", estado = 15,
                      nivel = "estado",  seleccion = "15")

mex$bd %>%  t_nivel("candidato", "ele_pri_dl_18", estado = 15,
                    nivel = "distritol_18.x",  seleccion = "08")


mex$bd %>%  t_nivel("candidato", "ele_pvem_dl_18", estado = 15,
                    nivel = "seccion",  seleccion = "0057")



bd$agregar_bd("df_18", entidad = "mex",llaves = c("seccion", "distritof", "distritol"))
bd$agregar_bd("pr_18", entidad = "mex")

