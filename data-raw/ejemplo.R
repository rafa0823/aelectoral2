devtools::load_all()
# devtools::install()
# library(aelectoral2)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

mex <- Electoral$new(eleccion = "df_21", entidad = "mex",extranjero = T,especiales = T)


c("df_18", "df_15", "pr_18", "pm_21", "pm_18", "gb_17", "dl_18") %>%
  walk(~{
    mex$agregar_bd(eleccion = .x, entidad = "mex")
  })



mex$bd %>%  t_nivel("partido", "ele_morena_df_21", estado = 15,
                    nivel = "seccion",  seleccion = "5934")

mex$bd %>%  t_nivel("partido", "ele_mc_df_21", estado = 15,
                    nivel = "seccion",  seleccion = "0392")
