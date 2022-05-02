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

bd <- Electoral$new("df_21", entidad = "mex",
                    llaves = c("seccion", "distritof", "distritol", "municipio"),
                    extranjero = T, especial = "repartir")

c("df_18", "df_15","pr_18", "cp_22") %>% purrr::walk(~{
  bd$agregar_bd(.x, entidad = "mex")
})


# Agregar pm_21 con extraordinaria ----------------------------------------
bd$agregar_bd("pm_21", entidad = "mex",extraordinaria = c(eleccion = "pmext_21", entidad = "mex"))
# Agregar bds auxiliares ---------------------------------------------


cat <- cat_utm_22 %>% filter(estado == 15) %>%
  distinct(estado, seccion, unidad_territorial, sede) %>%
  mutate(seccion = paste(estado, seccion, sep = "_")) %>% select(-estado)

bd$agregar_manual(cat, by = "seccion")

# Agregar regiones --------------------------------------------------------

reg <- regiones %>% select(region, municipio)

# reg %>% anti_join(bd$bd, by = c("municipio" = "nombre_municipio_pm_21"))
bd$agregar_manual(reg, by = c("nombre_municipio_pm_21" = "municipio"))

# Agregar presidentes municipales -----------------------------------------

presidentes <- presidentes_mpos_mex %>% select(1:3) %>%
  mutate(nombre_municipio = stringr::str_replace(nombre_municipio,"CASTANEDA", "CASTAÑEDA"))

bd$agregar_manual(presidentes, by = c("nombre_municipio_pm_21" = "nombre_municipio"))
nrow(bd$bd)


# Repartir coalicion partido ----------------------------------------------

bd$partido(nivel = "distritof_21",eleccion = "df_21")
bd$partido(nivel = "distritof_18",eleccion = "df_18")

bd$bd_partido$df_21
bd$bd_partido$df_18



# Coalición candidato -----------------------------------------------------

bd$candidato(al_df_21,nivel = "distritof_21", "df_21")

bd$bd %>% ganador(nivel = "distritof_21", "df_21") %>% select(contains("ganador"))



# tests: sandbox ----------------------------------------------------------


mex$bd %>%  t_nivel("candidato", "ele_morena_dl_21", estado = 15,
                    nivel = "estado",  seleccion = "15")

mex$bd %>%  t_nivel("candidato", "ele_morena_dl_21", estado = 15,
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

mex$bd %>% mutate(estado = 15) %>%     t_nivel("candidato", "ele_total_dl_21", estado = 15,
                      nivel = "estado",  seleccion = "15")

mex$bd %>%   t_nivel("candidato", "ele_prd_dl_21", estado = 15,
                    nivel = "distritol_21",  seleccion = "31")

mex$bd %>%  t_nivel("candidato", "ele_pri_dl_21", estado = 15,
                    nivel = "seccion",  seleccion = "0300")


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


# totales -----------------------------------------------------------------


mex$bd %>%mutate(estado = 15) %>%
  t_nivel("candidato", "ele_total_dl_21", estado = 15,
          nivel = "estado",  seleccion = "15")
6689225

mex$bd %>%mutate(estado = 15) %>%
  t_nivel("candidato", "ele_total_pm_21", estado = 15,
          nivel = "estado",  seleccion = "15")
6687908

mex$bd %>%mutate(estado = 15) %>%
  t_nivel("candidato", "ele_total_df_21", estado = 15,
          nivel = "estado",  seleccion = "15")
6720518

mex$bd %>%mutate(estado = 15) %>%
  t_nivel("candidato", "ele_total_df_18", estado = 15,
          nivel = "estado",  seleccion = "15")
8005606

mex$bd %>%mutate(estado = 15) %>%
  t_nivel("candidato", "ele_total_df_15", estado = 15,
          nivel = "estado",  seleccion = "15")
5569402

mex$bd %>%mutate(estado = 15) %>%
  t_nivel("candidato", "ele_total_pr_18", estado = 15,
          nivel = "estado",  seleccion = "15")
8040160

mex$bd %>%mutate(estado = 15) %>%
  t_nivel("candidato", "ele_total_pm_18", estado = 15,
          nivel = "estado",  seleccion = "15")
7979574

mex$bd %>%mutate(estado = 15) %>%
  t_nivel("candidato", "ele_total_gb_17", estado = 15,
          nivel = "estado",  seleccion = "15")
6080096


mex$bd %>%mutate(estado = 15) %>%
  t_nivel("candidato", "ele_total_dl_18", estado = 15,
          nivel = "estado",  seleccion = "15")
7998650



# coaliciones -------------------------------------------------------------

df21 <- readr::read_rds("inst/electoral/nac_df_21.rda") %>% as_tibble %>%
  mutate(distritof = paste(estado,distritof_21)) %>%  filter(estado == 15, distritof=="15 07")
df21 %>% filter(num_acta_impreso!="2ERP") %>% summarise(sum(ele_pan_df_21))
sum(df21$ele_prd_df_21)

readr::read_rds("inst/electoral/mex_dl_21.rda") %>%  as_tibble()

aux <- mex$bd %>% repartir_coalicion(nivel = "distritof_21", eleccion = "df_21")


t_nac(aux, "partido", "morena_")
16759789-16759917

t_nac(aux, "partido", "pvem_")
2671032-2670997

t_nac(aux, "partido", "pes_")
1352544

t_nac(aux, "partido", "rsp_")
868515


t_nac(aux, "partido", "pan_")
8969273-8969288

#partido
aux %>%  select(ele_pan_df_21, distritof_21) %>%  filter(distritof_21 == "07")
47585-47290

aux %>%  select(ele_morena_df_21, distritof_21) %>%  filter(distritof_21 == "08")
54810-54606
pr18 %>% repartir_coalicion(nivel = "seccion", eleccion = "pr_18")

mex$bd %>%  t_nivel("candidato", "pan_df_21", estado = 15, nivel = "distritof_21",seleccion = "07")


dl21 <-readr::read_rds("inst/electoral/mex_dl_21.rda") %>%  as_tibble()
aux<- mex$bd %>%  repartir_coalicion(nivel = "distritol_21", eleccion = "dl_21")




