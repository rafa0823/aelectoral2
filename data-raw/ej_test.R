library(readr)
library(tidyr)
load("data/nac_pr_18.rda")
pr18 <- nac_pr_18 %>% as_tibble %>% mutate(across(starts_with("ele_"), ~as.numeric(.x)))


# Nacional ----------------------------------------------------------------

#candidato
t_nac(pr18, "candidato", "morena_|pt_|pes_")
#partido
t_nac(pr18, "partido", "total_")
t_nac(pr18, "partido", "morena_")
t_nac(pr18, "partido", "pt_")
t_nac(pr18, "partido", "pes_")

# Niveles -----------------------------------------------------------------

#estado
#candidato
t_nivel(pr18, "candidato", "morena_|pt_|pes_", estado = 1, nivel = "nombre_estado",seleccion = "AGUASCALIENTES")
t_nivel(pr18, "candidato", "morena_|pt_|pes_", estado = 1, nivel = "distritof",seleccion = "1")
#partido
t_nivel(pr18, "partido", "morena_", estado = 1, nivel = "nombre_estado",seleccion = "AGUASCALIENTES")

#distritof
#partido
t_nivel(pr18, "candidato", "morena_|pt_|pes_", estado = 1, nivel = "distritof", seleccion = "1")

#casilla
cas <- pr18 %>% sample_n(1) %>% pull(clave_casilla)
t_nivel(pr18, "candidato", "ele_morena_pr_18", estado = 15, nivel = "clave_casilla", seleccion = cas)


# edomex pm21 -------------------------------------------------------------

pr18 %>% group_by(distritof) %>%
  summarise(across(starts_with("ele_"), ~sum(.x,na.rm = T))) %>%
  select(distritof, matches("morena_|pt_|pes_")) %>%
  pivot_longer(-distritof) %>% mutate(partidos = str_count(name,"_")-2) %>%
  mutate(cociente = value %/% partidos,
         residuo = value %% partidos)


load("data/mex_pm_21.rda")
edomex <-mex_pm_21 %>%  as_tibble()
t_nivel(edomex, "candidato", "pt|morena|panal", estado = 15,
        nivel = "nombre_municipio",  seleccion = "CHALCO")

t_nivel(edomex, "candidato", "pt|morena|panal", estado = 15,
        nivel = "nombre_municipio",  seleccion = "LERMA")
t_nivel(edomex, "candidato", "pt|morena|panal", estado = 15,
        nivel = "nombre_municipio",  seleccion = "CALIMAYA")

t_nivel(edomex, "candidato", "pan|pri|prd", estado = 15,
        nivel = "nombre_municipio",  seleccion = "CALIMAYA")
11749

t_nivel(edomex, "candidato", "pan|pri|prd", estado = 15,
        nivel = "nombre_municipio",  seleccion = "LERMA")
11828+30288+492

t_nivel(edomex, "candidato", "pan|pri|prd", estado = 15,
        nivel = "nombre_municipio",  seleccion = "ACULCO")
9219

t_nivel(edomex, "partido", "pan_", estado = 15,
        nivel = "nombre_municipio",  seleccion = "TEXCOCO")
5084

t_nivel(edomex, "candidato", "pt", estado = 15,
        nivel = "nombre_municipio",  seleccion = "TEXCOCO")

t_nivel(edomex, "candidato", "fxm", estado = 15,
        nivel = "nombre_municipio",  seleccion = "NICOLAS ROMERO")

t_nivel(edomex, "candidato", "mc", estado = 15,
        nivel = "seccion",  seleccion = "0510")


edomex %>%  filter(nombre_municipio == "TEXCOCO") %>%
  summarise(across(c(ele_pan_pm_21, ele_prd_pm_21, ele_pri_pm_21, ele_panal_pm_21),
                   ~sum(.x,na.rm = T )))
# PR 18 -------------------------------------------------------------------

load("data/nac_df_21.rda")

df21<- nac_df_21 %>% as_tibble()


t_nac(df21, "candidato", "pan_|pri_|prd_")
t_nac(df21, "candidato", "rsp")

#partido
t_nac(pr18, "partido", "total_")

t_nivel(df21, "candidato", "rsp", estado = 26,
        nivel = "nombre_estado",seleccion = "SONORA")

t_nivel(df21, "candidato", "total_", estado = 11,
        nivel = "nombre_estado",seleccion = "GUANAJUATO")

t_nivel(df21, "candidato", "total_", estado = 11, nivel = "distritof",seleccion = "8")

t_nivel(df21, "candidato", "total_", estado = 11, nivel = "seccion",seleccion = "0200")

t_nivel(df21, "candidato", "total_", estado = 18,
        nivel = "clave_casilla",seleccion = "180827C0100")



# df18 --------------------------------------------------------------------


load("data/nac_df_18.rda")

t_nac(df18, "candidato", "total_")
t_nac(df18, "candidato", "pan_|prd_|mc_")

#partido
t_nivel(df18, "candidato", "total_", estado = 22,
        nivel = "nombre_estado",seleccion = "QUERÉTARO")

t_nivel(df18, "candidato", "total_", estado = 22, nivel = "seccion",seleccion = "0715")

t_nivel(df18, "candidato", "total_", estado = 22, nivel = "clave_casilla",seleccion = "220134B0100")


# df15 --------------------------------------------------------------------


load("data/nac_df_15.rda")
df15<- nac_df_15 %>%  as_tibble()

t_nac(df15, "candidato", "total_")
t_nac(df15, "candidato", "morena_")

#partido
t_nivel(df15, "candidato", "pan_", estado = 20,
        nivel = "estado",seleccion = "20")

t_nivel(df15, "candidato", "pri_", estado = 20, nivel = "seccion",seleccion = "0187")

t_nivel(df15, "candidato", "prd_", estado = 20, nivel = "clave_casilla",seleccion = "200090C0200")



# pm18 --------------------------------------------------------------------
load("data/mex_dl_18.rda")
dl18 <- mex_dl_18 %>%  as_tibble()

#partido
t_nivel(dl18, "candidato", "pri_", estado = 15,
        nivel = "nombre_municipio_dl_18",  seleccion = "COACALCO")

t_nivel(edomex, "candidato", "pt|morena|panal", estado = 15,
        nivel = "nombre_municipio",  seleccion = "LERMA")
t_nivel(edomex, "candidato", "pt|morena|panal", estado = 15,
        nivel = "nombre_municipio",  seleccion = "CALIMAYA")

t_nivel(edomex, "candidato", "pan|pri|prd", estado = 15,
        nivel = "nombre_municipio",  seleccion = "CALIMAYA")
11749

t_nivel(edomex, "candidato", "pan|pri|prd", estado = 15,
        nivel = "nombre_municipio",  seleccion = "LERMA")
11828+30288+492

t_nivel(edomex, "candidato", "pan|pri|prd", estado = 15,
        nivel = "nombre_municipio",  seleccion = "ACULCO")
9219

t_nivel(edomex, "partido", "pan_", estado = 15,
        nivel = "nombre_municipio",  seleccion = "TEXCOCO")
5084

t_nivel(edomex, "candidato", "pt", estado = 15,
        nivel = "nombre_municipio",  seleccion = "TEXCOCO")

t_nivel(edomex, "candidato", "fxm", estado = 15,
        nivel = "nombre_municipio",  seleccion = "NICOLAS ROMERO")

t_nivel(dl18, "candidato", "mc_", estado = 15,
        nivel = "seccion",  seleccion = "4223")

t_nivel(dl18, "candidato", "morena_", estado = 15,
        nivel = "clave_casilla",  seleccion = "156278B0100")

# df15 --------------------------------------------------------------------


load("data/")
df15<- nac_df_15 %>%  as_tibble()

nex<- readRDS("inst/electoral/mex_pmext_21.rda") %>%  as_tibble()

t_nac(nex, "candidato", "total_")
t_nac(nex, "candidato", "morena_")

t_nivel(nex, "partido", "morena_", estado = 15,
        nivel = "nombre_municipio",  seleccion = "NEXTLALPAN")

t_nivel(nex, "candidato", "fxm_", estado = 15,
        nivel = "seccion",  seleccion = "3040")




