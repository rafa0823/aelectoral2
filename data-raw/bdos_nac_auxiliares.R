# NACIONAL AUXILIARES

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

## ALIANZAS -------------------------------------------------------------------------


## DF_21

alianzas_df_21 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/federales/df_21.csv") %>%
  rename(distritof_21 = distrito) %>%
  mutate(distritof_21 = formatC(distritof_21, width = 2, flag = "0"),
         eleccion = "df_21")

alianzas_df_21 %>% write_rds("inst/alianzas/nacional/df_21.rda")

## DF 18

alianzas_df_18 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/federales/df_18.csv") %>%
  mutate(estado = formatC(estado, width = 2, flag = "0"),
         distritof_18 = formatC(distritof_18, width = 2, flag = "0"),
         eleccion = "df_18")

alianzas_df_18 %>% write_rds("inst/alianzas/nacional/df_18.rda")

## DF 15

alianzas_df_15 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/federales/df_15.csv") %>%
  mutate(estado = formatC(estado, width = 2, flag = "0"),
         distritof_15 = formatC(distritof_15, width = 2, flag = "0"),
         eleccion = "df_15")

alianzas_df_15 %>% write_rds("inst/alianzas/nacional/df_15.rda")


## DF 18

alianzas_pr_18 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/federales/pr_18.csv") %>%
  mutate(eleccion = "pr_18")

alianzas_pr_18 %>% count(coaliciones)

alianzas_pr_18 %>% write_rds("inst/alianzas/nacional/pr_18.rda")

## DF 12

#### sacar alianzas del documento del ife de resultados por candidato

# alianza_pri_pven <- candidatos_df_12 %>% janitor::clean_names()  %>%
#   mutate(candidatura_comun = if_else(is.na(pri), TRUE,FALSE),
#          coaliciones = if_else(is.na(pri_pvem),"NA","pri_pvem")) %>%
#   select(id_estado,id_distrito,coaliciones, candidatura_comun) %>%
#   filter(coaliciones != "NA")
#
# alianza_prd_pt_mc <- candidatos_df_12 %>% janitor::clean_names()  %>%
#   mutate(candidatura_comun = !is.na(prd_pt_mc),
#          coaliciones = if_else(is.na(prd_pt_mc),"NA","prd_pt_mc")) %>%
#   select(id_estado,id_distrito,coaliciones, candidatura_comun) %>%
#   filter(coaliciones != "NA")
#
# alianza_pri_pven %>% full_join(alianza_prd_pt_mc) %>% write_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/federales/df_12.csv")

# subir alianzas df 12 al paquete

alianzas_df_12 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/federales/df_12.csv")

alianzas_df_12 <- alianzas_df_12 %>%
  rename(estado = id_estado,
         distritof_12 = id_distrito) %>%
  mutate(eleccion = "df_12",
         estado = formatC(estado,width = 2,flag = "0"),
         distritof_12 = formatC(distritof_12,width = 2,flag = "0"))

alianzas_df_12 %>% count(coaliciones)

alianzas_df_12 %>% write_rds("inst/alianzas/nacional/pr_12.rda")

