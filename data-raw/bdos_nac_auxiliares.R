# NACIONAL AUXILIARES

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

## ALIANZAS -------------------------------------------------------------------------


## DF_21

alianzas_df_21 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/federales/df_21.csv") %>%
  rename(distritof_21 = distrito) %>%
  mutate(distritof_21 = formatC(distritof_21, width = 2, flag = "0"),
         eleccion = "df_21")

alianzas_df_21 %>% write_rds("inst/alianzas/alianzas_edomex/alianzas_df_21.rda")

## DF 18

alianzas_df_18 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/federales/df_18.csv") %>%
  mutate(estado = formatC(estado, width = 2, flag = "0"),
         distritof_18 = formatC(distritof_18, width = 2, flag = "0"),
         eleccion = "df_18")

alianzas_df_18 %>% write_rds("inst/alianzas/alianzas_edomex/alianzas_df_18.rda")

## DF 15

alianzas_df_15 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/federales/df_15.csv") %>%
  mutate(estado = formatC(estado, width = 2, flag = "0"),
         distritof_15 = formatC(distritof_15, width = 2, flag = "0"),
         eleccion = "df_15")

alianzas_df_15 %>% write_rds("inst/alianzas/alianzas_edomex/alianzas_df_15.rda")

