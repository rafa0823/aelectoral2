# EDOMEX AUXILIARES

#ALIANZAS ----------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

#pm21

alianzas_edomex_pm_21 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_21_edomex.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "15",
         nombre_estado = "MÉXICO",
         municipio_21 = formatC(municipio_21, width = 3, flag = "0"))

alianzas_edomex_pm_21 %>% write_rds("inst/alianzas/mex/mex_pm_19.rda")

#pm18

alianzas_edomex_pm_18 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_18_edomex.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "15",
         nombre_estado = "MÉXICO",
         municipio_18 = formatC(municipio_18, width = 3, flag = "0"))

alianzas_edomex_pm_18 %>% write_rds("inst/alianzas/mex/mex_pm_18.rda")

#dl21

alianzas_edomex_dl_21 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/dl_21_edomex.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "15",
         nombre_estado = "MÉXICO",
         distritol_21 = formatC(distritol_21, width = 2, flag = "0"),
         eleccion = "dl_21")

alianzas_edomex_dl_21 %>% write_rds("inst/alianzas/mex/mex_dl_21.rda")

#dl18

alianzas_edomex_dl_18 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/dl_18_edomex.csv",
                               n_max = 30) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "15",
         nombre_estado = "MÉXICO",
         distritol_18 = formatC(distritol_18, width = 2, flag = "0"),
         eleccion = "dl_18")

alianzas_edomex_dl_18 %>% write_rds("inst/alianzas/mex/mex_dl_18.rda")

#gb 17

alianzas_edomex_gb_17 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_17_edomex.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "15",
         nombre_estado = "MÉXICO")

alianzas_edomex_gb_17 %>% write_rds("inst/alianzas/mex/mex_gb_17.rda")
