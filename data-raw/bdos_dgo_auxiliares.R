# ALIANZAS

# CARGAR BASES ----------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)


alianzas_dgo_dl_21 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/dl_21_durango.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "10",
         nombre_estado = "DURANGO",
         distritol_21 = formatC(distritol_21, width = 2, flag = "0"),
         eleccion = "dl_21")

alianzas_dgo_dl_21 %>% count(coaliciones)

alianzas_dgo_dl_21 %>% write_rds("inst/alianzas/dgo/dgo_dl_21.rda")

alianzas_dgo_pm_19 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_19_durango.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "10",
         nombre_estado = "DURANGO",
         municipio_19 = formatC(municipio_19, width = 3, flag = "0"))

alianzas_dgo_pm_19 %>% write_rds("inst/alianzas/dgo/dgo_pm_19.rda")

alianzas_dgo_pm_16 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_16_durango.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "10",
         nombre_estado = "DURANGO",
         municipio_16 = formatC(municipio_16, width = 3, flag = "0"),
         coaliciones = gsub("__","_",coaliciones),
         coaliciones = gsub("pvem_pri_pd_panal", "pri_pvem_pd_panal",coaliciones))

alianzas_dgo_pm_16 %>% write_rds("inst/alianzas/dgo/dgo_pm_16.rda")

alianzas_dgo_gb_16 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_16_durango.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "10",
         nombre_estado = "DURANGO",
         municipio_16 = formatC(municipio_16, width = 3, flag = "0"),
         eleccion = "gb_16")

alianzas_dgo_gb_16 %>% write_rds("inst/alianzas/dgo/dgo_gb_16.rda")

alianzas_dgo_dl_18 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/dl_18_durango.csv",
                                 n_max = 30) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "10",
         nombre_estado = "DURANGO",
         distritol_18 = formatC(distritol_18, width = 2, flag = "0"),
         eleccion = "dl_18")

alianzas_dgo_dl_18 %>% write_rds("inst/alianzas/dgo/dgo_dl_18.rda")

alianzas_dgo_dl_16 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/dl_16_durango.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "10",
         nombre_estado = "DURANGO",
         distritol_16 = formatC(distritol_16, width = 2, flag = "0"),
         eleccion = "dl_16")

alianzas_dgo_dl_16 %>% write_rds("inst/alianzas/dgo/dgo_dl_16.rda")







