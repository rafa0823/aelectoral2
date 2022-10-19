############################################### AUXILIARES HIDALGO ------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# ALIANZAS ---------------------------

#PM 21

alianzas_hgo_pm_20 <- readxl::read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_20_hidalgo.xlsx") %>%
  janitor::clean_names() %>%
  rename(municipio = municipio_20) %>%
  as_tibble() %>%
  mutate(estado = "13",
         nombre_estado = "HIDALGO",
         municipio_16 = formatC(municipio, width = 2, flag = "0"),
         eleccion = "pm_20")

alianzas_hgo_pm_16 %>% count(coaliciones)

alianzas_hgo_pm_16 %>% write_rds("inst/alianzas/hgo/hgo_pm_20.rda")


# GB 16

alianzas_hgo_gb_16 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_16_hidalgo.csv") %>%
  janitor::clean_names() %>%
  rename(municipio = municipio_16) %>%
  as_tibble() %>%
  mutate(estado = "13",
         nombre_estado = "HIDALGO",
         municipio_16 = formatC(municipio, width = 2, flag = "0"),
         eleccion = "gb_16")

alianzas_hgo_gb_16 %>% count(coaliciones)

alianzas_hgo_gb_16 %>% write_rds("inst/alianzas/hgo/hgo_gb_16.rda")

#PM 16

alianzas_hgo_pm_16 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_16_hidalgo.csv") %>%
  janitor::clean_names() %>%
  rename(municipio = municipio_16) %>%
  as_tibble() %>%
  mutate(estado = "13",
         nombre_estado = "HIDALGO",
         municipio_16 = formatC(municipio, width = 2, flag = "0"),
         eleccion = "pm_16")

alianzas_hgo_pm_16 %>% count(coaliciones)

alianzas_hgo_pm_16 %>% write_rds("inst/alianzas/hgo/hgo_pm_16.rda")

