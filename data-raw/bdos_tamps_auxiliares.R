############################################### AUXILIARES TAMAULIPAS ------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# ALIANZAS ---------------------------

# PM 21

alianzas_tamps_pm_21 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_21_tamaulipas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  rename(municipio = municipio_21) %>%
  select(eleccion, estado, nombre_estado, municipio, coaliciones, candidatura_comun) %>%
  mutate(estado = "28",
         nombre_estado = "TAMAULIPAS",
         municipio = formatC(municipio, width = 3, flag = "0"),
         eleccion = "pm_21")

alianzas_tamps_pm_21 %>% count(coaliciones)

alianzas_tamps_pm_21 %>% write_rds("inst/alianzas/tamps/tamps_pm_21.rda")

# PM 18

alianzas_tamps_pm_18 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_18_tamaulipas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  rename(municipio = municipio_18) %>%
  select(eleccion, estado, nombre_estado, municipio, coaliciones, candidatura_comun) %>%
  mutate(estado = "28",
         nombre_estado = "TAMAULIPAS",
         municipio = formatC(municipio, width = 3, flag = "0"),
         eleccion = "pm_18")

alianzas_tamps_pm_18 %>% count(coaliciones)

alianzas_tamps_pm_18 %>% write_rds("inst/alianzas/tamps/tamps_pm_18.rda")

# GB 16

alianzas_tamps_gb_16 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_16_tamaulipas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  rename(municipio = municipio_16) %>%
  mutate(estado = "28",
         nombre_estado = "TAMAULIPAS",
         municipio = formatC(municipio, width = 3, flag = "0"),
         eleccion = "gb_16")

alianzas_tamps_gb_16 %>% count(coaliciones)

alianzas_tamps_gb_16 %>% write_rds("inst/alianzas/tamps/tamps_gb_16.rda")

# PM 16

alianzas_tamps_pm_16 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_16_tamaulipas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  rename(municipio = municipio_16) %>%
  select(eleccion, estado, nombre_estado, municipio, coaliciones, candidatura_comun) %>%
  mutate(estado = "28",
         nombre_estado = "TAMAULIPAS",
         municipio = formatC(municipio, width = 3, flag = "0"),
         eleccion = "pm_16")

alianzas_tamps_pm_16 %>% count(coaliciones)

alianzas_tamps_pm_16 %>% write_rds("inst/alianzas/tamps/tamps_pm_16.rda")

