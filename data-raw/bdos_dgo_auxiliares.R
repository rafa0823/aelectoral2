# ALIANZAS

# CARGAR BASES ----------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

alianzas_dgo_pm_19 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_19_durango.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "10",
         nombre_estado = "DURANGO",
         municipio_19 = formatC(municipio_19, width = 2, flag = "0"))

alianzas_dgo_pm_19 %>% write_rds("inst/alianzas/alianzas_durango/alianzas_dgo_pm_19.rda")

alianzas_dgo_pm_16 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_16_durango.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "10",
         nombre_estado = "DURANGO",
         municipio_16 = formatC(municipio_16, width = 2, flag = "0"))

alianzas_dgo_pm_16 %>% write_rds("inst/alianzas/alianzas_durango/alianzas_dgo_pm_16.rda")

alianzas_dgo_gb_16 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_16_durango.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "10",
         nombre_estado = "DURANGO",
         municipio_16 = formatC(municipio_16, width = 2, flag = "0"),
         eleccion = "gb_16")

alianzas_dgo_gb_16 %>% write_rds("inst/alianzas/alianzas_durango/alianzas_dgo_gb_16.rda")





