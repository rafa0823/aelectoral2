############################################### AUXILIARES CHIAPAS ------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

######### PADRON BENEFICIARIOS CHIAPAS

beneficiarios <- read_csv("../revocacion/data_raw/consMpo7.csv") %>%
  janitor::clean_names() %>%
  select("id_estado" = clave_entidad,
         "id_municipio" = clave_municipio,
         "nombre_municipio" = nombe_municipio,
         beneficiarios_unicos)

beneficiarios %>% write_rds("inst/bdos_auxiliares_chis/beneficiarios.rda")


### REFERENTES MOVILIZACION CHIAPAS

informe_referentes <- read_excel("../revocacion/data_raw/informe_v2.xlsx") %>%
  janitor::clean_names()

referente_mpos <- informe_referentes %>%
  mutate(referente = paste(referente,apellido_referente),
         operador = paste(nombre_operador,apellido_pat_operador,apellido_mat_operador, sep = " "),
         referente = if_else(is.na(referente),"Sin referente", referente)) %>%
  rename("nombre_municipio_22" = "nom_mun_casilla") %>%
  mutate(referente = paste(referente,apellido_referente,sep = " "),
         operador = paste(nombre_operador,apellido_pat_operador,apellido_mat_operador, sep = " "),
         lider = paste(nombre_lider,apellido_pat_lider,apellido_mat_lider, sep = " ")) %>%
  select(!c(apellido_referente,apellido_pat_operador,apellido_mat_operador,nombre_operador))


referente_mpos %>% write_rds("inst/bdos_auxiliares_chis/referente_mpos.rda")


# ALIANZAS

alianzas_chis_pm_21 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_21_chiapas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "07",
         nombre_estado = "CHIAPAS",
         municipio_21 = formatC(municipio_21, width = 3, flag = "0"),
         candidatura_comun = NA,
         tipo_eleccion = "ORDINARIA",
         año = "2021")

# extraordinarias chiapas 22

alianzas_chis_pmextra_22 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_extra_22_chiapas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(eleccion = "pm_21",
         estado = "07",
         nombre_estado = "CHIAPAS",
         municipio_22 = formatC(municipio_22, width = 3, flag = "0"),
         candidatura_comun = NA,
         tipo_eleccion = "EXTRAORDINARIA",
         año = "2022") %>%
  rename(municipio_21 = municipio_22)

# nuntar alianzas pm 21 y pm extra 22

alianzas_chis_pm_21 %>% rbind(alianzas_chis_pmextra_22) %>% write_rds("inst/alianzas/chis/pm_21.rda")



alianzas_chis_pm_15 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_15_chiapas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "07",
         nombre_estado = "DURANGO",
         municipio_15 = formatC(municipio_15, width = 3, flag = "0"),
         candidatura_comun = NA)

alianzas_chis_pm_15 %>% write_rds("inst/alianzas/chis/pm_15.rda")

alianzas_chis_dl_21 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/dl_21_chiapas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "07",
         nombre_estado = "CHIAPAS",
         distritol_21 = formatC(distritol_21, width = 2, flag = "0"))

alianzas_chis_dl_21 %>% write_rds("inst/alianzas/chis/dl_21.rda")

alianzas_chis_gb_18 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_18_chiapas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "07",
         nombre_estado = "CHIAPAS")

alianzas_chis_gb_18 %>% write_rds("inst/alianzas/chis/gb_18.rda")

alianzas_chis_gb_18 %>% count(coaliciones)

alianzas_chis_pm_18 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_18_chiapas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "07",
         municipio_21 = formatC(municipio_21, width = 3, flag = "0"),
         nombre_estado = "CHIAPAS") %>%
  rename(municipio_18 = municipio_21,
         coaliciones = coaliaciones)

alianzas_chis_pm_18 %>% write_rds("inst/alianzas/chis/pm_18.rda")

alianzas_chis_pm_18 %>% count(coaliciones)

