# EDOMEX AUXILIARES

#ALIANZAS ----------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

#pm21

alianzas_edomex_pm_21 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_21_edomex.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "15",
         nombre_estado = "MÉXICO",
         municipio_21 = formatC(municipio_21, width = 3, flag = "0"),
         tipo_eleccion = "ORDINARIA",
         año = "2021")

#pmext21

alianzas_edomex_pmext_21 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_extra_21_edomex.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(eleccion = "pm_21",
         estado = "15",
         nombre_estado = "MÉXICO",
         municipio_21 = formatC(municipio_21, width = 3, flag = "0"),
         candidatura_comun = NA,
         tipo_eleccion = "EXTRAORDINARIA",
         año = "2021")%>%
  rename(coaliciones = coaliaciones)


alianzas_edomex_pm_21 %>% rbind(alianzas_edomex_pmext_21) %>% write_rds("inst/alianzas/mex/pm_21.rda")

#pm18

alianzas_edomex_pm_18 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_18_edomex.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "15",
         nombre_estado = "MÉXICO",
         municipio_18 = formatC(municipio_18, width = 3, flag = "0"))

alianzas_edomex_pm_18 %>% write_rds("inst/alianzas/mex/pm_18.rda")

#dl21

alianzas_edomex_dl_21 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/dl_21_edomex.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "15",
         nombre_estado = "MÉXICO",
         distritol_21 = formatC(distritol_21, width = 2, flag = "0"),
         eleccion = "dl_21")

alianzas_edomex_dl_21 %>% write_rds("inst/alianzas/mex/dl_21.rda")

#dl18

alianzas_edomex_dl_18 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/dl_18_edomex.csv",
                               n_max = 30) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "15",
         nombre_estado = "MÉXICO",
         distritol_18 = formatC(distritol_18, width = 2, flag = "0"),
         eleccion = "dl_18")

alianzas_edomex_dl_18 %>% write_rds("inst/alianzas/mex/dl_18.rda")

#gb 17

alianzas_edomex_gb_17 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_17_edomex.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "15",
         nombre_estado = "MÉXICO")

alianzas_edomex_gb_17 %>% write_rds("inst/alianzas/mex/gb_17.rda")

# pm 15

#
# candidatos_pm_15 <- read_excel("~/Downloads/Computo_MUNICIPAL_2015.xlsx",
#                                skip = 6,n_max = 125) %>%
#   janitor::clean_names()
#
# #### sacar alianzas del documento del ieem de resultados por candidato
#
# alianza_pan_pt <- candidatos_pm_15  %>%
#   mutate(candidatura_comun = if_else(is.na(pan_7), TRUE,FALSE),
#          coaliciones = if_else(is.na(pan_pt_22),"NA","pan_pt")) %>%
#   select(municipio,coaliciones, candidatura_comun) %>%
#   filter(coaliciones != "NA")
#
# alianza_pri_pvem_panal <- candidatos_pm_15 %>%
#   mutate(candidatura_comun = if_else(is.na(pri_8), TRUE,FALSE),
#          coaliciones = if_else(is.na(pri_pvem_na_18),"NA","pri_pvem_panal")) %>%
#   select(municipio,coaliciones, candidatura_comun) %>%
#   filter(coaliciones != "NA")
#
# alianza_pan_pt %>% full_join(alianza_pri_pvem_panal) %>% write_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_15_edomex.csv")


alianzas_pm_15 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_15_edomex.csv")

alianzas_pm_15 <- alianzas_pm_15 %>%
  rename(municipio_15 = municipio) %>%
  mutate(eleccion = "pm_15",
         estado = "15",
         municipio_15 = formatC(municipio_15,width = 3,flag = "0"))

alianzas_pm_15 %>% count(coaliciones)

alianzas_pm_15 %>% write_rds("inst/alianzas/mex/pm_15.rda")


# dl 15

alianzas_edomex_dl_15 <- c(1,2,4,5,6,seq(9,125))%>%
  as_tibble() %>%
  rename(distritol_15 = value) %>%
  mutate(estado = "15",
         nombre_estado = "MÉXICO",
         coaliciones = "pri_pvem",
         candidatura_comun = FALSE)


alianzas_edomex_dl_15 %>% write_rds("inst/alianzas/mex/dl_15.rda")

