

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

bd_pm_21_chis <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Chiapas/2021_AYUN_LOC_MR_CHIS_CAS.xlsx",
                            n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()

bd_pm_18_chis <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Chiapas/2021_AYUN_LOC_MR_CHIS_CAS.xlsx",
                            n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()

bd_pm_15_chis <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Chiapas/2021_AYUN_LOC_MR_CHIS_CAS.xlsx",
                            n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()

bd_gb_18_chis <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Chiapas/2021_AYUN_LOC_MR_CHIS_CAS.xlsx",
                            n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()

bd_dl_21_chis <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Chiapas/2021_AYUN_LOC_MR_CHIS_CAS.xlsx",
                          n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()
