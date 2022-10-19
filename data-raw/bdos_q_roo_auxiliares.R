############################################### AUXILIARES QUINTANA ROO ------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# ALIANZAS  ---------------------------------------------------------------------------------


# gb 16

q_roo_pm_21 <- read.csv('~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_21_quintanaroo.csv') %>%
  janitor::clean_names() %>%
  rename(municipio = municipio_21) %>%
  mutate(eleccion='pm_21',
         nombre_estado='QUINTANA ROO',
         municipio = formatC(municipio, width = 2, flag='0'))

q_roo_pm_21 %>% count(coaliciones)

q_roo_pm_21%>% write_rds("inst/alianzas/q_roo/q_roo_pm_21.rda")

# pm 18

q_roo_pm_18 <- read.csv('~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_18_quintanaroo.csv') %>%
  janitor::clean_names() %>%
  rename(municipio = municipio_18) %>%
  mutate(eleccion='pm_18',
         nombre_estado='QUINTANA ROO',
         municipio = formatC(municipio, width = 2, flag='0'))

q_roo_pm_18 %>% count(coaliciones)

q_roo_pm_18%>% write_rds("inst/alianzas/q_roo/q_roo_pm_18.rda")

# pm 16

q_roo_pm_16 <- read.csv('~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_16_quintanaroo.csv') %>%
  janitor::clean_names() %>%
  rename(municipio = municipio_16) %>%
  mutate(eleccion='pm_16',
         nombre_estado='QUINTANA ROO',
         municipio = formatC(municipio, width = 2, flag='0'))

q_roo_pm_16 %>% count(coaliciones)

q_roo_pm_16 %>% write_rds("inst/alianzas/q_roo/q_roo_pm_16.rda")



# gb 16

q_roo_gb_16 <- read.csv('~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_16_quintanaroo.csv') %>%
  janitor::clean_names() %>%
  rename(distritol = distrito_16) %>%
  mutate(eleccion='gb_16',
         nombre_estado='QUINTANA ROO',
         distritol = formatC(distritol, width = 2, flag='0'))
q_roo_gb_16 %>% count(coaliciones)

q_roo_gb_16 %>% write_rds("inst/alianzas/q_roo/q_roo_gb_16.rda")




