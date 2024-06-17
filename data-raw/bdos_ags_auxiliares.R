############################################### AUXILIARES AGUASCALIENTES ------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)


# ALIANZAS  ---------------------------------------------------------------------------------

ags_gb_16 <- read.csv('~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_16_aguascalientes.csv') %>%
  janitor::clean_names() %>% mutate(eleccion='gb_16',
                                    estado = "01",
                                    nombre_estado='AGUASCALIENTES',
                                    municipio_16 = formatC(municipio_16, width = 2, flag='0')) %>%
  select(!c(x,x_1,x_2,x_3))
ags_gb_16 %>% count(coaliciones)

ags_gb_16 %>% write_rds("inst/alianzas/ags/ags_gb_16.rda")

