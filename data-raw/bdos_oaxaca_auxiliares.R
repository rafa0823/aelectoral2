############################################### AUXILIARES OAXACA ------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)


# ALIANZAS  ---------------------------------------------------------------------------------

oaxaca_gb_16 <- read.csv('~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_16_oaxaca.csv') %>%
  janitor::clean_names() %>% mutate(eleccion='gb_16',
                                    estado = "20",
                                    nombre_estado='OAXACA',
                                    municipio_16 = formatC(municipio_16, width = 2, flag='0'))
oaxaca_gb_16 %>% count(coaliciones)

oaxaca_gb_16 %>% write_rds("inst/alianzas/oaxaca/oaxaca_gb_16.rda")
