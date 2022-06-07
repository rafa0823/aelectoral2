############################################### AUXILIARES QUINTANA ROO ------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# ALIANZAS  ---------------------------------------------------------------------------------

q_roo_gb_16 <- read.csv('~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_16_quintanaroo.csv') %>%
  janitor::clean_names() %>% mutate(eleccion='gb_16',
                                    nombre_estado='QUINTANA ROO',
                                    distrito_16 = formatC(distrito_16, width = 2, flag='0'))
q_roo_gb_16 %>% count(coaliciones)

q_roo_gb_16 %>% write_rds("inst/alianzas/q_roo/q_roo_gb_16.rda")




