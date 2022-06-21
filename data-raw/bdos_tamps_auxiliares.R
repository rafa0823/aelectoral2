############################################### AUXILIARES TAMAULIPAS ------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# ALIANZAS ---------------------------

# GB 16

alianzas_tamps_gb_16 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_16_tamaulipas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "28",
         nombre_estado = "TAMAULIPAS",
         municipio_16 = formatC(municipio_16, width = 3, flag = "0"),
         eleccion = "gb_16")

alianzas_tamps_gb_16 %>% count(coaliciones)

alianzas_tamps_gb_16 %>% write_rds("inst/alianzas/tamps/tamps_gb_16.rda")
