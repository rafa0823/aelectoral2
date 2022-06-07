############################################### AUXILIARES HIDALGO ------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# ALIANZAS ---------------------------

# GB 16

alianzas_hgo_gb_16 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_16_hidalgo.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "13",
         nombre_estado = "HIDALGO",
         municipio_16 = formatC(municipio_16, width = 2, flag = "0"),
         eleccion = "gb_16")

alianzas_hgo_gb_16 %>% count(coaliciones)

alianzas_hgo_gb_16 %>% write_rds("inst/alianzas/hgo/hgo_16.rda")
