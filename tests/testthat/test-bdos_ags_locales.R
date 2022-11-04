library(tidyverse)
devtools::load_all()

# Test de construcción de las bases de datos de aguascalientes
# Se comparan las bases construidas con los cómputos del INEE

# Base sin procesar -------------------------------------------------------

setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2021/Municipio/Aguascalientes")

bd_INEE <- list.files(full.names = T) %>%
  map_dfr(~{
    print(.x)
    read_csv(.x,skip = 2) %>%
      janitor::clean_names() %>%
      mutate(municipio = .x)
  }) %>%
  mutate(municipio = gsub(pattern = "[[:alpha:]]|[[:punct:]]",
                          replacement = "",
                          municipio)) %>%
  filter(!casillas %in% c("TOTAL",NA))

# Base procesada ----------------------------------------------------------

bd_procesada <- aelectoral2::Electoral$new(eleccion = "pm_21", entidad = "ags", llaves = c("municipio", "nombre_municipio"))

# Comparativa, mismo numero de votos y casillas ---------------------------

test_that("Comparativa de base descargada vs base procesada", {
  tot_pro <- bd_procesada$bd %>% filter(municipio == "01_001") %>% summarise(sum(ele_pan_pm_21, na.rm = T)) %>% pull()
  tot_INEE <- bd_INEE %>% filter(municipio == 1) %>% summarise(sum(pan, na.rm = T)) %>% pull()
  expect_equal(tot_pro, tot_INEE)
})
