## code to prepare `censo` dataset goes here
library(tidyverse)
censo <- read_csv("../telectoral.edomex/data-raw/INE_SECCION_2020.csv") |>
  janitor::clean_names()

censo <- censo |>
  mutate(across(entidad:distrito, ~str_pad(.x, width = 2, pad = "0")),
         municipio = str_pad(municipio, width = 3, pad = "0"),
         seccion = str_pad(seccion, width = 4, pad = "0"),
         distritof = paste(entidad, distrito, sep = "_"),
         municipio = paste(entidad, municipio, sep = "_"),
         seccion = paste(entidad, seccion, sep = "_")) |>
  select(-id, - distrito)

readr::write_rds(censo, "inst/censo/seccion_2020.rda")


# censo distritof y distritol 22 ------------------------------------------

shp <- ElectoralSHP$new(unidad = "secc_22", entidad = diccionario$abreviatura[[1]])
walk(diccionario$abreviatura[-1],~shp$agregar_shp(unidad = "secc_22", entidad = .x))

relacion <- shp$shp |>
  bind_rows() |>
  as_tibble() |>
  select(seccion, distritof_22, distritol_22 )

censo_df22 <- censo |>
  left_join(relacion) |>
  filter(!is.na(distritof_22)) |>
  summarise(across(where(is.numeric), ~sum(.x, na.rm = T)),
                                       .by = c(entidad, distritof_22))

censo_dl22 <- censo |>
  left_join(relacion) |>
  filter(!is.na(distritol_22)) |>
  summarise(across(where(is.numeric), ~sum(.x, na.rm = T)),
            .by = c(entidad, distritol_22))

readr::write_rds(censo_dl22, "inst/censo/distritol_22_2020.rda")


# censo por municipio -----------------------------------------------------

csv <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INEGI/Censo 2020/Datos geografico/ITER_NALCSV20.csv"
censo_mun <- read_csv(csv) |>
  janitor::clean_names()

# IMPORTANTE!!!!! hacer relación de municipio inegi y municipio ine

censo_mun <- censo_mun |>
  mutate(municipio_22 = paste(entidad, mun, sep = "_")) |>
  filter(nom_loc == "Total del Municipio") |>
  select(entidad, municipio_22, pobtot:vph_sintic)

readr::write_rds(censo_mun, "inst/censo/municipio_22_2020.rda")
