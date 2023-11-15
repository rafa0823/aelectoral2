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
