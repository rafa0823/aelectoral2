## code to prepare `resultados` dataset goes here
library(tidyverse)
path1 <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/PEL"
archivos <- "AYUNTAMIENTOS|LOC_MR|GOB|ALCALDIAS"
ja <- list()

carpeta <- list.dirs(path1, full.names = T, recursive = T)
csv <- list.files(carpeta, full.names = T, pattern = "PP.csv")
csv <- csv[grepl(archivos, csv)]
resultados <- NULL

for (i in csv) {
  var <- case_when(grepl("AYUN", i) ~ "pm",
                   grepl("DIPUTACIONES", i) ~ "dl",
                   grepl("GUBERNATURA|JEFATURA", i) ~ "gb")

  ano <- str_sub(sub(".+/(\\d+)_SEE.+", "\\1", i), 3, 4)

  df <- read_csv(i) |>
    janitor::clean_names() |>
    mutate(eleccion = paste(var, ano, sep = "_"),
           across(contains("id"), as.character)) |>
    group_by(nombre_estado, eleccion) |>
    summarise_if(is.numeric, \(x)sum(x, na.rm = T))

  resultados <- bind_rows(resultados, df)
}

bd_test <- resultados |>
  distinct(nombre_estado, eleccion, .keep_all = T) |>
  mutate(nombre_estado = if_else(nombre_estado == "MEXICO", "ESTADO DE MEXICO", nombre_estado)) |>
  left_join(mutate(diccionario, estado = stringi::stri_trans_general(estado, id = "latin-ascii")), join_by(nombre_estado == estado)) |>
  select(id_estado, nombre_estado, abreviatura, eleccion, pan:morena, votos = total_votos, nominal = lista_nominal) |>
  ungroup()

usethis::use_data(bd_test, overwrite = T)
