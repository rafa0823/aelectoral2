## code to prepare `claves` dataset goes here
library(readr)
library(dplyr)
df <- read_rds("inst/electoral/nacional/df_21.rda") |>
  as_tibble() |>
  transmute(seccion = paste(estado, seccion, sep = "_"),
            nombre_distritof = paste(distritof, nombre_distritof),
            distritof = paste(estado, sprintf("%02s", distritof), sep = "_"))


# clave Jalisco -----------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/JALISCO/JAL_PEL_2021/AYUNTAMIENTOS_csv/jalisco_normal_casilla.csv"
clave_jal <- read_csv(path) |>
  janitor::clean_names() |>
  distinct(seccion,
           nombre_distritol = cabecera_distrital_local,
           distritol = id_distrito_local,
           nombre_municipio = municipio,
           municipio = id_municipio) |>
  mutate(seccion = sprintf("14_%04s", seccion),
         distritol = sprintf("14_%02s", distritol),
         nombre_distritol = paste(gsub("14_", "", distritol), nombre_distritol),
         municipio = sprintf("14_%03s", municipio)) |>
  left_join(df)

claves <- clave_jal

usethis::use_data(claves, overwrite = TRUE)
