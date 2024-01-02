## code to prepare `claves` dataset goes here
library(readr)
library(dplyr)
df <- read_rds("inst/electoral/nacional/df_21.rda") |>
  as_tibble() |>
  transmute(seccion = paste(estado, seccion, sep = "_"),
            nombre_distritof_22 = paste(distritof, nombre_distritof),
            distritof_22 = paste(estado, sprintf("%02s", distritof), sep = "_"))


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

# CDMX --------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/CDMX/2021/AYUNTAMIENTOS_csv/2021_SEE_AYUN_CDMX_CAS.csv"

clave_cdmx <- read_csv(path) |>
  janitor::clean_names() |>
  distinct(seccion,
           nombre_distritol_22 = cabecera_distrital_local,
           distritol_22 = id_distrito_local,
           nombre_municipio_22 = municipio,
           municipio_22 = id_municipio) |>
  mutate(seccion = sprintf("09_%04s", seccion),
         distritol_22 = sprintf("09_%02s", distritol_22),
         nombre_distritol_22 = paste(gsub("09_", "", distritol_22), nombre_distritol_22),
         municipio_22 = sprintf("09_%03s", municipio_22)) |>
  left_join(df)

#clave Chiapas -----------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/CHIS/2021/AYUNTAMIENTOS_csv/2021_SEE_AYUN_CHIS_CAS.csv"
clave_chis <- read_csv(file=path) |>
  janitor::clean_names() |>
  distinct(seccion,
           nombre_distritol_22 = cabecera_distrital_local,
           distritol_22 = id_distrito_local,
           nombre_municipio_22 = municipio,
           municipio_22 = id_municipio) |>
  mutate(seccion = sprintf("07_%04s", seccion),
         distritol_22 = sprintf("07_%02s", distritol_22),
         nombre_distritol_22 = paste(gsub("07_", "", distritol_22), nombre_distritol_22),
         municipio_22 = sprintf("07_%03s", municipio_22)) |>
  left_join(df)

claves <- clave_chis
#clave EDOMEX -----------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/MEX/2021/AYUNTAMIENTOS_csv/2021_SEE_AYUN_MEX_CAS.csv"
clave_mex <- read_csv(path) |>
  janitor::clean_names() |>
  distinct(seccion,
           nombre_distritol_22 = cabecera_distrital_local,
           distritol_22 = id_distrito_local,
           nombre_municipio_22 = municipio,
           municipio_22 = id_municipio) |>
  mutate(seccion = sprintf("15_%04s", seccion),
         distritol_22 = sprintf("15_%02s", distritol_22),
         nombre_distritol_22 = paste(gsub("15_", "", distritol_22), nombre_distritol_22),
         municipio_22 = sprintf("15_%03s", municipio_22)) |>
  left_join(df)

claves <- claves |>
  bind_rows(clave_cdmx) |>
  bind_rows(clave_chis) |>
  bind_rows(clave_mex)

usethis::use_data(claves, overwrite = TRUE)
