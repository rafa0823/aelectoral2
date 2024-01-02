## code to prepare `claves` dataset goes here
library(readr)
library(dplyr)

path <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTyxbdhZzFPh0ZH35fdh6YIt-rIPk9CPYlQpHBNIBR0doVkBRyyhbbRP1c2yCh31g/pub?output=csv"
claves <- readr::read_csv(path) |>
  mutate(entidad = sprintf("%02s", entidad),
         across(where(is.numeric), ~paste(entidad, sprintf("%02s", .x), sep = "_")),
         nombre_distritol_22 = paste(stringr::str_sub(distritol_22, 4,5), nombre_distritol_22),
         nombre_distritof_22 = paste(stringr::str_sub(distritof_22, 4,5), nombre_distritof_22))

usethis::use_data(claves, overwrite = T)

# Municipios -----------------------------------------------------------------
claves_mun <- tibble(municipio_22 = NA, nombre_municipio_22 = NA)
claves_mun <- diccionario$abreviatura |>
  purrr::map(~{
    shp <- ElectoralSHP$new(unidad = "mun_22", entidad =  .x)
    mun <- shp$shp[[1]] |>
      as_tibble() |>
      select(contains("municipio"))

    return(mun)
  })

claves_mun <- claves_mun |>
  reduce(bind_rows)

usethis::use_data(claves_mun)
# Juntar municipios y distritos -------------------------------------------

claves <- bind_cols(claves, claves_mun)












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


# Morelos -----------------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/MOR/MOR_PEL_2021/AYUNTAMIENTOS_csv/2021_SEE_AYUN_MOR_SEC.csv"
clave_mor <- read_csv(path) |>
  janitor::clean_names() |>
  distinct(seccion,
           nombre_distritol_22 = cabecera_distrital_local,
           distritol_22 = id_distrito_local,
           nombre_municipio_22 = municipio,
           municipio_22 = id_municipio) |>
  mutate(seccion = sprintf("17_%04s", seccion),
         distritol_22 = sprintf("17_%02s", distritol_22),
         nombre_distritol_22 = paste(gsub("17_", "", distritol_22), nombre_distritol_22),
         municipio_22 = sprintf("17_%03s", municipio_22)) |>
  left_join(df)

#   bind_rows(clave_cdmx) |>
#   bind_rows(clave_chis) |>
#   bind_rows(clave_mex)

# Puebla ------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/PUE/AYUNTAMIENTOS_csv/2021_SEE_AYUN_PUE_CAS.csv"

clave_pue <- read_csv(path) |>
  janitor::clean_names() |>
  distinct(seccion,
           nombre_distritol_22 = cabecera_distrital_local,
           distritol_22 = id_distrito_local,
           nombre_municipio_22 = municipio,
           municipio_22 = id_municipio) |>
  mutate(seccion = sprintf("21_%04s", seccion),
         distritol_22 = sprintf("21_%02s", distritol_22),
         nombre_distritol_22 = paste(gsub("21_", "", distritol_22), nombre_distritol_22),
         municipio_22 = sprintf("21_%03s", municipio_22)) |>
  left_join(df)

# TABASCO -----------------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/TAB/TAB_PEL_2021/AYUNTAMIENTOS_csv/2021_SEE_AYUN_TAB_CAS.csv"

clave_tab <- read_csv(path) |>
  janitor::clean_names() |>
  distinct(seccion,
           nombre_distritol_22 = cabecera_distrital_local,
           distritol_22 = id_distrito_local,
           nombre_municipio_22 = municipio,
           municipio_22 = id_municipio) |>
  mutate(seccion = sprintf("27_%04s", seccion),
         distritol_22 = sprintf("27_%02s", distritol_22),
         nombre_distritol_22 = paste(gsub("27_", "", distritol_22), nombre_distritol_22),
         municipio_22 = sprintf("27_%03s", municipio_22)) |>
  left_join(df)


claves <- claves |>
  bind_rows(clave_tab)

# Corrección nombres dl ---------------------------------------------------

tabasco_correg <- tibble(
  entidad = "27",
  distritol_22 = sprintf("%02s", c(1:21)),
  nombre_distritol_22 = c("HUIMANGUILLO", "CARDENAS", "CARDENAS", "CENTLA", rep("CENTRO", 6),
                          "COMACALCO", "COMACALCO", "CUNDUACAN", "MACUSPANA", "HUIMANGUILLO",
                          "MACUSPANA", "JALAPA DE MENDEZ", "NACAJUCA", "PARAISO", "TEAPA",
                          "TENOSIQUE"),
  distritof_22 = c(""),
  nombre_distritof_22 = c("")
)

claves |>
  left_join(tabasco_correg, join_by(distritol_22)) |>
  mutate(nombre_distritol_22 = if_else(!is.na(temp), temp, nombre_distritol_22)) |>
  select(-temp) |>
  filter(distritol_22 == "27_18")


# Yucatán -----------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/YUC/YUC_PEL_2021/AYUNTAMIENTOS_csv/2021_SEE_AYUN_YUC_CAS.csv"

clave_yuc <- read_csv(path) |>
  janitor::clean_names() |>
  distinct(seccion,
           nombre_distritol_22 = cabecera_distrital_local,
           distritol_22 = id_distrito_local,
           nombre_municipio_22 = municipio,
           municipio_22 = id_municipio) |>
  mutate(seccion = sprintf("31_%04s", seccion),
         distritol_22 = sprintf("31_%02s", distritol_22),
         nombre_distritol_22 = paste(gsub("31_", "", distritol_22), nombre_distritol_22),
         municipio_22 = sprintf("31_%03s", municipio_22)) |>
  left_join(df)


claves <- claves |>
  bind_rows(clave_yuc)

usethis::use_data(claves, overwrite = TRUE)
