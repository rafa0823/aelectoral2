## code to prepare `chih_bdos` dataset goes here
library(readr)
library(dplyr)
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/computos/Locales/CHIH/"
estado <- "chih"

homologar_bd <- function(bd){
  bd |>
    mutate(casilla = case_when(casilla == "B"~ "B01",
                               grepl("MEC", casilla) ~ gsub("MEC", "M", casilla),
                               grepl("EC", casilla) ~ gsub("EC", "E", casilla),
                               grepl("VA", casilla) ~ gsub("VA|VAP", "V", casilla),
                               grepl("VPP", casilla) ~ gsub("VPPP|VPP", "P", casilla),
                               grepl("SMR", casilla) ~ gsub("SMR", "S", casilla),
                               T ~casilla),
           id_casilla = case_when(nchar(casilla) >= 4 ~ stringr::str_extract_all(casilla,"(?<=E)[^C]*?(\\d+)(?=C)"),
                                  T ~ stringr::str_extract_all(casilla,"(?<=[a-zA-Z])(\\d+)")),
           tipo_casilla = substr(casilla, 1, 1),
           ext_contigua = if_else(nchar(casilla) >= 4, stringr::str_extract_all(casilla,"(?<=C)(\\d+)"), list("0")),
           clave_casilla = glue::glue("{estado}{stringr::str_pad(seccion,pad = '0', width = 4)}{tipo_casilla}{stringr::str_pad(id_casilla,pad = '0', width = 2)}{stringr::str_pad(ext_contigua,pad = '0', width = 2)}")
    ) |>
    tidyr::unnest(cols = c(casilla:ext_contigua))
}


# 2021 --------------------------------------------------------------------
## Ayuntamiento
eleccion <- "pm_21"
aux <- read_csv(paste0(path, "/CHIH_PEL_2021/CHIH_PEL_2021/AYUNTAMIENTOS_csv/2021_SEE_AYUN_CHIH_CAS.csv")) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_|cand_", "", .x), contains(c("num_votos_", "cand_"))) |>
  rename(
    distritol_21 = id_distrito_local,
    nombre_distritol_21 = cabecera_distrital_local,
    municipio_21 = id_municipio,
    nombre_municipio_21 = municipio,
    total = total_votos,
    nominal = lista_nominal,
    noreg = can_nreg,
  ) |>
  mutate(estado = sprintf("%02s", id_estado),
         distritol_21 = sprintf("%03s", distritol_21),
         municipio_21 = sprintf("%03s", municipio_21),
         seccion = sprintf("%04s", seccion)
  ) |>
  homologar_bd()  |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = pan:nominal) |>
  rename_with(~gsub("_nach_", "_panal_", .x), contains("_nach_"))

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

## Distrito local
eleccion <- "dl_21"
aux <- read_csv(paste0(path, "/CHIH_PEL_2021/CHIH_PEL_2021/DIPUTACIONES_LOC_MR_csv/2021_SEE_DIP_LOC_MR_CHIH_CAS.csv")) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_|cand_", "", .x), contains(c("num_votos_", "cand_"))) |>
  rename(
    distritol_21 = id_distrito_local,
    nombre_distritol_21 = cabecera_distrital_local,
    municipio_21 = id_municipio,
    nombre_municipio_21 = municipio,
    total = total_votos,
    nominal = lista_nominal,
    noreg = can_nreg,
  ) |>
  mutate(estado = sprintf("%02s", id_estado),
         distritol_21 = sprintf("%03s", distritol_21),
         municipio_21 = sprintf("%03s", municipio_21),
         seccion = sprintf("%04s", seccion)
  ) |>
  homologar_bd()  |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = pan:nominal) |>
  rename_with(~gsub("_nach_", "_panal_", .x), contains("_nach_"))

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

## Gobernatura
eleccion <- "gb_21"
aux <- read_csv(paste0(path, "/CHIH_PEL_2021/CHIH_PEL_2021/GUBERNATURA_csv/2021_SEE_GOB_CHIH_CAS.csv")) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_|cand_", "", .x), contains(c("num_votos_", "cand_"))) |>
  rename(
    distritol_21 = id_distrito_local,
    nombre_distritol_21 = cabecera_distrital_local,
    municipio_21 = id_municipio,
    nombre_municipio_21 = municipio,
    total = total_votos,
    nominal = lista_nominal,
    noreg = can_nreg,
  ) |>
  mutate(estado = sprintf("%02s", id_estado),
         distritol_21 = sprintf("%03s", distritol_21),
         municipio_21 = sprintf("%03s", municipio_21),
         seccion = sprintf("%04s", seccion)
  ) |>
  homologar_bd()  |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = pan:nominal) |>
  rename_with(~gsub("_nach_", "_panal_", .x), contains("_nach_"))

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))
# 2018 --------------------------------------------------------------------
## Ayuntamiento
eleccion <- "pm_18"
aux <- read_csv(paste0(path, "/CHIH_PEL_2018/AYUNTAMIENTOS_csv/2018_SEE_AYUN_CHIH_CAS.csv")) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_|cand_", "", .x), contains(c("num_votos_", "cand_"))) |>
  rename(
    distritol_18 = id_distrito_local,
    nombre_distritol_18 = cabecera_distrital_local,
    municipio_18 = id_municipio,
    nombre_municipio_18 = municipio,
    total = total_votos,
    nominal = lista_nominal,
    noreg = can_nreg
  ) |>
  mutate(estado = sprintf("%02s", id_estado),
         distritol_18 = sprintf("%03s", distritol_18),
         municipio_18 = sprintf("%03s", municipio_18),
         seccion = sprintf("%04s", seccion)
  ) |>
  homologar_bd()  |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = pan:nominal) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  rename_with(~gsub("_na_", "_panal_", .x), contains("_na_"))

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

## Distrito local
eleccion <- "dl_18"
aux <- read_csv(paste0(path, "/CHIH_PEL_2018/DIPUTACIONES_LOC_MR_csv/2018_SEE_DIP_LOC_MR_CHIH_CAS.csv")) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_|cand_", "", .x), contains(c("num_votos_", "cand_"))) |>
  rename(
    distritol_18 = id_distrito_local,
    nombre_distritol_18 = cabecera_distrital_local,
    municipio_18 = id_municipio,
    nombre_municipio_18 = municipio,
    total = total_votos,
    nominal = lista_nominal,
    noreg = can_nreg
  ) |>
  mutate(estado = sprintf("%02s", id_estado),
         distritol_18 = sprintf("%03s", distritol_18),
         municipio_18 = sprintf("%03s", municipio_18),
         seccion = sprintf("%04s", seccion)
  ) |>
  homologar_bd()  |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = pan:nominal) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  rename_with(~gsub("_na_", "_panal_", .x), contains("_na_"))

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))


# 2016 --------------------------------------------------------------------
## Ayuntamiento
eleccion <- "pm_16"
aux <- read_csv(paste0(path, "/CHIH_PEL_2016/AYUNTAMIENTOS_csv/2016_SEE_AYUN_CHIH_CAS.csv")) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_|cand_", "", .x), contains(c("num_votos_", "cand_"))) |>
  tidyr::pivot_longer(cols = c(contains(c("pan", "pri", "prd", "pt", "pvem", "mc", "nva_alianza", "morena", "ind")), "es"),
                      names_to = "combination", values_to = "votes") %>%
  mutate(combination = gsub("_\\d+$|c_comun_", "", combination)) %>%
  group_by(id_estado, nombre_estado, id_distrito, cabecera_distrital,
           id_municipio, municipio, seccion, casilla, combination, can_nreg, validos, nulos, total_votos, lista_nominal) %>%
  summarize(votes = sum(votes, na.rm = TRUE), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = combination, values_from = votes) |>
  rename(
    distritol_16 = id_distrito,
    nombre_distritol_16 = cabecera_distrital,
    municipio_16 = id_municipio,
    nombre_municipio_16 = municipio,
    total = total_votos,
    nominal = lista_nominal,
    noreg = can_nreg,
  ) |>
  mutate(estado = sprintf("%02s", id_estado),
         distritol_16 = sprintf("%03s", distritol_16),
         municipio_16 = sprintf("%03s", municipio_16),
         seccion = sprintf("%04s", seccion)
  ) |>
  homologar_bd()  |>
  relocate(nominal, .after = pvem_pt_nva_alianza) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = noreg:nominal) |>
  rename_with(~gsub("nva_alianza", "panal", .x), contains("nva_alianza")) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_"))

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

## Distrito local
eleccion <- "dl_16"
aux <- read_csv(paste0(path, "/CHIH_PEL_2016/DIPUTACIONES_LOC_MR_csv/2016_SEE_DIP_LOC_MR_CHIH_CAS.csv")) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_|cand_", "", .x), contains(c("num_votos_", "cand_"))) |>
  tidyr::pivot_longer(cols = c(contains(c("pan", "pri", "prd", "pt", "pvem", "mc", "nva_alianza", "morena", "ind")), "es"),
                      names_to = "combination", values_to = "votes") %>%
  mutate(combination = gsub("_\\d+$|c_comun_", "", combination)) %>%
  group_by(id_estado, nombre_estado, id_distrito, cabecera_distrital,
           id_municipio, municipio, seccion, casilla, combination, can_nreg, validos, nulos, total_votos, lista_nominal) %>%
  summarize(votes = sum(votes, na.rm = TRUE), .groups = 'drop') %>%
  tidyr::pivot_wider(names_from = combination, values_from = votes) |>
  rename(
    distritol_16 = id_distrito,
    nombre_distritol_16 = cabecera_distrital,
    municipio_16 = id_municipio,
    nombre_municipio_16 = municipio,
    total = total_votos,
    nominal = lista_nominal,
    noreg = can_nreg,
  ) |>
  mutate(estado = sprintf("%02s", id_estado),
         distritol_16 = sprintf("%03s", distritol_16),
         municipio_16 = sprintf("%03s", municipio_16),
         seccion = sprintf("%04s", seccion)
  ) |>
  homologar_bd()  |>
  relocate(nominal, .after = pvem) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = noreg:nominal) |>
  rename_with(~gsub("nva_alianza", "panal", .x), contains("nva_alianza")) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_"))

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

## Gobernatura
eleccion <- "gb_16"
aux <- read_csv(paste0(path, "/CHIH_PEL_2016/GUBERNATURA_csv/2016_SEE_GOB_CHIH_CAS.csv")) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_|cand_", "", .x), contains(c("num_votos_", "cand_"))) |>
  rename(
    distritol_16 = id_distrito,
    nombre_distritol_16 = cabecera_distrital,
    municipio_16 = id_municipio,
    nombre_municipio_16 = municipio,
    total = total_votos,
    nominal = lista_nominal,
    noreg = can_nreg,
  ) |>
  mutate(estado = sprintf("%02s", id_estado),
         distritol_16 = sprintf("%03s", distritol_16),
         municipio_16 = sprintf("%03s", municipio_16),
         seccion = sprintf("%04s", seccion)
  ) |>
  homologar_bd()  |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = pan:nominal) |>
  rename_with(~gsub("nva_alianza", "panal", .x), contains("nva_alianza"))

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))


# Locales 2024 ------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/computos/Locales/CHIH/chih_24/ResultadosComputo_2024.xlsx"
# Distrito local
eleccion <- "dl_24"

aux <- readxl::read_excel(path) |>
  janitor::clean_names() |>
  rename(
    distritol_24 = distrito_4,
    nombre_distritol_24 = cabecera_distrito,
    municipio_24 = municipio_3,
    nombre_municipio_24 = municipio_local,
    total = total_votos,
    nominal = ln,
    noreg = cand_no_reg,
    pt_morena = ppt_morena
  ) |>
  mutate(estado = "08",
         distritol_24 = sprintf("%03s", distritol_24),
         municipio_24 = sprintf("%03s", municipio_24),
         seccion = sprintf("%04s", seccion)
  ) |>
  homologar_bd()  |>
  select(-c(eleccion, numero, tipocasilla, numero_ext_contig, distrito_10, municipio_11, municipio_nombre, seccion_2, boletassobrantes, boletas_otras_elecciones)) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = total:nulos)

glimpse(aux)

write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

## Municipio

path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/computos/Locales/CHIH/chih_24/ResultadosComputo_2024.xlsx"

eleccion <- "pm_24"

aux <- readxl::read_excel(path, sheet = 2) |>
  janitor::clean_names() |>
  rename(
    distritol_24 = distrito_4,
    nombre_distritol_24 = cabecera_distrito,
    municipio_24 = municipio_3,
    nombre_municipio_24 = municipio_local,
    total = total_votos,
    nominal = ln,
    noreg = cand_no_reg,
    pt_morena = ppt_morena
  ) |>
  mutate(estado = "08",
         distritol_24 = sprintf("%03s", distritol_24),
         municipio_24 = sprintf("%03s", municipio_24),
         seccion = sprintf("%04s", seccion)
  ) |>
  homologar_bd()  |>
  select(-c(eleccion, numero, tipocasilla, numero_ext_contig, distrito_10, municipio_11, municipio_nombre, seccion_2, boletassobrantes, boletas_otras_elecciones)) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = total:nulos)

glimpse(aux)

write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))
