## code to prepare `bdos_mor_locales` dataset goes here
library(tidyverse)
library(readxl)
library(hablar)

# Funciones ---------------------------------------------------------------
homologar_bd <- function(bd, estado, nombre_estado){
  bd |>
    mutate(estado = !!estado,
           nombre_estado = nombre_estado,
           casilla = case_when(casilla == "B"~ "B01",
                               grepl("MEC", casilla) ~ gsub("MEC", "M", casilla),
                               grepl("VA", casilla) ~ gsub("VA", "V", casilla),
                               grepl("VPPP", casilla) ~ gsub("VPPP", "P", casilla),
                               T ~casilla),
           id_casilla = case_when(nchar(casilla) >= 4 ~ stringr::str_extract_all(casilla,"(?<=E)[^C]*?(\\d+)(?=C)"),
                                  T ~ stringr::str_extract_all(casilla,"(?<=[a-zA-Z])(\\d+)")),
           tipo_casilla = substr(casilla, 1, 1),
           ext_contigua = if_else(nchar(casilla) >= 4, stringr::str_extract_all(casilla,"(?<=C)(\\d+)"), list("0")),
           clave_casilla = glue::glue("{estado}{stringr::str_pad(seccion,pad = '0', width = 4)}{tipo_casilla}{stringr::str_pad(id_casilla,pad = '0', width = 2)}{stringr::str_pad(ext_contigua,pad = '0', width = 2)}")
    ) |>
    tidyr::unnest(cols = c(casilla:ext_contigua))
}

# gb_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/MOR/MOR_PEL_2018/GUBERNATURA_csv/2018_SEE_GOB_MOR_CAS.csv"
aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(panal = na,
         ph = humanista,
         noreg = can_nreg,
         total = total_votos,
         nominal = lista_nominal,
         distritol_18 = id_distrito_local,
         nombre_distritol_18 = cabecera_distrital_local,
         nombre_municipio_18 = municipio,
         municipio_18 = id_municipio) |>
  rename_with(~paste0("ele_", .x, "_gb_18"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  homologar_bd(estado = "17", "MORELOS") |>
  mutate(distritol_18 = sprintf("%02s", distritol_18),
         municipio_18 = sprintf("%03s", municipio_18),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_18:clave_casilla, -c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion)

glimpse(aux)
aux
write_rds(aux, file = "inst/electoral/mor/gb_18.rda")
# pm_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/MOR/MOR_PEL_2018/AYUNTAMIENTOS_csv/2018_SEE_AYUN_MOR_CAS.csv"
aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(panal = na,
         ph = humanista,
         noreg = can_nreg,
         total = total_votos,
         nominal = lista_nominal,
         distritol_18 = id_distrito_local,
         nombre_distritol_18 = cabecera_distrital_local,
         nombre_municipio_18 = municipio,
         municipio_18 = id_municipio) |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0)),
         prd_psd = prd_psd_25 + prd_psd_32,
         pt_morena = pt_morena_27 + pt_morena_35,
         pt_es = pt_es_28 + pt_es_36,
         morena_es = morena_es_29 + morena_es_37,
         pt_morena_es = c_comun_pt_morena_es + pt_morena_es
  ) |>
  select(-c(prd_psd_25, prd_psd_32, pt_morena_27, pt_morena_35, pt_es_28, pt_es_36, morena_es_29, morena_es_37, c_comun_pt_morena_es)) |>
  relocate(c(prd_psd:morena_es, .after = pvem_psd)) |>
  rename_with(~paste0("ele_", .x, "_pm_18"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  rename_with(~gsub("c_comun_", "", .x), contains("c_comun_")) |>
  homologar_bd(estado = "17", "MORELOS") |>
  mutate(distritol_18 = sprintf("%02s", distritol_18),
         municipio_18 = sprintf("%03s", municipio_18),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_18:clave_casilla)

glimpse(aux)
aux |>
  count(nchar(clave_casilla))
write_rds(aux, file = "inst/electoral/mor/pm_18.rda")
# dl_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/MOR/MOR_PEL_2018/DIPUTACIONES_LOC_MR_csv/2018_SEE_DIP_LOC_MR_MOR_CAS.csv"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(panal = na,
         ph = humanista,
         noreg = can_nreg,
         total = total_votos,
         nominal = lista_nominal,
         distritol_18 = id_distrito_local,
         nombre_distritol_18 = cabecera_distrital_local,
         nombre_municipio_18 = municipio,
         municipio_18 = id_municipio) |>
  mutate(across(where(is.numeric), ~replace_na(.x, 0)),
         prd_psd = prd_psd_25 + prd_psd_32,
         pt_morena = pt_morena_27 + pt_morena_35,
         pt_es = pt_es_28 + pt_es_36,
         morena_es = morena_es_29 + morena_es_37,
         pt_morena_es = c_comun_pt_morena_es + pt_morena_es
  ) |>
  select(-c(prd_psd_25, prd_psd_32, pt_morena_27, pt_morena_35, pt_es_28, pt_es_36, morena_es_29, morena_es_37, c_comun_pt_morena_es)) |>
  relocate(c(prd_psd:morena_es, .after = pvem_psd)) |>
  rename_with(~paste0("ele_", .x, "_dl_18"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  rename_with(~gsub("c_comun_", "", .x), contains("c_comun_")) |>
  homologar_bd(estado = "17", "MORELOS") |>
  mutate(distritol_18 = sprintf("%02s", distritol_18),
         municipio_18 = sprintf("%03s", municipio_18),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_18:clave_casilla)

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

write_rds(aux, file = "inst/electoral/mor/dl_18.rda")

# pm_21 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/MOR/MOR_PEL_2021/AYUNTAMIENTOS_csv/2021_SEE_AYUN_MOR_CAS.csv"

estado <- "mor"
nombre_estado <- "MORELOS"
id_estado <- "17"

eleccion <- "pm_21"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(
    # panal = na,
    # pes = es,
    humanista = phm,
    pesol = pes,
    noreg = can_nreg,
    total = total_votos,
    nominal = lista_nominal,
    distritol_21 = id_distrito_local,
    nombre_distritol_21 = cabecera_distrital_local,
    nombre_municipio_21 = municipio,
    municipio_21 = id_municipio) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_pesm_", "_pes_", .x), contains("_pesm_")) |>
  rename_with(~gsub("_panalm_", "_panal_", .x), contains("_panalm_")) |>
  # rename_with(~gsub("_na_", "_panal_", .x), contains("_na_")) |>
  homologar_bd(estado = id_estado, nombre_estado = nombre_estado) |>
  mutate(distritol_21 = sprintf("%02s", distritol_21),
         municipio_21 = sprintf("%03s", municipio_21),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_21:clave_casilla, -c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion)

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

# DL - 21 -----------------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/MOR/MOR_PEL_2021/DIPUTACIONES_LOC_MR_csv/2021_SEE_DIP_LOC_MR_MOR_CAS.csv"

estado <- "mor"
nombre_estado <- "MORELOS"
id_estado <- "17"

eleccion <- "dl_21"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(
    # panal = na,
    # pes = es,
    humanista = phm,
    pesol = pes,
    noreg = can_nreg,
    total = total_votos,
    nominal = lista_nominal,
    distritol_21 = id_distrito_local,
    nombre_distritol_21 = cabecera_distrital_local,
    nombre_municipio_21 = municipio,
    municipio_21 = id_municipio) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_pesm_", "_pes_", .x), contains("_pesm_")) |>
  rename_with(~gsub("_panalm_", "_panal_", .x), contains("_panalm_")) |>
  # rename_with(~gsub("_na_", "_panal_", .x), contains("_na_")) |>
  homologar_bd(estado = id_estado, nombre_estado = nombre_estado) |>
  mutate(distritol_21 = sprintf("%02s", distritol_21),
         municipio_21 = sprintf("%03s", municipio_21),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_21:clave_casilla, -c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion)

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))
