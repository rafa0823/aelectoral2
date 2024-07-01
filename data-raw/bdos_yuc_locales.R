## code to prepare `bdos_yuc_locales` dataset goes here
library(readr)
library(dplyr)

estado <- "yuc"
nombre_estado <- "YUCATAN"
id_estado <- "31"

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

# pm_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/YUC/YUC_PEL_2018/AYUNTAMIENTOS_csv/2018_SEE_AYUN_YUC_CAS.csv"

eleccion <- "pm_18"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(panal = na,
         # pes = es,
         noreg = can_nreg,
         total = total_votos,
         nominal = lista_nominal,
         distritol_18 = id_distrito_local,
         nombre_distritol_18 = cabecera_distrital_local,
         nombre_municipio_18 = municipio,
         municipio_18 = id_municipio) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  rename_with(~gsub("_na_", "_panal_", .x), contains("_na_")) |>
  homologar_bd(estado = id_estado, nombre_estado = ) |>
  mutate(distritol_18 = sprintf("%02s", distritol_18),
         municipio_18 = sprintf("%03s", municipio_18),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_18:clave_casilla, -c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion)

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

# gb_18 -------------------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/YUC/YUC_PEL_2018/GUBERNATURA_csv/2018_SEE_GOB_YUC_CAS.csv"

eleccion <- "gb_18"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(panal = na,
         # pes = es,
         noreg = can_nreg,
         total = total_votos,
         nominal = lista_nominal,
         distritol_18 = id_distrito_local,
         nombre_distritol_18 = cabecera_distrital_local,
         nombre_municipio_18 = municipio,
         municipio_18 = id_municipio) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  rename_with(~gsub("_na_", "_panal_", .x), contains("_na_")) |>
  homologar_bd(estado = id_estado, nombre_estado = nombre_estado) |>
  mutate(distritol_18 = sprintf("%02s", distritol_18),
         municipio_18 = sprintf("%03s", municipio_18),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_18:clave_casilla, -c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion)

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

# dl_18 -------------------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/YUC/YUC_PEL_2018/DIPUTACIONES_LOC_MR_csv/2018_SEE_DIP_LOC_MR_YUC_CAS.csv"

eleccion <- "dl_18"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(panal = na,
         # pes = es,
         noreg = can_nreg,
         total = total_votos,
         nominal = lista_nominal,
         distritol_18 = id_distrito_local,
         nombre_distritol_18 = cabecera_distrital_local,
         nombre_municipio_18 = municipio,
         municipio_18 = id_municipio) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  rename_with(~gsub("_na_", "_panal_", .x), contains("_na_")) |>
  homologar_bd(estado = id_estado, nombre_estado = ) |>
  mutate(distritol_18 = sprintf("%02s", distritol_18),
         municipio_18 = sprintf("%03s", municipio_18),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_18:clave_casilla, -c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion)

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))
# pm_21 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/YUC/YUC_PEL_2021/AYUNTAMIENTOS_csv/2021_SEE_AYUN_YUC_CAS.csv"

eleccion <- "pm_21"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(
    # panal = na,
    # pes = es,
    noreg = can_nreg,
    total = total_votos,
    nominal = lista_nominal,
    distritol_21 = id_distrito_local,
    nombre_distritol_21 = cabecera_distrital_local,
    nombre_municipio_21 = municipio,
    municipio_21 = id_municipio) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  rename_with(~gsub("_nay_", "_panal_", .x), contains("_nay_")) |>
  # rename_with(~gsub("_na_", "_panal_", .x), contains("_na_")) |>
  homologar_bd(estado = id_estado, nombre_estado = nombre_estado) |>
  mutate(distritol_21 = sprintf("%02s", distritol_21),
         municipio_21 = sprintf("%03s", municipio_21),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_21:clave_casilla, -c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion) |>
  mutate(ele_prd_panal_pm_21 = ele_prd_panal_pm_21 + ele_prd_panal_2_pm_21) |>
  select(-ele_prd_panal_2_pm_21)

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))
# dl_21 -------------------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/YUC/YUC_PEL_2021/DIPUTACIONES_LOC_MR_csv/2021_SEE_DIP_LOC_MR_YUC_CAS.csv"

eleccion <- "dl_21"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(
    # panal = na,
    # pes = es,
    noreg = can_nreg,
    total = total_votos,
    nominal = lista_nominal,
    distritol_21 = id_distrito_local,
    nombre_distritol_21 = cabecera_distrital_local,
    nombre_municipio_21 = municipio,
    municipio_21 = id_municipio) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  rename_with(~gsub("_nay_", "_panal_", .x), contains("_nay_")) |>
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


# Elecciones 2015 ---------------------------------------------------------
## PM 15
path_15 <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/computos/YUCATÁN/YUC_PEL_2015/AYUNTAMIENTOS_csv/2015_SEE_AYUN_YUC_CAS.csv"

eleccion <- "pm_15"

aux <- readr::read_csv(path_15) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_|can_", "", .x), contains(c("cand_", "can_"))) |>
  rename(
    panal = nva_alianza,
    pes = es,
    noreg = nreg,
    total = total_votos,
    nominal = lista_nominal,
    !!glue::glue("distritol_{readr::parse_number(eleccion)}") := id_distrito,
    !!glue::glue("nombre_distritol_{readr::parse_number(eleccion)}") := cabecera_distrital,
    !!glue::glue("nombre_municipio_{readr::parse_number(eleccion)}") := municipio,
    !!glue::glue("municipio_{readr::parse_number(eleccion)}") := id_municipio
  ) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  rename_with(~gsub("_nva_alianza_", "_panal_", .x), contains("_nva_alianza_")) |>
  homologar_bd(estado = id_estado, nombre_estado = nombre_estado) |>
  mutate(distritol_15 = sprintf("%02s", distritol_15),
         municipio_15 = sprintf("%03s", municipio_15),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_15:clave_casilla, -c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion)

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

## DL 15
path_15 <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/computos/YUCATÁN/YUC_PEL_2015/DIPUTACIONES_LOC_MR_csv/2015_SEE_DIP_LOC_MR_YUC_CAS.csv"

eleccion <- "dl_15"

aux <- readr::read_csv(path_15) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_|can_", "", .x), contains(c("cand_", "can_"))) |>
  rename(
    panal = nva_alianza,
    pes = es,
    noreg = nreg,
    total = total_votos,
    nominal = lista_nominal,
    !!glue::glue("distritol_{readr::parse_number(eleccion)}") := id_distrito,
    !!glue::glue("nombre_distritol_{readr::parse_number(eleccion)}") := cabecera_distrital,
    !!glue::glue("nombre_municipio_{readr::parse_number(eleccion)}") := municipio,
    !!glue::glue("municipio_{readr::parse_number(eleccion)}") := id_municipio
  ) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  rename_with(~gsub("_nva_alianza_", "_panal_", .x), contains("_nva_alianza_")) |>
  homologar_bd(estado = id_estado, nombre_estado = nombre_estado) |>
  mutate(distritol_15 = sprintf("%02s", distritol_15),
         municipio_15 = sprintf("%03s", municipio_15),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_15:clave_casilla, -c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion)

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))
