## code to prepare `bdos_cdmx_locales` dataset goes here
library(readr)
library(dplyr)

# Funciones ---------------------------------------------------------------
homologar_bd <- function(bd, estado, nombre_estado){
  bd |>
    mutate(estado = !!estado,
           nombre_estado = nombre_estado,
           casilla = if_else(casilla == "B", "B01",casilla),
           id_casilla = case_when(nchar(casilla) >= 4 ~ stringr::str_extract_all(casilla,"(?<=E)[^C]*?(\\d+)(?=C)"),
                                  T ~ stringr::str_extract_all(casilla,"(?<=[a-zA-Z])(\\d+)")),
           tipo_casilla = substr(casilla, 1, 1),
           ext_contigua = if_else(nchar(casilla) >= 4, stringr::str_extract_all(casilla,"(?<=C)(\\d+)"), list("0")),
           clave_casilla = glue::glue("{estado}{stringr::str_pad(seccion,pad = '0', width = 4)}{tipo_casilla}{stringr::str_pad(id_casilla,pad = '0', width = 2)}{stringr::str_pad(ext_contigua,pad = '0', width = 2)}")
    ) |>
    tidyr::unnest(cols = c(casilla:ext_contigua))
}

# Relación 18 ----------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Gobernador/cdmx_normal_casilla.csv"
rel_18 <- read_csv(path) |>
  janitor::clean_names() |>
  rename(municipio_18 = clave_dem,
         nombre_municipio_18 = dem,
         distritol_18 = distrito,
         noreg = no_reg,
         ind = independiente) |>
  mutate(municipio_18 = sprintf("%03s", municipio_18),
         distritol_18 = sprintf("%03s", distritol_18),
         seccion = sprintf("%04s", seccion)) |>
  homologar_bd(estado = "09", nombre_estado = "CIUDAD DE MÉXICO") |>
  distinct(municipio_18, nombre_municipio_18, distritol_18, seccion, clave_casilla, nominal)

# Relacion 21 -------------------------------------------------------------
# GB18 --------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Gobernador/cdmx_normal_casilla.csv"
aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename(municipio_18 = clave_dem,
         nombre_municipio_18 = dem,
         distritol_18 = distrito,
         noreg = no_reg,
         ind = independiente) |>
  mutate(municipio_18 = sprintf("%03s", municipio_18),
         distritol_18 = sprintf("%03s", distritol_18),
         seccion = sprintf("%04s", seccion)) |>
  homologar_bd(estado = "09", nombre_estado = "CIUDAD DE MÉXICO") %>%
  rename_with(~paste0("ele_", .x, "_gb_18"),
              .cols = c(pan:nominal)) |>
  select(estado:clave_casilla,casilla, municipio_18:seccion, everything())

colnames(aux) <- gsub("es_", "pes_", colnames(aux))

gb_18 <- aux
glimpse(gb_18)
write_rds(gb_18, "inst/electoral/cdmx/gb_18.rda")

# PM18-------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/Recursos/Externas/Limpieza/PEL/CDMX/2018/ALCALDIAS_csv/2018_SEE_AYUN_CDMX_CAS.csv"
pm_18 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(is.na(tribunal)) |>
  rename(estado = id_estado,
         distritol_18 = id_distrito_local,
         nombre_distritol_18 = cabecera_distrital_local,
         municipio_18 = id_municipio,
         nombre_municipio_18 = municipio,
         pes = es,
         noreg = num_votos_can_nreg,
         validos = num_votos_validos,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         panal = na
  ) |>
  rename_with(~gsub("cand_", "", .x), .cols = contains("cand")) |>
  rename_with(~gsub("c_comun_", "", .x), .cols = contains("c_comun_")) |>
  mutate(distritol_18 = sprintf("%03d", distritol_18),
         municipio_18 = sprintf("%03d", municipio_18),
         seccion = sprintf("%04d", seccion)) |>
  select(-c(estatus_acta:last_col(), circunscripcion)) |>
  mutate_all(~tidyr::replace_na(., 0)) |>
  mutate_if(is.logical, as.integer) |>
  homologar_bd(estado = "09",
               nombre_estado = "CDMX"
  ) |>
  relocate(clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "pm_18", sep = "_"), .cols = pan:last_col()) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_"))

glimpse(pm_18)
write_rds(pm_18, "inst/electoral/cdmx/pm_18.rda")
# DL18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/PEL/CDMX/2018/DIPUTACIONES_LOC_MR_csv/2018_SEE_DIP_LOC_MR_CDMX_CAS.csv"
aux <- read_csv(path) |>
  janitor::clean_names() |>
  filter(is.na(tribunal)) |>
  rename(estado = id_estado,
         distritol_18 = id_distrito_local,
         nombre_distritol_18 = cabecera_distrital_local,
         municipio_18 = id_municipio,
         nombre_municipio_18 = municipio,
         pes = es,
         noreg = num_votos_can_nreg,
         validos = num_votos_validos,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         panal = na
  ) |>
  rename_with(~gsub("cand_", "", .x), .cols = contains("cand")) |>
  mutate(prd_mc = prd_mc + c_comun_prd_mc) |>
  select(-c_comun_prd_mc) |>
  rename_with(~gsub("c_comun_", "", .x), .cols = contains("c_comun_")) |>
  mutate(distritol_18 = sprintf("%03d", distritol_18),
         municipio_18 = sprintf("%03d", municipio_18),
         seccion = sprintf("%04d", seccion)) |>
  select(-c(estatus_acta:last_col(), circunscripcion)) |>
  mutate_all(~replace_na(., 0)) |>
  mutate_if(is.logical, as.integer) |>
  homologar_bd(estado = "09",
               nombre_estado = "CDMX"
  ) |>
  relocate(clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "dl_18", sep = "_"), .cols = pan:last_col()) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_"))

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

dl_18 <- aux
glimpse(dl_18)
write_rds(dl_18, "inst/electoral/cdmx/dl_18.rda")
# PM21 --------------------------------------------------------------------
nombres_dl <- read_rds("inst/electoral/cdmx/dl_18.rda") |>
  distinct(distritol_18, nombre_distritol_18)

path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Municipio/cdmx_normal_casilla.csv"
aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename(
    distritol_21 = distrito_local,
    municipio_21 = clave_dem,
    nombre_municipio_21 = demarcacion_territorial,
    total = votacion_total,
    nominal = lista_nominal
  ) |>
  homologar_bd(estado = "09", nombre_estado = "CIUDAD DE MÉXICO")  |>
  mutate(distritol_21 = sprintf("%03s", distritol_21),
         municipio_21 = sprintf("%03s", municipio_21),
         seccion = sprintf("%04s", seccion),
         estado = "09"
  ) %>%
  left_join(nombres_dl, by = c("distritol_21" = "distritol_18")) |>
  rename_with(~paste0("ele_", .x, "_pm_21"), .cols = pan:nominal) |>
  select(estado:nombre_estado, nombre_distritol_21 = nombre_distritol_18,
         distritol_21:seccion, clave_casilla, tipo_casilla:casilla, everything()) %>%
  mutate_all(~tidyr::replace_na(., 0))

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

pm_21 <- aux
glimpse(pm_21)
write_rds(pm_21, "inst/electoral/cdmx/pm_21.rda")

# DL21 --------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Distrito local/cdmx_normal_casilla.csv"
aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename(
    distritol_21 = distrito_local,
    municipio_21 = clave_dem,
    nombre_municipio_21 = demarcacion_territorial,
    total = votos,
    noreg = no_reg
  ) |>
  homologar_bd(estado = "09", nombre_estado = "CIUDAD DE MÉXICO")  |>
  mutate(distritol_21 = sprintf("%03s", distritol_21),
         municipio_21 = sprintf("%03s", municipio_21),
         seccion = sprintf("%04s", seccion),
         estado = "09"
  ) %>%
  left_join(nombres_dl, by = c("distritol_21" = "distritol_18")) |>
  rename_with(~paste0("ele_", .x, "_dl_21"), .cols = pan:nominal) |>
  select(estado:nombre_estado, nombre_distritol_21 = nombre_distritol_18,
         distritol_21:seccion, clave_casilla, tipo_casilla:casilla, everything()) %>%
  mutate_all(~tidyr::replace_na(., 0))

glimpse(aux)

colnames(aux) <- gsub("independiente_", "ind", colnames(aux))

aux |>
  count(nchar(clave_casilla))

dl_21 <- aux
glimpse(dl_21)
write_rds(dl_21, "inst/electoral/cdmx/dl_21.rda")

# PM-15 -------------------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/CDMX/2015/JEFE_DELEGACIONAL_csv/2015_SEE_JEF_DELEG_DF_CAS.csv"

pm_15 <- readr::read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("cand_", "", .x)) |>
  rename_with(~gsub("nva_alianza", "panal", .x)) |>
  rename_with(~gsub("num_votos_", "", .x)) |>
  rename(noreg = can_nreg,
         total = total_votos,
         nominal = lista_nominal,
         estado = id_estado) |>
  mutate(nombre_estado = "CIUDAD DE MÉXICO",
         estado = sprintf("%02s", estado),
         seccion = sprintf("%04s", seccion)) |>
  rename_with(~paste("ele", .x, "pm_15", sep = "_"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  homologar_bd(estado = "09", nombre_estado = "CIUDAD DE MÉXICO") |>
  relocate(clave_casilla, .before = seccion)

pm_15 |>
  count(nchar(clave_casilla))

readr::write_rds(pm_15, "inst/electoral/cdmx/pm_15.rda")

# Resultados 2024 ---------------------------------------------------------
## Jefatura de gobierno
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/computos/CDMX/JG/2024_SEE_GOB_CDMX_CAS.csv"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename(
    nombre_distritol_24 = cabecera_distrital_local,
    distritol_24 = id_distrito_local,
    municipio_24 = id_municipio,
    nombre_municipio_24 = municipio,
    total = total_votos,
    nominal = lista_nominal,
    noreg = can_nreg,
  ) |>
  rename(casilla = acta_casilla_mec) |>
  homologar_bd(estado = "09", nombre_estado = "CIUDAD DE MÉXICO")  |>
  mutate(distritol_24 = sprintf("%02s", distritol_24),
         municipio_24 = sprintf("%03s", municipio_24),
         seccion = sprintf("%04s", seccion),
         estado = "09"
  ) %>%
  rename_with(~paste0("ele_", .x, "_gb_24"), .cols = pan:nominal) |>
  mutate_all(~tidyr::replace_na(., 0)) |>
  select(-id_estado)

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

gb_24 <- aux
write_rds(gb_24, "inst/electoral/cdmx/gb_24.rda")

# pm_24 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/computos/CDMX/ALC/2024_SEE_AYUN_CDMX_CAS.csv"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(
    nombre_distritol_24 = cabecera_distrital_local,
    distritol_24 = id_distrito_local,
    municipio_24 = id_municipio,
    nombre_municipio_24 = municipio,
    total = total_votos,
    nominal = lista_nominal,
    noreg = can_nreg,
  ) |>
  rename(casilla = acta_casilla_mec) |>
  homologar_bd(estado = "09", nombre_estado = "CIUDAD DE MÉXICO")  |>
  mutate(distritol_24 = sprintf("%02s", distritol_24),
         municipio_24 = sprintf("%03s", municipio_24),
         seccion = sprintf("%04s", seccion),
         estado = "09"
  ) %>%
  rename_with(~paste0("ele_", .x, "_pm_24"), .cols = pan:nominal) |>
  mutate_all(~tidyr::replace_na(., 0)) |>
  select(-id_estado)

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

pm_24 <- aux
write_rds(pm_24, "inst/electoral/cdmx/pm_24.rda")

# DL_24 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/computos/CDMX/DIP MR/2024_SEE_DIP_LOC_MR_CDMX_CAS.csv"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(
    nombre_distritol_24 = cabecera_distrital_local,
    distritol_24 = id_distrito_local,
    municipio_24 = id_municipio,
    nombre_municipio_24 = municipio,
    total = total_votos,
    nominal = lista_nominal,
    noreg = can_nreg,
  ) |>
  rename(casilla = acta_casilla_mec) |>
  homologar_bd(estado = "09", nombre_estado = "CIUDAD DE MÉXICO")  |>
  mutate(distritol_24 = sprintf("%02s", distritol_24),
         municipio_24 = sprintf("%03s", municipio_24),
         seccion = sprintf("%04s", seccion),
         estado = "09"
  ) %>%
  rename_with(~paste0("ele_", .x, "_dl_24"), .cols = pan:nominal) |>
  mutate_all(~tidyr::replace_na(., 0)) |>
  select(-id_estado)

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

dl_24 <- aux
write_rds(dl_24, "inst/electoral/cdmx/dl_24.rda")


