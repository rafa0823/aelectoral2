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
  mutate(distritol_18 = sprintf("17_%02s", distritol_18),
         municipio_18 = sprintf("17_%03s", municipio_18),
         seccion = sprintf("17_%04s", seccion)) |>
  select(estado, nombre_estado, distritol_18:clave_casilla, -c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion)

glimpse(aux)
gb_18 <- aux
write_rds(gb_18, file = "inst/electoral/mor/gb_18.rda")
# pm_18 -------------------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/MOR/MOR_PEL_2018/AYUNTAMIENTOS_csv/2018_SEE_AYUN_MOR_CAS.csv"
aux <- read_csv(path) |>
  janitor::clean_names() |> glimpse()
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
  rename_with(~paste0("ele_", .x, "_gb_18"), .cols = c(pan:ind1)) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  homologar_bd(estado = "17", "MORELOS") |>
  mutate(distritol_18 = sprintf("17_%02s", distritol_18),
         municipio_18 = sprintf("17_%03s", municipio_18),
         seccion = sprintf("17_%04s", seccion)) |>
  select(estado, nombre_estado, distritol_18:clave_casilla)

glimpse(aux)
gb_18 <- aux
write_rds(gb_18, file = "inst/electoral/mor/gb_18.rda")


glimpse(aux)
pm_18 <- aux
write_rds(pm_18, file = "inst/electoral/mor/pm_18.rda")
# dl_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Distrito local/morelos"
temp <- list.files(path, full.names = T) %>%
  map_df(~{
    read_excel(.x, skip = 1) |>
      janitor::remove_empty(which = "rows") |>
      janitor::clean_names()
  })

glimpse(temp)
aux <- temp |>
  rename_with(~gsub("votos_", "", .x), .cols = contains("votos_")) |>
  rename_with(~gsub("coal_", "", .x), .cols = contains("coal_")) |>
  rename_with(~gsub("config_", "", .x), .cols = contains("config_")) |>
  rename_with(~gsub("cand_", "", .x), .cols = contains("cand_")) |>
  rename(panal = na,
         ph = humanista,
         noreg = no_registrados,
         nulos = num_nulos,
         total = total_de_votos,
         nominal = lista_nominal) |>
  mutate(seccion = if_else(seccion == 1000, "0000", sprintf('%04s', seccion)),
         casilla = if_else(seccion == 1000, "S1", casilla),
         pt_morena_pes = sum(pt_morena_pes, cc_pt_morena_pes, na.rm = T),
         pt_morena = sum(pt_morena, cc_pt_morena, na.rm = T),
         morena_pes = sum(morena_pes, cc_morena_pes, na.rm = T),
         pt_pes = sum(pt_pes, cc_pt_pes, na.rm = T)
  ) |>
  select(-contains("cc_")) |>
  homologar_bd(estado = "17", nombre_estado = "MORELOS") |>
  left_join(select(rel_18, clave_casilla, distritol_18, contains("municipio")), join_by(clave_casilla)) |>
  select(estado, nombre_estado, distritol_18, contains("municipio"), seccion, clave_casilla, id_casilla:ext_contigua, casilla, everything())

viejos <- colnames(aux)[grepl("ind_", colnames(aux))]
nuevos <- paste0("ind", seq_along(viejos))

aux <- aux |>
  rename_at(all_of(viejos), ~nuevos) |>
  rename_with(~paste0("ele_", .x, "_dl_18"), .cols = c(pan:ind7)) |>
  mutate_all(~tidyr::replace_na(., 0))

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
dl_18 <- aux
write_rds(dl_18, file = "inst/electoral/mor/dl_18.rda")

# Relación 21 -------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Municipio/nombres_municipios_morelos.csv"
nom_mun <- read_csv(path, skip = 4) |>
  janitor::clean_names() |>
  filter(clave_casilla != "-") |>
  distinct(municipio_21 = id_municipio,
           nombre_municipio_21 = municipio,
           seccion) |>
  na.omit()

path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Distrito local/nombre_dl_morelos.csv"
nom_dl <- read_csv(path, skip = 4) |>
  janitor::clean_names() |>
  filter(clave_casilla != "-", distrito_local != "MORELOS") |>
  distinct(distritol_21 = id_distrito_local,
           nombre_distritol_21 = distrito_local,
           seccion) |>
  na.omit()

rel_21 <- left_join(nom_mun, nom_dl, join_by(seccion))

rel_21 |>
  filter(n() > 1, .by = seccion)
# pm_21 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Municipio/2021_SEE_AYUN_MOR_CAS.csv"
aux <- read_csv(path) |>
  janitor::clean_names() |>
  filter(is.na(tribunal)) |>
  rename(estado = id_estado,
         distritol_21 = id_distrito_local,
         nombre_distritol_21 = cabecera_distrital_local,
         municipio_21 = id_municipio,
         validos = num_votos_validos,
         noreg = num_votos_can_nreg,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal
  ) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  select(-c(circunscripcion, tipo_casilla:ext_contigua, estatus_acta:ruta_acta)) |>
  rename_with(~gsub("panalm", "panal", .x), contains("panalm")) |>
  mutate(estado = as.character(estado),
         distritol_21 = sprintf("%03s", distritol_21),
         municipio_21 = sprintf("%03s", municipio_21),
         seccion = sprintf("%04s", seccion)) |>
  homologar_bd(estado = "17", nombre_estado = "MORELOS") |>
  rename_with(~paste0("ele_", .x, "_pm_21"), .cols = c(pan:nominal)) |>
  mutate_all(~tidyr::replace_na(., 0))

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
pm_21 <- aux
write_rds(pm_21, file = "inst/electoral/mor/pm_21.rda")

# dl_21 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Distrito local/2021_SEE_DIP_LOC_MR_MOR_CAS.csv"
aux <- read_csv(path) |>
  janitor::clean_names() |>
  filter(is.na(tribunal)) |>
  rename(estado = id_estado,
         distritol_21 = id_distrito_local,
         nombre_distritol_21 = cabecera_distrital_local,
         municipio_21 = id_municipio,
         validos = num_votos_validos,
         noreg = num_votos_can_nreg,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal
  ) |>
  select(-c(circunscripcion, tipo_casilla:ext_contigua, estatus_acta:ruta_acta)) |>
  rename_with(~gsub("panalm", "panal", .x), contains("panalm")) |>
  mutate(estado = as.character(estado),
         distritol_21 = sprintf("%03s", distritol_21),
         municipio_21 = sprintf("%03s", municipio_21),
         seccion = sprintf("%04s", seccion)) |>
  homologar_bd(estado = "17", nombre_estado = "MORELOS") |>
  rename_with(~paste0("ele_", .x, "_dl_21"), .cols = c(pan:nominal)) |>
  mutate_all(~tidyr::replace_na(., 0))

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
dl_21 <- aux
write_rds(dl_21, file = "inst/electoral/mor/dl_21.rda")

