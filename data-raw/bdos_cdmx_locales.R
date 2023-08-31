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
  mutate(municipio_18 = sprintf("%02s", municipio_18),
         distritol_18 = sprintf("%02s", distritol_18),
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
  mutate(municipio_18 = sprintf("%02s", municipio_18),
         distritol_18 = sprintf("%02s", distritol_18),
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
mun <- rel_18 |>
  distinct(clave_casilla, municipio_18, nombre_municipio_18, nominal)
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Municipio/cdmx_normal_casilla.csv"
aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename(distritol_18 = distrito,
         noreg = no_reg) |>
  homologar_bd(estado = "09", nombre_estado = "CIUDAD DE MÉXICO")  |>
  mutate(distritol_18 = sprintf("%02s", distritol_18),
         seccion = sprintf("%04s", seccion),
         estado = "09") |>
  rowwise() |>
  mutate(total = sum(c_across(pan:noreg))) |>
  left_join(mun, join_by(clave_casilla)) |>
  relocate(total, .after = noreg) |>
  relocate(nominal, .after = total) |>
  rename_with(~paste0("ele_", .x, "_pm_18"),
              .cols = c(pan:nominal)) |>
  select(estado, nombre_estado:seccion, municipio_18:nombre_municipio_18, clave_casilla, casilla, id_casilla:ext_contigua, everything())

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

colnames(aux) <- sub("independiente_", "ind", colnames(aux))
colnames(aux) <- sub("_es_", "_pes_", colnames(aux))

pm_18 <- aux
glimpse(pm_18)
write_rds(pm_18, "inst/electoral/cdmx/pm_18.rda")
# DL18 -------------------------------------------------------------------
mun <- rel_18 |>
  distinct(clave_casilla, municipio_18, nombre_municipio_18, nominal)
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Distrito local/cdmx_normal_casilla.csv"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  rename(distritol_18 = distrito,
         noreg = no_reg) |>
  homologar_bd(estado = "09", nombre_estado = "CIUDAD DE MÉXICO")  |>
  mutate(distritol_18 = sprintf("%02s", distritol_18),
         seccion = sprintf("%04s", seccion),
         estado = "09",
         prd_mc = if_else(prd_mc_33 > 0, prd_mc_33, prd_mc_29)) |>
  relocate(prd_mc, .before = nulos) |>
  select(-c(prd_mc_33, prd_mc_29)) |>
  rowwise() |>
  mutate(total = sum(c_across(pan:noreg))) |>
  left_join(mun, join_by(clave_casilla)) |>
  relocate(total, .after = noreg) |>
  relocate(nominal, .after = total) |>
  rename_with(~paste0("ele_", .x, "_dl_18"),
              .cols = c(pan:nominal)) |>
  select(estado, nombre_estado:seccion, municipio_18:nombre_municipio_18, clave_casilla, casilla, id_casilla:ext_contigua, everything())

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

colnames(aux) <- sub("independiente_", "ind", colnames(aux))
colnames(aux) <- sub("_es_", "_pes_", colnames(aux))

dl_18 <- aux
glimpse(dl_18)
write_rds(dl_18, "inst/electoral/cdmx/dl_18.rda")
# PM21 --------------------------------------------------------------------
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
  mutate(distritol_21 = sprintf("%02s", distritol_21),
         municipio_21 = sprintf("%02s", municipio_21),
         seccion = sprintf("%04s", seccion),
         estado = "09"
  ) %>%
  rename_with(~paste0("ele_", .x, "_pm_21"), .cols = pan:nominal) |>
  select(estado:nombre_estado, distritol_21:seccion, clave_casilla, tipo_casilla:casilla, everything()) %>%
  mutate_all(~tidyr::replace_na(., 0))

glimpse(aux)

aux |>
  count(nchar(clave_casilla))

pm_21 <- aux
glimpse(pm_21)
write_rds(pm_21, "inst/electoral/cdmx/pm_21.rds")

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
  mutate(distritol_21 = sprintf("%02s", distritol_21),
         municipio_21 = sprintf("%02s", municipio_21),
         seccion = sprintf("%04s", seccion),
         estado = "09"
  ) %>%
  rename_with(~paste0("ele_", .x, "_dl_21"), .cols = pan:nominal) |>
  select(estado:nombre_estado, distritol_21:seccion, clave_casilla, tipo_casilla:casilla, everything()) %>%
  mutate_all(~tidyr::replace_na(., 0))

glimpse(aux)

colnames(aux) <- gsub("independiente_", "ind", colnames(aux))

aux |>
  count(nchar(clave_casilla))

dl_21 <- aux
glimpse(dl_21)
write_rds(dl_21, "inst/electoral/cdmx/dl_21.rds")
