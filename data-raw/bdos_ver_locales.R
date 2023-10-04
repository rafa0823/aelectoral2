## code to prepare `bdos_ver_locales` dataset goes here
library(tidyverse)

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


# pm_17 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2017/Municipal/veracruz_normal_casilla.csv"
pm_17 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(tepjf != "Anulada" | is.na(tepjf)) |>
  rename(estado = id_estado,
         distritol_17 = id_distrito,
         nombre_distritol_17 = cabecera_distrital,
         municipio_17 = id_municipio,
         nombre_municipio_17 = municipio,
         pes = es,
         noreg = num_votos_can_nreg,
         validos = num_votos_validos,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         panal = nva_alianza
         ) |>
  rename_with(~gsub("cand_", "", .x), .cols = contains("cand")) |>
  mutate(distritol_17 = sprintf("%03d", distritol_17),
         municipio_17 = sprintf("%03d", municipio_17),
         seccion = sprintf("%04d", seccion)) |>
  select(-c(estatus_acta:last_col(), circunscripcion)) |>
  mutate_all(~replace_na(., 0)) |>
  mutate_if(is.logical, as.integer) |>
  homologar_bd(estado = "30",
               nombre_estado = "VERACRUZ"
               ) |>
  relocate(id_casilla:clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "pm_17", sep = "_"), .cols = pan:last_col())

path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2017/Municipal/veracruz_ext_casilla.csv"
pm_17_ext <- read_csv(path) |>
  janitor::clean_names() |>
  filter(!is.na(id_estado), tepjf != "Anulada" | is.na(tepjf)) |>
  rename(estado = id_estado,
         distritol_17 = id_distrito,
         nombre_distritol_17 = cabecera_distrital,
         municipio_17 = id_municipio,
         nombre_municipio_17 = municipio,
         pes = es,
         noreg = num_votos_can_nreg,
         validos = num_votos_validos,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         panal = nva_alianza
  ) |>
  rename_with(~gsub("cand_", "", .x), .cols = contains("cand")) |>
  mutate(distritol_17 = sprintf("%03d", distritol_17),
         municipio_17 = sprintf("%03d", municipio_17),
         seccion = sprintf("%04d", seccion)) |>
  select(-c(estatus_acta:last_col())) |>
  homologar_bd(estado = "30",
               nombre_estado = "VERACRUZ"
  ) |>
  mutate_if(is.double, ~replace_na(., 0)) |>
  relocate(id_casilla:clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "pm_17", sep = "_"), .cols = pan:last_col())

glimpse(pm_17)

naniar::vis_miss(pm_17)

pm_17 <- bind_rows(pm_17, pm_17_ext)
pm_17 <- pm_17 |>
  mutate_all(~replace_na(., 0))

write_rds(pm_17, "inst/electoral/ver/pm_17.rda")
# gb_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Gobernador/veracruz_normal_casilla.csv"
gb_18 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(is.na(tribunal)) |>
  rename(estado = id_estado,
         distritol_18 = id_distrito_local,
         nombre_distritol_18 = cabecera_distrital_local,
         municipio_18 = id_municipio,
         nombre_municipio_18 = municipio,
         pes = es,
         panal = na,
         noreg = num_votos_can_nreg,
         validos = num_votos_validos,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal) |>
  mutate(distritol_18 = sprintf("%03d", distritol_18),
         municipio_18 = sprintf("%03d", municipio_18),
         seccion = sprintf("%04d", seccion)) |>
  select(-c(estatus_acta:last_col(), circunscripcion)) |>
  homologar_bd(estado = "30",
               nombre_estado = "VERACRUZ"
  ) |>
  mutate_if(is.double, ~replace_na(., 0)) |>
  relocate(clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "gb_18", sep = "_"), .cols = pan:last_col()) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_"))

glimpse(gb_18)

write_rds(gb_18, "inst/electoral/ver/gb_18.rda")
# dl_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Distrito local/veracruz_normal_casilla.csv"
dl_18 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(tribunal != "ANULADA" | is.na(tribunal)) |>
  rename(estado = id_estado,
         distritol_18 = id_distrito_local,
         nombre_distritol_18 = cabecera_distrital_local,
         municipio_18 = id_municipio,
         nombre_municipio_18 = municipio,
         pes = es,
         panal = na,
         noreg = num_votos_can_nreg,
         validos = num_votos_validos,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  mutate(distritol_18 = sprintf("%03d", distritol_18),
         municipio_18 = sprintf("%03d", municipio_18),
         seccion = sprintf("%04d", seccion)) |>
  select(-c(estatus_acta:last_col(), circunscripcion)) |>
  homologar_bd(estado = "30",
               nombre_estado = "VERACRUZ"
  ) |>
  mutate_all(~replace_na(., 0)) |>
  relocate(clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "dl_18", sep = "_"), .cols = pan:last_col()) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_"))

dl_18 |>
  count(nchar(clave_casilla))

glimpse(dl_18)

write_rds(dl_18, "inst/electoral/ver/dl_18.rda")

# pm_21 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Municipio/veracruz_normal_casilla.csv"
pm_21 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(is.na(tribunal)) |>
  rename(estado = id_estado,
         distritol_21 = id_distrito_local,
         nombre_distritol_21 = cabecera_distrital_local,
         municipio_21 = id_municipio,
         nombre_municipio_21 = municipio,
         noreg = num_votos_can_nreg,
         validos = num_votos_validos,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  mutate(distritol_21 = sprintf("%03d", distritol_21),
         municipio_21 = sprintf("%03d", municipio_21),
         seccion = sprintf("%04d", seccion)) |>
  select(-c(estatus_acta:last_col(), circunscripcion)) |>
  homologar_bd(estado = "30",
               nombre_estado = "VERACRUZ"
  ) |>
  mutate_all(~replace_na(., 0)) |>
  relocate(clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "pm_21", sep = "_"), .cols = pan:last_col())

glimpse(pm_21)

pm_21 |>
  count(nchar(clave_casilla))

write_rds(pm_21, "inst/electoral/ver/pm_21.rda")
# dl_21 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Distrito local/veracruz_normal_casilla.csv"
dl_21 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(is.na(tribunal)) |>
  rename(estado = id_estado,
         distritol_21 = id_distrito_local,
         nombre_distritol_21 = cabecera_distrital_local,
         municipio_21 = id_municipio,
         nombre_municipio_21 = municipio,
         noreg = num_votos_can_nreg,
         validos = num_votos_validos,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal) |>
  mutate(distritol_21 = sprintf("%03d", distritol_21),
         municipio_21 = sprintf("%03d", municipio_21),
         seccion = sprintf("%04d", seccion)) |>
  select(-c(estatus_acta:last_col(), circunscripcion)) |>
  homologar_bd(estado = "30",
               nombre_estado = "VERACRUZ"
  ) |>
  mutate_all(~replace_na(., 0)) |>
  relocate(clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "dl_21", sep = "_"), .cols = pan:last_col())

dl_21 |>
  count(nchar(clave_casilla))

glimpse(dl_21)

write_rds(dl_21, "inst/electoral/ver/dl_21.rda")
