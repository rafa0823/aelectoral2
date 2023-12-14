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
pm_18 <- aux
write_rds(pm_18, file = "inst/electoral/mor/pm_18.rda")
# dl_18 -------------------------------------------------------------------
