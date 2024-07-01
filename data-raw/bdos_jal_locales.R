## code to prepare `bdos_jal_locales` dataset goes here
library(tidyverse)
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


# pm 18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Distrito local/jalisco_normal_casilla.csv"

dl_18 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(is.na(tribunal)) |>
  rename(estado = id_estado,
         distritol_18 = id_distrito_local,
         nombre_distrio = cabecera_distrital_local,
         municipio_18 = id_municipio,
         nombre_municipio = municipio,
         pes = es,
         noreg = num_votos_can_nreg,
         validos = num_votos_validos,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         panal = na
  ) |>
  rename_with(~gsub("cand_", "", .x), .cols = contains("cand")) |>
  mutate(distritol_18 = sprintf("%03d", distritol_18),
         municipio_18 = sprintf("%03d", municipio_18),
         seccion = sprintf("%04d", seccion)) |>
  select(-c(estatus_acta:last_col(), circunscripcion)) |>
  mutate_all(~replace_na(., 0)) |>
  mutate_if(is.logical, as.integer) |>
  homologar_bd(estado = "14",
               nombre_estado = "JALISCO") |>
  relocate(clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "dl_18", sep = "_"), pan:last_col()) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_"))

glimpse(dl_18)

dl_18 |>
  count(nchar(clave_casilla))

write_rds(dl_18, "inst/electoral/jal/dl_18.rda")
# gb_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Gobernador/jalisco_normal_casilla.csv"

gb_18 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(is.na(tribunal)) |>
  rename(estado = id_estado,
         distritol_18 = id_distrito_local,
         nombre_distrio = cabecera_distrital_local,
         municipio_18 = id_municipio,
         nombre_municipio = municipio,
         pes = es,
         noreg = num_votos_can_nreg,
         validos = num_votos_validos,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         panal = na
  ) |>
  rename_with(~gsub("cand_", "", .x), .cols = contains("cand")) |>
  mutate(distritol_18 = sprintf("%03d", distritol_18),
         municipio_18 = sprintf("%03d", municipio_18),
         seccion = sprintf("%04d", seccion),
         casilla = if_else(grepl("MEC", casilla), gsub("MEC", "M", casilla), casilla)) |>
  select(-c(estatus_acta:last_col(), circunscripcion)) |>
  mutate_all(~replace_na(., 0)) |>
  mutate_if(is.logical, as.integer) |>
  homologar_bd(estado = "14",
               nombre_estado = "JALISCO") |>
  relocate(clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "gb_18", sep = "_"), pan:last_col()) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_"))

glimpse(gb_18)

gb_18 |>
  filter(is.na(distritol_18))

gb_18 |>
  count(nchar(clave_casilla))

write_rds(gb_18, "inst/electoral/jal/gb_18.rda")
# pm_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Municipio/jalisco_normal_casilla.csv"

pm_18 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(is.na(tribunal)) |>
  rename(estado = id_estado,
         distritol_18 = id_distrito_local,
         nombre_distrio = cabecera_distrital_local,
         municipio_18 = id_municipio,
         nombre_municipio = municipio,
         pes = es,
         noreg = num_votos_can_nreg,
         validos = num_votos_validos,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         panal = na
  ) |>
  rename_with(~gsub("cand_", "", .x), .cols = contains("cand")) |>
  mutate(distritol_18 = sprintf("%03d", distritol_18),
         municipio_18 = sprintf("%03d", municipio_18),
         seccion = sprintf("%04d", seccion),
         casilla = if_else(grepl("MEC", casilla), gsub("MEC", "M", casilla), casilla)) |>
  select(-c(estatus_acta:last_col(), circunscripcion)) |>
  mutate_all(~replace_na(., 0)) |>
  mutate_if(is.logical, as.integer) |>
  homologar_bd(estado = "14",
               nombre_estado = "JALISCO") |>
  relocate(clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "pm_18", sep = "_"), pan:last_col()) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_"))

glimpse(pm_18)

pm_18 |>
  count(nchar(clave_casilla))

write_rds(pm_18, "inst/electoral/jal/pm_18.rda")
# dl_21 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Distrito local/jalisco_normal_casilla.csv"
dl_21 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(is.na(tribunal)) |>
  rename(estado = id_estado,
         distritol_21 = id_distrito_local,
         nombre_distrio = cabecera_distrital_local,
         municipio_21 = id_municipio,
         nombre_municipio = municipio,
         noreg = num_votos_can_nreg,
         validos = num_votos_validos,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal
  ) |>
  rename_with(~gsub("cand_", "", .x), .cols = contains("cand")) |>
  mutate(distritol_21 = sprintf("%03d", distritol_21),
         municipio_21 = sprintf("%03d", municipio_21),
         seccion = sprintf("%04d", seccion),
         casilla = if_else(grepl("MEC", casilla), gsub("MEC", "M", casilla), casilla)) |>
  select(-c(estatus_acta:last_col(), circunscripcion)) |>
  mutate_all(~replace_na(., 0)) |>
  mutate_if(is.logical, as.integer) |>
  homologar_bd(estado = "14",
               nombre_estado = "JALISCO") |>
  relocate(clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "dl_21", sep = "_"), pan:last_col()) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_"))

glimpse(dl_21)

dl_21 |>
  count(nchar(clave_casilla))

write_rds(dl_21, "inst/electoral/jal/dl_21.rda")
# pm_21 -------------------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Municipio/jalisco_normal_casilla.csv"
pm_21 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(is.na(tribunal)) |>
  rename(estado = id_estado,
         distritol_21 = id_distrito_local,
         nombre_distrio = cabecera_distrital_local,
         municipio_21 = id_municipio,
         nombre_municipio = municipio,
         noreg = num_votos_can_nreg,
         validos = num_votos_validos,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal
  ) |>
  rename_with(~gsub("cand_", "", .x), .cols = contains("cand")) |>
  mutate(distritol_21 = sprintf("%03d", distritol_21),
         municipio_21 = sprintf("%03d", municipio_21),
         seccion = sprintf("%04d", seccion),
         casilla = if_else(grepl("MEC", casilla), gsub("MEC", "M", casilla), casilla)) |>
  select(-c(estatus_acta:last_col(), circunscripcion)) |>
  mutate_all(~replace_na(., 0)) |>
  mutate_if(is.logical, as.integer) |>
  homologar_bd(estado = "14",
               nombre_estado = "JALISCO") |>
  relocate(clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "pm_21", sep = "_"), pan:last_col()) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_"))

glimpse(pm_21)

pm_21 |>
  count(nchar(clave_casilla))

write_rds(pm_21, "inst/electoral/jal/pm_21.rda")
# Elecciones 2015 ---------------------------------------------------------
# pm 15 -------------------------------------------------------------------
path_15 <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/computos/JALISCO/JAL_PEL_2015/AYUNTAMIENTOS_csv/2015_SEE_AYUN_JAL_CAS.csv"
estado <- "jal"
nombre_estado <- "JALISCO"
id_estado <- "14"
eleccion <- "pm_15"

aux <- readr::read_csv(path_15) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_|can_", "", .x), contains(c("cand_", "can_"))) |>
  rename(
    panal = nva_alianza,
    # pes = es,
    noreg = nreg,
    total = total_votos,
    nominal = lista_nominal,
    !!glue::glue("distritol_{readr::parse_number(eleccion)}") := id_distrito,
    !!glue::glue("nombre_distritol_{readr::parse_number(eleccion)}") := cabecera_distrital,
    !!glue::glue("nombre_municipio_{readr::parse_number(eleccion)}") := municipio,
    !!glue::glue("municipio_{readr::parse_number(eleccion)}") := id_municipio
  ) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  # rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
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

# pm 15 -------------------------------------------------------------------
path_15 <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/computos/JALISCO/JAL_PEL_2015/DIPUTACIONES_LOC_MR_csv/2015_SEE_DIP_LOC_MR_JAL_CAS.csv"
eleccion <- "dl_15"

aux <- readr::read_csv(path_15) |>
  janitor::clean_names() |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_|can_", "", .x), contains(c("cand_", "can_"))) |>
  rename(
    panal = nva_alianza,
    # pes = es,
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
