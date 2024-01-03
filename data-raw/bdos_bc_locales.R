## code to prepare `bdos_bc_locales` dataset goes here

library(readr)
library(dplyr)

estado <- "bc"
nombre_estado <- "BAJA CALIFORNIA"
id_estado <- "02"

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

# pm_19 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/BC/BC_PEL_2019/AYUNTAMIENTOS_csv/2019_SEE_AYUN_BC_CAS.csv"

eleccion <- "pm_19"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  mutate(municipio = if_else(id_municipio == 5 & municipio == "ENSENADA", "PLAYAS DE ROSARITO", municipio),
         municipio = if_else(municipio == "ROSARITO", "PLAYAS DE ROSARITO", municipio)) |> #OJO AQUÍ
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(
    # panal = na,
    # pes = es,
    noreg = can_nreg,
    total = total_votos,
    nominal = lista_nominal,
    distritol_19 = id_distrito_local,
    nombre_distritol_19 = cabecera_distrital_local,
    nombre_municipio_19 = municipio,
    municipio_19 = id_municipio) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  # rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  # rename_with(~gsub("_na_", "_panal_", .x), contains("_na_")) |>
  homologar_bd(estado = id_estado, nombre_estado = nombre_estado) |>
  mutate(distritol_19 = sprintf("%02s", distritol_19),
         municipio_19 = sprintf("%03s", municipio_19),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_19:clave_casilla, -c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion)

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

# gb_19 -------------------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/BC/BC_PEL_2019/GUBERNATURA_csv/2019_SEE_GOB_BC_CAS.csv"

eleccion <- "gb_19"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  mutate(municipio = if_else(id_municipio == 5 & municipio == "ENSENADA", "PLAYAS DE ROSARITO", municipio),
         municipio = if_else(municipio == "ROSARITO", "PLAYAS DE ROSARITO", municipio)) |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(
    # panal = na,
    # pes = es,
    noreg = can_nreg,
    total = total_votos,
    nominal = lista_nominal,
    distritol_19 = id_distrito_local,
    nombre_distritol_19 = cabecera_distrital_local,
    nombre_municipio_19 = municipio,
    municipio_19 = id_municipio) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  # rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  # rename_with(~gsub("_na_", "_panal_", .x), contains("_na_")) |>
  homologar_bd(estado = id_estado, nombre_estado = nombre_estado) |>
  mutate(distritol_19 = sprintf("%02s", distritol_19),
         municipio_19 = sprintf("%03s", municipio_19),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_19:clave_casilla, -c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion)

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

# dl_19 -------------------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/BC/BC_PEL_2019/DIPUTACIONES_LOC_MR_csv/2019_SEE_DIP_LOC_MR_BC_CAS.csv"

eleccion <- "dl_19"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  mutate(municipio = if_else(id_municipio == 5 & municipio == "ENSENADA", "PLAYAS DE ROSARITO", municipio),
         municipio = if_else(municipio == "ROSARITO", "PLAYAS DE ROSARITO", municipio)) |>
  rename_with(~gsub("num_votos_", "", .x), contains("num_votos_")) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  rename(
    # panal = na,
    # pes = es,
    noreg = can_nreg,
    total = total_votos,
    nominal = lista_nominal,
    distritol_19 = id_distrito_local,
    nombre_distritol_19 = cabecera_distrital_local,
    nombre_municipio_19 = municipio,
    municipio_19 = id_municipio) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  rename_with(~gsub("_na_", "_panal_", .x), contains("_na_")) |>
  homologar_bd(estado = id_estado, nombre_estado = ) |>
  mutate(distritol_19 = sprintf("%02s", distritol_19),
         municipio_19 = sprintf("%03s", municipio_19),
         seccion = sprintf("%04s", seccion)) |>
  select(estado, nombre_estado, distritol_19:clave_casilla, -c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion)

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))
# pm_21 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/BC/BC_PEL_2021/AYUNTAMIENTOS_csv/2021_SEE_AYUN_BC_CAS.csv"
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
  # rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  # rename_with(~gsub("_nay_", "_panal_", .x), contains("_nay_")) |>
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
# dl_21 -------------------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/BC/BC_PEL_2021/DIPUTACIONES_LOC_MR_csv/2021_SEE_DIP_LOC_MR_BC_CAS.csv"

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

# gb_21 -------------------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/BC/BC_PEL_2021/GUBERNATURA_csv/2021_SEE_GOB_BC_CAS.csv"

eleccion <- "gb_21"

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
  # rename_with(~gsub("_es_", "_pes_", .x), contains("_es_")) |>
  # rename_with(~gsub("_nay_", "_panal_", .x), contains("_nay_")) |>
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
