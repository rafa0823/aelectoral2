## code to prepare `bdos_pue_locales` dataset goes here
library(readr)
library(dplyr)
library(stringr)
# Funciones ---------------------------------------------------------------
homologar_bd <- function(bd, estado){
  bd |>
    mutate(casilla = if_else(casilla == "B", "B01",casilla),
           id_casilla = case_when(nchar(casilla) >= 4 ~ stringr::str_extract_all(casilla,"(?<=E)[^C]*?(\\d+)(?=C)"),
                                  T ~ stringr::str_extract_all(casilla,"(?<=[a-zA-Z])(\\d+)")),
           tipo_casilla = substr(casilla, 1, 1),
           ext_contigua = if_else(nchar(casilla) >= 4, stringr::str_extract_all(casilla,"(?<=C)(\\d+)"), list("0")),
           clave_casilla = glue::glue("{estado}{str_pad(seccion,pad = '0', width = 4)}{tipo_casilla}{str_pad(id_casilla,pad = '0', width = 2)}{str_pad(ext_contigua,pad = '0', width = 2)}")
    ) |>
    tidyr::unnest(cols = c(casilla:ext_contigua))
}
homologar_bd_pue <- function(bd) {
  bd |>
    mutate(casilla = if_else(grepl("EXTRAORDINARIA", casilla) & grepl("CONTIGUA", casilla),
                             gsub("CONTIGUA ", "", casilla), casilla),
           casilla = gsub("BASICA", "B", casilla),
           casilla = gsub("CONTIGUA", "C", casilla),
           casilla = gsub("EXTRAORDINARIA", "E", casilla),
           casilla = gsub("ESPECIAL", "S", casilla)
    )|>
    tidyr::separate(casilla, into = c("tipo_casilla", "id_casilla", "ext_contigua"), remove = FALSE) |>
    tidyr::replace_na(replace = list(ext_contigua = "00", id_casilla = "01")) |>
    mutate(clave_casilla = glue::glue("21{sprintf('%04s', seccion)}{tipo_casilla}{sprintf('%02s', id_casilla)}{sprintf('%02s',ext_contigua)}"))
}
# Relación 18 --------------------------------------------------------
df_18 <- read_rds("inst/electoral/nacional/df_18.rda") |>
  filter(nombre_estado == "PUEBLA") |>
  distinct(clave_casilla, distritof_18 = distritof, nombre_distritof_18 = nombre_distritof,
           municipio_18 = municipio, seccion, nominal = ele_nominal_df_18)

relacion_18 <- df_18

relacion_18 |>
  count(nchar(clave_casilla))
# Relacion 21 -------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Federal/2021/Diputado_normal_casilla.csv"
df_21 <- read_delim(path, delim = "|", skip = 5) |>
  janitor::clean_names() |>
  filter(id_estado == 21) |>
  mutate(clave_casilla = gsub("'", "", clave_casilla)) |>
  distinct(clave_casilla, distritof_21 = id_distrito, nombre_distritof_21 = nombre_distrito, seccion = as.numeric(seccion))

df_21 |>
  count(nchar(clave_casilla))

path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Municipio/puebla_normal_casilla.csv"
pm_21 <- read_csv(path, skip = 2) |>
  janitor::clean_names() |>
  distinct(distritol_21 = id_distrito_local, nombre_municipio_21 = municipio_local, seccion, casilla = paste0(tipo_casilla, id_casilla)) |>
  homologar_bd(estado = 21) |>
  select(-c(id_casilla, tipo_casilla, casilla, ext_contigua))

pm_21 |>
  count(nchar(clave_casilla))

relacion_21 <- df_21 |>
  left_join(pm_21, join_by(clave_casilla, seccion))

# gb_18 --------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Gobernador/puebla_normal_casilla.xlsx"
aux <- readxl::read_excel(path, skip = 1) |>
  janitor::clean_names() |>
  mutate(seccion = sprintf("%04s", seccion)) |>
  homologar_bd_pue() |>
  rename(distritol_18 = distrito_local,
         nombre_distritol_18 = cabecera,
         nombre_municipio_18 = municipio,
         noreg = no_registrados) |>
  left_join(relacion_18, join_by(clave_casilla, seccion)) |>
  relocate(nominal, .after = total)

elec <- "gb_18"

glimpse(aux)

aux <- aux |>
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio_18 = formatC(municipio_18, width = 3, flag = "0"),
         distritol_18 = formatC(distritol_18, width = 2, flag = "0"),
         distritof_18 = formatC(distritof_18, width = 2, flag = "0"),
         estado = "21",
         nombre_estado = "PUEBLA") |>
  rename_with(~paste0('ele_', .x, "_", elec),
              .cols = pan:nominal)

# colnames(aux) <- sub("_es_", "_pes_", colnames(aux))

aux |>
  filter(is.na(ele_nominal_gb_18)) |> #La relación se completó satisfactoriamente
  select(distritof_18, municipio_18, distritol_18, clave_casilla, casilla, everything())

final <- aux  %>%
  select(estado, nombre_estado, distritof_18, nombre_distritof_18, distritol_18, nombre_distritol_18, municipio_18, nombre_municipio_18, seccion, clave_casilla, everything())

##TEST: El test satisfactorio cuando todas las claves casilla tienen 11 caracteres

final |>
  count(nchar(clave_casilla))

gb_18 <- final
glimpse(gb_18)

write_rds(gb_18, glue::glue("inst/electoral/pue/{elec}.rda"))
# pm_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Municipio/puebla_normal_casilla.xlsx"
aux <- readxl::read_excel(path, skip = 1) |>
  janitor::clean_names() |>
  mutate(seccion = sprintf("%04s", seccion)) |>
  homologar_bd_pue() |>
  rename(distritol_18 = distrito_local,
         nombre_distritol_18 = cabecera,
         nombre_municipio_18 = municipio,
         noreg = no_registrados,
         candidatura_independiente2 = candidatura_independiente_2) |>
  left_join(relacion_18, join_by(clave_casilla, seccion)) |>
  relocate(nominal, .after = total)

elec <- "pm_18"

glimpse(aux)

aux |>
  filter(is.na(nominal)) |> #La relación se completó satisfactoriamente
  select(distritof_18, municipio_18, distritol_18,  nominal, clave_casilla, casilla, everything())

aux <- aux |>
  mutate(across(pan:nominal, ~as.numeric(.x)),
         estado = "21",
         nombre_estado = "PUEBLA") |>
  rename_with(~paste0('ele_', .x, "_", elec),
              .cols = pan:nominal)

colnames(aux) <- sub("_na_", "_panal_", colnames(aux))
colnames(aux) <- sub("candidatura_independiente", "ind", colnames(aux))

final <- aux  %>%
  select(estado, nombre_estado, distritof_18, nombre_distritof_18, distritol_18, nombre_distritol_18, municipio_18, nombre_municipio_18, seccion, clave_casilla, everything())

##TEST: El test satisfactorio cuando todas las claves casilla tienen 11 caracteres

final |>
  count(nchar(clave_casilla))

pm_18 <- final
glimpse(pm_18)
write_rds(pm_18, glue::glue("inst/electoral/pue/{elec}.rda"))
# dl_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Distrito local/puebla_normal_casilla.xlsx"
aux <- readxl::read_excel(path, skip = 1) |>
  janitor::clean_names() |>
  mutate(seccion = sprintf("%04s", seccion)) |>
  homologar_bd_pue() |>
  rename(distritol_18 = distrito_local,
         nombre_distritol_18 = cabecera,
         nombre_municipio_18 = municipio,
         noreg = no_registrados) |>
  left_join(relacion_18, join_by(clave_casilla, seccion)) |>
  relocate(nominal, .after = total)

elec <- "dl_18"

glimpse(aux)

aux |>
  filter(is.na(nominal)) |> #La relación se completó satisfactoriamente
  select(distritof_18, municipio_18, distritol_18,  nominal, clave_casilla, casilla, everything())

aux <- aux |>
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio_18 = formatC(municipio_18, width = 3, flag = "0"),
         distritol_18 = formatC(distritol_18, width = 2, flag = "0"),
         distritof_18 = formatC(distritof_18, width = 2, flag = "0"),
         clave_casilla = gsub("'", "", clave_casilla),
         estado = "21",
         nombre_estado = "PUEBLA") |>
  rename_with(~paste0('ele_', .x, "_", elec),
              .cols = pan:nominal)

colnames(aux) <- sub("_na_", "_panal_", colnames(aux))

final <- aux  %>%
  select(estado, nombre_estado, distritof_18, nombre_distritof_18, distritol_18, nombre_distritol_18, municipio_18, nombre_municipio_18, seccion, clave_casilla, everything())

##TEST: El test satisfactorio cuando todas las claves casilla tienen 11 caracteres

final |>
  count(nchar(clave_casilla))

dl_18 <- final
glimpse(dl_18)
write_rds(dl_18, glue::glue("inst/electoral/pue/{elec}.rda"))
# pm_21 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Municipio/puebla_normal_casilla.xlsx"
nombres_mun <- read_rds("inst/electoral/pue/pm_18.rda") |>
  distinct(nombre_municipio_21 = nombre_municipio_18, municipio_21 = municipio_18)

aux <- readxl::read_excel(path, skip = 4) |>
  janitor::clean_names() |>
  mutate(
    estado = "21",
    seccion = sprintf('%04s', seccion),
    clave_casilla = glue::glue("{estado}{seccion}{tipo_casilla}{sprintf('%02s', id_casilla)}{sprintf('%02s', ext_contigua)}"),
  ) |>
  rename(distritol_21 = id_distrito_local,
         nombre_distritol_21 = cabecera_distrital_local,
         nombre_municipio_21 = municipio_local,
         nominal = lista_nominal_casilla,
         nulos = num_votos_nulos,
         noreg = no_registrados,
         total = total_votos) |>
  left_join(nombres_mun) |>
  relocate(municipio_21, .after = nombre_municipio_21) |>
  select(nombre_estado, estado, clave_casilla, distritol_21:id_tipo_candidatura, pan:cand_ind_2, nominal:total, -numero_votos_validos)

elec <- "pm_21"

glimpse(aux)

colnames(aux) <- sub("cand_ind_", "ind", colnames(aux))

final <- aux |>
  mutate(across(pan:total, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         distritol_21 = formatC(distritol_21, width = 2, flag = "0")) |>
  rename_with(~paste0('ele_', .x, "_", elec),
              .cols = pan:total)

##TEST: El test satisfactorio cuando todas las claves casilla tienen 11 caracteres

final |>
  count(nchar(clave_casilla))

pm_21 <- final
glimpse(pm_21)

write_rds(pm_21, glue::glue("inst/electoral/pue/{elec}.rda"))


# dl_21 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Distrito local/puebla_normal_casilla.xlsx"
nombres_mun <- read_rds("inst/electoral/pue/pm_18.rda") |>
  distinct(nombre_municipio_21 = nombre_municipio_18, municipio_21 = municipio_18)

aux <- readxl::read_excel(path, skip = 4) |>
  janitor::clean_names() |>
  mutate(
    estado = "21",
    seccion = sprintf('%04s', seccion),
    clave_casilla = glue::glue("{estado}{seccion}{tipo_casilla}{sprintf('%02s', id_casilla)}{sprintf('%02s', ext_contigua)}"),
  ) |>
  rename(distritol_21 = id_distrito_local,
         nombre_distritol_21 = cabecera_distrital_local,
         nombre_municipio_21 = municipio_local,
         nominal = lista_nominal_casilla,
         nulos = num_votos_nulos,
         noreg = no_registrados,
         total = total_votos) |>
  left_join(nombres_mun) |>
  relocate(municipio_21, .after = nombre_municipio_21) |>
  select(nombre_estado, estado, clave_casilla, distritol_21:id_tipo_candidatura, pan:cand_ind_2, nominal:total, -numero_votos_validos)

elec <- "dl_21"

glimpse(aux)

colnames(aux) <- sub("cand_ind_", "ind", colnames(aux))

aux <- aux |>
  mutate(across(pan:total, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         distritol_21 = formatC(distritol_21, width = 2, flag = "0")) |>
  rename_with(~paste0('ele_', .x, "_", elec),
              .cols = pan:total)

colnames(aux) <- sub("_na_", "_panal_", colnames(aux))
##TEST: El test satisfactorio cuando todas las claves casilla tienen 11 caracteres
final <- aux
final |>
  count(nchar(clave_casilla))

dl_21 <- final
glimpse(dl_21)

write_rds(dl_21, glue::glue("inst/electoral/pue/{elec}.rda"))

