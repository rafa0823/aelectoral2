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
# Relación 18 --------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Gobernador/puebla_normal_casilla.csv"
dl_18 <- read_csv(path) |>
  janitor::clean_names() |>
  mutate(clave_casilla = gsub("'", "", clave_casilla)) |>
  distinct(clave_casilla, distritol_18 = distrito_local, nombre_distritol_18 = nombre_distrito_local, seccion)

path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Federal/2018/diputados_federales/diputados_mr.xlsx"
relacion_18 <- readxl::read_excel(path) |>
  janitor::clean_names() |>
  filter(nombre_estado == "PUEBLA") |>
  homologar_bd(estado = 21) |>
  distinct(nominal = lista_nominal, distritof_18 = id_distrito, nombre_distritof_18 = cabecera_distrital,
           municipio_18 = id_municipio, nombre_municipio_18 = municipio, seccion, clave_casilla) |>
  select(nominal, everything()) |>
  left_join(dl_18, join_by(clave_casilla, seccion))

relacion_18 |>
  count(nchar(clave_casilla))
# Relacion 21 -------------------------------------------------------------

# GB18 --------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Gobernador/puebla_normal_casilla.csv"
aux <- read_csv(path) |>
  janitor::clean_names() |>
  select(-nominal) |>
  mutate(clave_casilla = gsub("'", "", clave_casilla)) |>
  left_join(relacion_18, join_by(clave_casilla, seccion))

elec <- "gb_18"

glimpse(aux)

aux <- aux |>
  rename(total = total_votos_calculado,
         distritol_18 = distrito_local,
         nombre_distritol_18 = nombre_distrito_local) %>%
  filter(!is.na(total)) |>
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio_18 = formatC(municipio_18, width = 3, flag = "0"),
         distritol_18 = formatC(distritol_18, width = 2, flag = "0"),
         distritof_18 = formatC(distritof_18, width = 2, flag = "0")) |>
  rename_with(~paste0('ele_', .x, "_", elec),
              .cols = pan:nominal)

colnames(aux) <- sub("_es_", "_pes_", colnames(aux))

final <- aux  %>%
  select(estado, nombre_estado, distritof_18, nombre_distritof_18, distritol_18, nombre_distritol_18, municipio_18, nombre_municipio_18, seccion, clave_casilla, everything())

##TEST: El test satisfactorio cuando todas las claves casilla tienen 11 caracteres

final |>
  count(nchar(clave_casilla))

gb_18 <- final
glimpse(gb_18)

write_rds(gb_18, glue::glue("inst/electoral/pue/{elec}.rda"))
# pm_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Municipio/puebla_normal_casilla.csv"
aux <- read_csv(path) |>
  janitor::clean_names() |>
  select(-c(distrito, nombre_municipio)) |>
  mutate(casilla = sub("CONTIGUA", "C", casilla),
         casilla = sub("EXTRAORDINARIA", "E", casilla),
         casilla = sub("BASICA", "B", casilla),
         casilla = gsub(" ", "", casilla)) |>
  filter(!grepl("ESPECIAL", casilla)) |>
  homologar_bd(estado = 21) |>
  left_join(relacion_18, join_by(clave_casilla, seccion)) |>
  relocate(nominal, .after = total)

elec <- "pm_18"

glimpse(aux)

aux |>
  filter(is.na(distritof_18), is.na(municipio_18), is.na(distritol_18)) #La relación se completó satisfactoriamente

aux <- aux |>
  filter(!is.na(total)) |>
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio_18 = formatC(municipio_18, width = 3, flag = "0"),
         distritol_18 = formatC(distritol_18, width = 2, flag = "0"),
         distritof_18 = formatC(distritof_18, width = 2, flag = "0"),
         clave_casilla = gsub("'", "", clave_casilla),
         estado = 21,
         nombre_estado = "PUEBLA") |>
  rename_with(~paste0('ele_', .x, "_", elec),
              .cols = pan:nominal)

colnames(aux) <- sub("es_", "pes_", colnames(aux))

final <- aux  %>%
  select(estado, nombre_estado, distritof_18, nombre_distritof_18, distritol_18, nombre_distritol_18, municipio_18, nombre_municipio_18, seccion, clave_casilla, everything())

##TEST: El test satisfactorio cuando todas las claves casilla tienen 11 caracteres

final |>
  count(nchar(clave_casilla))

pm_18 <- final

write_rds(pm_18, glue::glue("inst/electoral/pue/{elec}.rda"))
# dl_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Distrito local/puebla_normal_casilla.csv"
aux <- read_csv(path) |>
  janitor::clean_names() |>
  select(-c(distrito, nombre_municipio)) |>
  mutate(casilla = sub("CONTIGUA", "C", casilla),
         casilla = sub("EXTRAORDINARIA", "E", casilla),
         casilla = sub("BASICA", "B", casilla),
         casilla = gsub(" ", "", casilla)) |>
  filter(!grepl("ESPECIAL", casilla)) |>
  homologar_bd(estado = 21) |>
  left_join(relacion_18, join_by(clave_casilla, seccion)) |>
  relocate(nominal, .after = total)

elec <- "dl_18"

glimpse(aux)

aux |>
  filter(is.na(distritof_18), is.na(municipio_18), is.na(distritol_18)) #La relación se completó satisfactoriamente

aux <- aux |>
  filter(!is.na(total)) |>
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio_18 = formatC(municipio_18, width = 3, flag = "0"),
         distritol_18 = formatC(distritol_18, width = 2, flag = "0"),
         distritof_18 = formatC(distritof_18, width = 2, flag = "0"),
         clave_casilla = gsub("'", "", clave_casilla),
         estado = 21,
         nombre_estado = "PUEBLA") |>
  rename_with(~paste0('ele_', .x, "_", elec),
              .cols = pan:nominal)

colnames(aux) <- sub("es_", "pes_", colnames(aux))

final <- aux  %>%
  select(estado, nombre_estado, distritof_18, nombre_distritof_18, distritol_18, nombre_distritol_18, municipio_18, nombre_municipio_18, seccion, clave_casilla, everything())

##TEST: El test satisfactorio cuando todas las claves casilla tienen 11 caracteres

final |>
  count(nchar(clave_casilla))

dl_18 <- final

write_rds(dl_18, glue::glue("inst/electoral/pue/{elec}.rda"))

