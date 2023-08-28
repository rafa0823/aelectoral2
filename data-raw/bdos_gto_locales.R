## code to prepare `bdos_gto_locales` dataset goes here
library(readr)
library(dplyr)
library(stringr)
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
           clave_casilla = glue::glue("{estado}{str_pad(seccion,pad = '0', width = 4)}{tipo_casilla}{str_pad(id_casilla,pad = '0', width = 2)}{str_pad(ext_contigua,pad = '0', width = 2)}")
    ) |>
    tidyr::unnest(cols = c(casilla:ext_contigua))
}
# Lista nominal 18 --------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Lista nominal/2018/guanajuato_18_nominal.xlsx"

ln_18 <- readxl::read_excel(path, skip = 6) |>
  janitor::clean_names()

# Relacion 21 -------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Distrito local/guanajuato_normal_casilla.csv"
dl_21 <- read_csv(path, skip = 6) |>
  janitor::clean_names() |>
  distinct(distritol_21 = distrito_local, seccion) |>
  mutate(distritol_21 = as.numeric(as.roman(gsub("DISTRITO ", "", distritol_21))))

path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Municipio/guanajuato_normal_casilla.csv"
mun_21 <- read_csv(path, skip = 6) |>
  janitor::clean_names() |>
  distinct(municipio_21 = id_municipio, seccion)
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Federal/2021/Diputado_normal_casilla.csv"
df_21 <- read_delim(path, skip = 5, delim = "|") |>
  janitor::clean_names() |>
  filter(id_estado == 11) |>
  distinct(distritof_21 = id_distrito, nombre_distrito_21 = nombre_distrito, seccion = as.integer(seccion))

relacion_21 <-  mun_21 |>
  left_join(dl_21) |>
  left_join(df_21)

# gb_18 ------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Gobernador/guanajuato_normal_casilla.csv"
gb_18 <- read_csv(path) |>
  janitor::clean_names()

#Modificar los partidos que están escritos de manera distinta: pes, panal
colnames(gb_18) <- sub("es", "pes", colnames(gb_18))

colnames(gb_18) #Revisar que todo esté en orden

## Hay que pegarle el municipio a partir de la lista nominal
nom_mun <- ln_18 |>
  distinct(municipio, nombre_municipio_18 = nom_municipio, seccion)

gb_18 <- gb_18 |>
  left_join(nom_mun) |>
  rename(municipio_18 = municipio,
         distritol_18 = distrito
         ) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio_18 = formatC(municipio_18, width = 3, flag = "0"),
         distritol_18 = formatC(distritol_18, width = 2, flag = "0")) |>
  rename_with(~paste0('ele_', .x, "_gb_18"),
              .cols = pan:nominal)

## E1C10 hay unas casillas que tienen más de 4 número de caracteres. ¿Esto cómo se procesa?
final_gb18_gto <- gb_18  %>%
  homologar_bd(estado = "11", nombre_estado = "GUANAJUATO") |>
  select(estado, nombre_estado, distritol_18, municipio_18, seccion, id_casilla:clave_casilla, everything())

##TEST: El test satisfactorio cuando todas las claves casilla tienen 11 caracteres

final_gb18_gto |>
  count(nchar(clave_casilla))

# guardar rda

gb_18 <- final_gb18_gto

write_rds(gb_18, "inst/electoral/gto/gb_18.rda")


# dl_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Distrito local/guanajuato_normal_casilla.csv"

dl_18 <- read_csv(path) |>
  janitor::clean_names()

#Modificar los partidos que están escritos de manera distinta: pes, panal
colnames(dl_18) <- sub("es", "pes", colnames(dl_18))

colnames(dl_18) #Revisar que todo esté en orden

## Hay que pegarle el municipio a partir de la lista nominal
nom_mun <- ln_18 |>
  distinct(municipio, nombre_municipio_18 = nom_municipio, seccion)

dl_18 <- dl_18 |>
  left_join(nom_mun) |>
  rename(municipio_18 = municipio,
         distritol_18 = ubicacion
  ) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio_18 = formatC(municipio_18, width = 3, flag = "0"),
         distritol_18 = formatC(distritol_18, width = 2, flag = "0")) |>
  rename_with(~paste0('ele_', .x, "_dl_18"),
              .cols = pan:nominal)

## E1C10 hay unas casillas que tienen más de 4 número de caracteres. ¿Esto cómo se procesa?
final_dl18_gto <- dl_18  %>%
  homologar_bd(estado = "11", nombre_estado = "GUANAJUATO") |>
  select(estado, nombre_estado, distritol_18, municipio_18, seccion, id_casilla:clave_casilla, everything())

##TEST: El test satisfactorio cuando todas las claves casilla tienen 11 caracteres

final_dl18_gto |>
  count(nchar(clave_casilla))

# guardar rda

dl_18 <- final_dl18_gto

write_rds(dl_18, "inst/electoral/gto/dl_18.rda")



# pm_18 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2018/Municipio/guanajuato_normal_casilla.csv"
elec <- "pm_18"

aux <- read_csv(path) |>
  janitor::clean_names() |>
  mutate(seccion = as.integer(seccion))

#Modificar los partidos que están escritos de manera distinta: pes, panal
colnames(aux) <- sub("es", "pes", colnames(aux))

colnames(aux) #Revisar que todo esté en orden

## Hay que pegarle el municipio a partir de la lista nominal
nom_mun <- ln_18 |>
  distinct(municipio, seccion, distritol_18 = distritacion_local_2016)

aux <- aux |>
  left_join(nom_mun) |>
  rename(municipio_18 = municipio) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio_18 = formatC(municipio_18, width = 3, flag = "0"),
         distritol_18 = formatC(distritol_18, width = 2, flag = "0")) |>
  rename_with(~paste0('ele_', .x, "_", elec),
              .cols = pan:nominal)

## E1C10 hay unas casillas que tienen más de 4 número de caracteres. ¿Esto cómo se procesa?
final <- aux  %>%
  homologar_bd(estado = "11", nombre_estado = "GUANAJUATO") |>
  select(estado, nombre_estado, distritol_18, municipio_18, seccion, id_casilla:clave_casilla, everything())

##TEST: El test satisfactorio cuando todas las claves casilla tienen 11 caracteres

final |>
  count(nchar(clave_casilla))

# guardar rda

pm_18 <- final

write_rds(pm_18, glue::glue("inst/electoral/gto/{elec}.rda"))



# pm_21 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Municipio/guanajuato_normal_casilla.csv"
elec <- "pm_21"

aux <- read_csv(path, skip = 6) |>
  janitor::clean_names() |>
  select(-id_municipio) |>
  left_join(relacion_21, join_by(seccion))

glimpse(aux) #Revisar que todo esté en orden

aux <- aux |>
  rename(nombre_municipio_21 = municipio,
         nominal = lista_nominal,
         nombre_estado = estado,
         estado = id_estado,
         total = total_votos_calculado) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio_21 = formatC(municipio_21, width = 3, flag = "0"),
         distritol_21 = formatC(distritol_21, width = 2, flag = "0"),
         clave_casilla = gsub("'", "", clave_casilla)) |>
  rename_with(~paste0('ele_', .x, "_", elec),
              .cols = pan:nominal)

colnames(aux) <- sub("_na", "_panal", names(aux))

## E1C10 hay unas casillas que tienen más de 4 número de caracteres. ¿Esto cómo se procesa?
final <- aux  %>%
  select(estado, nombre_estado, distritof_21, nombre_distrito_21, distritol_21, municipio_21, seccion, id_casilla:clave_casilla, everything())

##TEST: El test satisfactorio cuando todas las claves casilla tienen 11 caracteres

final |>
  count(nchar(clave_casilla))

# guardar rda

pm_21 <- final
glimpse(pm_21)

write_rds(pm_21, glue::glue("inst/electoral/gto/{elec}.rda"))


# dl_21 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Distrito local/guanajuato_normal_casilla.csv"
elec <- "dl_21"

aux <- read_csv(path, skip = 6) |>
  janitor::clean_names() |>
  select(-c(id_distrito_local, distrito_local)) |>
  left_join(relacion_21, join_by(seccion))

glimpse(aux) #Revisar que todo esté en orden

aux <- aux |>
  rename(nominal = lista_nominal,
         nombre_estado = estado,
         estado = id_estado,
         total = total_votos_calculado) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio_21 = formatC(municipio_21, width = 3, flag = "0"),
         distritol_21 = formatC(distritol_21, width = 2, flag = "0"),
         clave_casilla = gsub("'", "", clave_casilla)) |>
  rename_with(~paste0('ele_', .x, "_", elec),
              .cols = pan:nominal)

colnames(aux) <- sub("_na", "_panal", names(aux))
colnames(aux) <- sub("_gto", "", names(aux))

## E1C10 hay unas casillas que tienen más de 4 número de caracteres. ¿Esto cómo se procesa?
final <- aux  %>%
  select(estado, nombre_estado, distritof_21, nombre_distrito_21, distritol_21, municipio_21, seccion, id_casilla:clave_casilla, everything())

##TEST: El test satisfactorio cuando todas las claves casilla tienen 11 caracteres

final |>
  count(nchar(clave_casilla))

# guardar rda

dl_21 <- final
glimpse(dl_21)

write_rds(dl_21, glue::glue("inst/electoral/gto/{elec}.rda"))

