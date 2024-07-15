
# FEDERALES

# CARGAR BASES ----------------------------------------------------------------------------------------
#renv::deactivate()
library(readr)
library(dplyr)

homologar_bd <- function(bd){
  bd |>
    mutate(casilla = case_when(casilla == "B"~ "B01",
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


pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# pr 18

bd_pr_18 <- pr_18 <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal/2018/Presidente_casilla.xlsx")%>%
  janitor::clean_names() %>%
  as_tibble()

# df_18

setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal/2018/diputados_federales")

bd_df_18 <- list.files(full.names = T) %>%
  map_dfr(~{
    print(.x)
    read_excel(.x) %>%
      janitor::clean_names() %>%
      mutate(
        mr_rp = toupper(substr(gsub("./diputados_","",.x),1,2)))
  })

bd_df_18 %>% names()

setwd("~/Documents/Git/aelectoral2")

# df 21

bd_df_21 <- read_delim("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/diputaciones.csv",
                       delim = "|", escape_double = FALSE, trim_ws = TRUE,  locale = locale(encoding = "CP1252"),
                       skip = 5) %>%
  janitor::clean_names() %>%
  as_tibble()

bd_df_15 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal/2015/Diputado_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

bd_df_12 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal/2012/Diputado_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()


# FEDERALES -----------------------------------------------------------------------

## Senado 2012

path <-  "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/Resultados definitivos/Federal/2012/Senador_casilla.csv"
sen_12 <- readr::read_csv(path, col_types = "c") |>
  janitor::clean_names() |>
  filter(tipo_casilla != "MEC") |>
  rename(distritof_12 = distrito,
         municipio_12 = municipio,
         total = votos,
         nominal = lista_nominal,
         noreg = no_reg,
  ) |>
  select(-c(lista_nominal_casilla)) |>
  mutate(estado = sprintf("%02d", estado),
         distritof_12 = sprintf("%03d", distritof_12),
         municipio_12 = sprintf("%03d", municipio_12),
         seccion = sprintf("%04d", seccion)) |>
  relocate(c(orden:total), .after = pt_mc) |>
  relocate(municipio_12, .after = distritof_12)

sen_12_c <- sen_12 |>
  filter(nchar(clave_casilla) == 11)

sen_12_e <- sen_12 |>
  filter(nchar(clave_casilla) != 11) |>
  mutate(clave_casilla = glue::glue("{estado}{seccion}{tipo_casilla}{sprintf('%02s', id_casilla)}{sprintf('%02s', ext_contigua)}"))

sen_12 <- bind_rows(sen_12_c, sen_12_e)

readr::write_rds(sen_12, "inst/electoral/nacional/sen_12.rda")


## Senado 2018
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Federal/2018/senadores/2018_SEE_SEN_MR_NAL_CAS.csv"
sen_18 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(tipo_casilla != "MEC") |>
  rename(estado = id_estado,
         distritof_18 = id_distrito,
         nombre_distritof = cabecera_distrital,
         municipio_18 = id_municipio,
         nombre_municipio = municipio,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         noreg = num_votos_can_nreg
         ) |>
  mutate(estado = sprintf("%02d", estado),
         distritof_18 = sprintf("%03d", distritof_18),
         municipio_18 = sprintf("%03d", municipio_18),
         seccion = sprintf("%04d", seccion)) |>
  rename_with(~gsub("cand_", "", .x), contains("cand_")) |>
  select(-c(tipo_casilla:ext_contigua)) |>
  mutate(
    casilla = if_else(casilla == "B", "B01",casilla),
    id_casilla = case_when(nchar(casilla) >= 4 ~ stringr::str_extract_all(casilla,"(?<=E)[^C]*?(\\d+)(?=C)"),
                           T ~ stringr::str_extract_all(casilla,"(?<=[a-zA-Z])(\\d+)")),
    tipo_casilla = substr(casilla, 1, 1),
    ext_contigua = if_else(nchar(casilla) >= 4, stringr::str_extract_all(casilla,"(?<=C)(\\d+)"), list("0")),
    clave_casilla = glue::glue("{estado}{stringr::str_pad(seccion,pad = '0', width = 4)}{tipo_casilla}{stringr::str_pad(id_casilla,pad = '0', width = 2)}{stringr::str_pad(ext_contigua,pad = '0', width = 2)}")
  ) |>
  tidyr::unnest(cols = c(casilla:ext_contigua)) |>
  relocate(c(clave_casilla, id_casilla:ext_contigua), .after = seccion) |>
  mutate_if(is.logical, as.integer) |>
  mutate_if(is.integer, ~replace_na(., 0)) |>
  rename_with(~paste("ele", .x, "sen_18", sep = "_"), pan:nominal) |>
  rename_with(~gsub("_na_", "_panal_", .x), contains("_na_")) |>
  rename_with(~gsub("_es_", "_pes_", .x), contains("_es_"))

sen_18 |>
  count(nchar(clave_casilla))

glimpse(sen_18)

write_rds(sen_18, "inst/electoral/nacional/sen_18.rda")

## dipfed 21 ------------------------------------------------------------------

# Identificar nombres de variables y nombres de partidos

df21 <- bd_df_21 %>%
  clean_names()


colnames(df21)

# Homogenizar nombres de variables partidos

df21 <- df21  %>%
  rename("noreg" = candidato_a_no_registrado_a,
         "nulos" = votos_nulos,
         "total" = total_votos_calculados,
         "nominal" = lista_nominal_casilla,
         "distritof" = id_distrito,
         "nombre_distritof" = nombre_distrito,
         "estado" = id_estado) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         estado = formatC(estado, width = 2,flag = "0"),
         seccion = formatC(seccion, width = 4,flag = "0"),
         id_casilla = formatC(id_casilla, width = 2,flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2,flag = "0"),
         distritof = formatC(distritof, width = 2, flag = "0"),
         mr_rp = gsub(pattern = "[[:digit:]]","",num_acta_impreso),
         mr_rp = gsub(pattern = "E","",mr_rp),
         mr_rp = if_else(mr_rp == "","MR",mr_rp),
         seccion = if_else(tipo_casilla == "P","9999",seccion),
         nombre_distritof = if_else(tipo_casilla == "P","PREVENTIVA",nombre_distritof))


df21 <- df21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# revisar nombres de partidos

detectar_partidos(df21)



# sufijo para join

final_df21 <- insertar_sufijo(bd=df21, "df", "21") %>%
  mutate(clave_casilla = substr(clave_casilla,2,nchar(clave_casilla)-1))

# nchar casillas

final_df21 %>% count(nchar(clave_casilla))

# guardar rda

nac_df_21 <- final_df21

nac_df_21 %>% write_rds("inst/electoral/nacional/df_21.rda")

rm(df21)


## presidente 18 ------------------------------------------------------------------

# Identificar nombres de variables y nombres de partidos

pr18 <- bd_pr_18

colnames(pr18)

pr18 <- bd_pr_18   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))

# revisar nombres de varianles

colnames(pr18)

pr18 <- pr18 %>%
  rename(noreg = num_votos_can_nreg,
         distritof = id_distrito,
         nombre_distritof = cabecera_distrital,
         panal =  na,
         nombre_municipio = municipio,
         municipio = id_municipio,
         panal = na,
         pes = es,
         pri_pvem_panal = pri_pvem_na,
         pt_morena_pes = pt_morena_es,
         pvem_panal = pvem_na,
         pri_panal = pri_na,
         pt_pes = pt_es,
         morena_pes = morena_es,
         pt_pes = pt_es,
         ind1 = cand_ind1,
         ind2 = cand_ind2,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal
  ) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritof = formatC(distritof, width = 3, flag = "0"),
         id_estado = formatC(id_estado, width = 2, flag = "0"))


pr18 <- pr18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pr18)

# sufijo para join

final_pr18 <- insertar_sufijo(bd=pr18, "pr", "18")

#agregar clave casillas

final_pr18 <- final_pr18%>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(id_estado,seccion,id_casilla))

# se va voto en el extranjero volver a poner cuando se pueda

final_pr18 <- final_pr18 %>% filter(!grepl("MEC", casilla))

#prueba

final_pr18 %>% count(nchar(clave_casilla))


# guardar rda

pr_18 <- final_pr18

pr_18 %>% write_rds("inst/electoral/nacional/pr_18.rda")

rm(pr18)



## DIPFED 18 -------------------------------------------------------------

# Identificar nombres de variables y nombres de partidos

df18 <- bd_df_18

colnames(df18)

df18 <- bd_df_18   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))

# revisar nombres de varianles

colnames(df18)

df18 <- df18 %>%
  rename(noreg = num_votos_can_nreg,
         distritof = id_distrito,
         nombre_distritof = cabecera_distrital,
         panal =  na,
         nombre_municipio = municipio,
         municipio = id_municipio,
         panal = na,
         pes = es,
         pri_pvem_panal = pri_pvem_na,
         pt_morena_pes = pt_morena_es,
         pvem_panal = pvem_na,
         pri_panal = pri_na,
         pt_pes = pt_es,
         morena_pes = morena_es,
         pt_pes = pt_es,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal
  ) %>%
  rename_with( ~ gsub("cand_", "", .x, fixed = T)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritof = formatC(distritof, width = 3, flag = "0"),
         id_estado = formatC(id_estado, width = 2, flag = "0"))


df18 <- df18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(df18)

# sufijo para join

final_df18 <- insertar_sufijo(bd=df18, "df", "18")

#agregar clave casillas

final_df18 <- final_df18%>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(id_estado,seccion,id_casilla))

# se va voto en el extranjero volver a poner cuando se pueda

final_df18 <- final_df18 %>% filter(!grepl("MEC", casilla))

#prueba

final_df18 %>% count(nchar(clave_casilla))


# guardar rda

df_18 <- final_df18 %>% as_tibble()

df_18 %>% saveRDS("inst/electoral/nacional/df_18.rda")

rm(df18)


## DIPFED 15 --------------------------------------------------------------------------


# Identificar nombres de variables y nombres de partidos

df15 <- bd_df_15%>%
  mutate(clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla,ext_contigua))

df15[df15 == "-"] <- NA


colnames(df15)

# Homogenizar nombres de variables partidos

df15 <- df15  %>%
  rename("noreg" = no_reg,
         "distritof" = distrito,
         "pes" = ps) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         estado = formatC(estado, width = 2,flag = "0"),
         seccion = formatC(seccion, width = 4,flag = "0"),
         id_casilla = formatC(id_casilla, width = 2,flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2,flag = "0"),
         distritof = formatC(distritof, width = 2, flag = "0"),
         seccion = if_else(tipo_casilla == "P","9999",seccion),
         mr_rp = if_else(tipo_acta == 2, "MR", ""),
         mr_rp = if_else(tipo_acta == 3, "MR",mr_rp),
         mr_rp = if_else(tipo_casilla == "S" & tipo_acta == 4, "RP",mr_rp))


df15 <- df15 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# revisar nombres de partidos

detectar_partidos(df15)


# sufijo para join

final_df15 <- insertar_sufijo(bd=df15, "df", "15")

# guardar rda
nac_df_15 <- final_df15

nac_df_15 %>% write_rds("inst/electoral/nacional/df_15.rda")

rm(df15)


## DIPFED 12 --------------------------------------------------------------------------


# Identificar nombres de variables y nombres de partidos

df12 <- bd_df_12

df12[df12 == "NA"] <- NA


colnames(df12)

# Homogenizar nombres de variables partidos

df12 <- df12  %>%
  select(clave_casilla:orden,
         municipio,
         pan:pt_mc,
         no_reg,
         nulos,
         lista_nominal,
         votos_reservados,
         id_grupo,
         tipo_recuento) %>%
  rename("noreg" = no_reg,
         distritof = distrito,
         nominal = lista_nominal) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         estado = formatC(estado, width = 2,flag = "0"),
         seccion = formatC(seccion, width = 4,flag = "0"),
         id_casilla = formatC(id_casilla, width = 2,flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2,flag = "0"),
         distritof = formatC(distritof, width = 2, flag = "0"),
         seccion = if_else(tipo_casilla == "P","9999",seccion),
         mr_rp = if_else(tipo_candidatura == 2, "MR", "RP"))



df12 <- df12 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# revisar nombres de partidos

detectar_partidos(df12)

# Agregar municipios del año

# municipios_df_12 <- read_excel("data-raw/DatosAbiertos-derfe-pdln_edms_sexo_20120415.xlsx") %>%
#   janitor::clean_names() %>%
#   select("estado" = clave_entidad,
#          seccion,
#          "municipio_df_12" = clave_municipio,
#          "nombre_municipio_df_12" = nombre_municipio) %>%
#   mutate(seccion = formatC(seccion, width = 4,flag = "0")) %>%
#   unique()
#
# df12 <- df12 %>%
#   left_join(municipios_df_12)


# sufijo para join

final_df12 <- insertar_sufijo(bd=df12, "df", "12")

# guardar rda
nac_df_12 <- final_df12

nac_df_12 %>% write_rds("inst/electoral/nacional/df_12.rda")

rm(df12)





# Cambiar id_estado por estado --------------------------------------------
library(tidyverse)

df_18 <- read_rds("inst/electoral/nacional/df_18.rda")
df_18 %>% rename(estado = id_estado) %>% saveRDS("inst/electoral/nacional/df_18.rda")

# PR - 06 -----------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/NAC/PRESIDENCIA_2006/2006_SEE_PRE_NAL_CAS.csv"
pr_06 <- read_csv(path) |>
  janitor::clean_names() |>
  select(-c(circunscripcion, id_distrito:municipio)) |>
  rename(estado = id_estado,
         panal = nva_alianza,
         noreg = num_votos_can_nreg,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal) |>
  mutate(estado = sprintf("%02s", estado),
         seccion = sprintf("%04s", seccion),
         casilla = if_else(casilla == "B", "B01",casilla),
         id_casilla = case_when(nchar(casilla) >= 4 ~ stringr::str_extract_all(casilla,"(?<=E)[^C]*?(\\d+)(?=C)"),
                                T ~ stringr::str_extract_all(casilla,"(?<=[a-zA-Z])(\\d+)")),
         tipo_casilla = substr(casilla, 1, 1),
         ext_contigua = if_else(nchar(casilla) >= 4, stringr::str_extract_all(casilla,"(?<=C)(\\d+)"), list("0")),
         clave_casilla = glue::glue("{estado}{stringr::str_pad(seccion,pad = '0', width = 4)}{tipo_casilla}{stringr::str_pad(id_casilla,pad = '0', width = 2)}{stringr::str_pad(ext_contigua,pad = '0', width = 2)}")) |>
  rename_with(~paste("ele", .x, "pr", "06", sep = "_"),.cols = c(pan:nominal)) |>
  relocate(clave_casilla, .after = seccion) |>
  relocate(casilla, .after = ruta_acta)

write_rds(pr_06, "inst/electoral/nacional/pr_06.rda")

# PR - 12 -----------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/3_Insumos/Externas/Limpieza/PEL/NAC/PRESIDENCIA_2012/2012_SEE_PRE_NAL_CAS.csv"
pr_12 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(!grepl("M", casilla)) |>
  select(-c(circunscripcion, id_distrito:municipio)) |>
  rename(estado = id_estado,
         panal = nva_alianza,
         noreg = num_votos_can_nreg,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal) |>
  mutate(estado = sprintf("%02s", estado),
         seccion = sprintf("%04s", seccion),
         casilla = if_else(casilla == "B", "B01",casilla),
         id_casilla = case_when(nchar(casilla) >= 4 ~ stringr::str_extract_all(casilla,"(?<=E)[^C]*?(\\d+)(?=C)"),
                                T ~ stringr::str_extract_all(casilla,"(?<=[a-zA-Z])(\\d+)")),
         tipo_casilla = substr(casilla, 1, 1),
         ext_contigua = if_else(nchar(casilla) >= 4, stringr::str_extract_all(casilla,"(?<=C)(\\d+)"), list("0")),
         clave_casilla = glue::glue("{estado}{stringr::str_pad(seccion,pad = '0', width = 4)}{tipo_casilla}{stringr::str_pad(id_casilla,pad = '0', width = 2)}{stringr::str_pad(ext_contigua,pad = '0', width = 2)}")) |>
  rename_with(~paste("ele", .x, "pr", "12", sep = "_"),.cols = c(pan:nominal)) |>
  relocate(clave_casilla, .after = seccion) |>
  relocate(casilla, .after = ruta_acta)

write_rds(pr_12, "inst/electoral/nacional/pr_12.rda")

# PR 24 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/computos/pr_24/PRES_2024.csv"
estado <- "nacional"
eleccion <- "pr_24"

aux <- read_delim(path, delim = "|", skip = 7, locale = locale(encoding = "ISO-8859-1")) |>
  janitor::clean_names() |>
  mutate(clave_casilla = gsub("'", "", clave_casilla),
         across(c(contains("id_"), seccion), readr::parse_number)) |>
  rename(
    noreg = candidato_a_no_registrado_a,
    # panal = nva_alianza,
    # pes = es,
    nulos = votos_nulos,
    total = total_votos_calculados,
    nominal = lista_nominal,
    nombre_estado = entidad,
    !!glue::glue("distritof_{readr::parse_number(eleccion)}") := id_distrito_federal,
    !!glue::glue("nombre_distritof_{readr::parse_number(eleccion)}") := distrito_federal
  ) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  mutate(estado = sprintf("%02s", id_entidad),
    distritof_24 = sprintf("%02s", distritof_24),
         seccion = sprintf("%04s", seccion),
    across(contains("ele_"), as.numeric)) |>
  relocate(clave_casilla, .after = seccion) |>
  select(-c(id_entidad, clave_acta))

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

# SEN 24 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/computos/sen_24/SEN_2024.csv"
estado <- "nacional"
eleccion <- "sen_24"

aux <- read_delim(path, delim = "|", skip = 7, locale = locale(encoding = "ISO-8859-1")) |>
  janitor::clean_names() |>
  mutate(clave_casilla = gsub("'", "", clave_casilla),
         across(c(contains("id_"), seccion), readr::parse_number)) |>
  rename(
    noreg = candidato_a_no_registrado_a,
    # panal = nva_alianza,
    # pes = es,
    nombre_estado = entidad,
    nulos = votos_nulos,
    total = total_votos_calculados,
    nominal = lista_nominal,
    !!glue::glue("distritof_{readr::parse_number(eleccion)}") := id_distrito_federal,
    !!glue::glue("nombre_distritof_{readr::parse_number(eleccion)}") := distrito_federal
  ) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  mutate(estado = sprintf("%02s", id_entidad),
         distritof_24 = sprintf("%02s", distritof_24),
         seccion = sprintf("%04s", seccion),
         across(contains("ele_"), as.numeric)) |>
  relocate(clave_casilla, .after = seccion) |>
  select(-c(id_entidad, clave_acta))

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

# DF 24 -------------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/computos/df_24/DIP_FED_2024.csv"
estado <- "nacional"
eleccion <- "df_24"

aux <- read_delim(path, delim = "|", skip = 7, locale = locale(encoding = "ISO-8859-1")) |>
  janitor::clean_names() |>
  mutate(clave_casilla = gsub("'", "", clave_casilla),
         across(c(contains("id_"), seccion), readr::parse_number)) |>
  rename(
    noreg = candidato_a_no_registrado_a,
    ind1 = candidato_a_independiente,
    # panal = nva_alianza,
    # pes = es,
    nulos = votos_nulos,
    total = total_votos_calculados,
    nominal = lista_nominal,
    nombre_estado = entidad,
    !!glue::glue("distritof_{readr::parse_number(eleccion)}") := id_distrito_federal,
    !!glue::glue("nombre_distritof_{readr::parse_number(eleccion)}") := distrito_federal
  ) |>
  rename_with(~paste("ele", .x, eleccion, sep = "_"), .cols = c(pan:nominal)) |>
  mutate(estado = sprintf("%02s", id_entidad),
         distritof_24 = sprintf("%02s", distritof_24),
         seccion = sprintf("%04s", seccion),
         across(contains("ele_"), as.numeric)) |>
  relocate(clave_casilla, .after = seccion) |>
  select(-c(id_entidad, clave_acta))

aux |>
  count(nchar(clave_casilla))

glimpse(aux)
write_rds(aux, file = glue::glue("inst/electoral/{estado}/{eleccion}.rda"))

