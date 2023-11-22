
# EDOMEX LOCALES

# Cargar bases --------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

bd_pm_21_mex <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Estado de México/Casillas_computo_municipio_por_partido_politico_2021.xlsx",
                           n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()

bd_pmext_21_mex <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2021/Municipio/edomex_extraordinaria_casilla.csv",
                            skip = 3,
                            n_max = 47)%>%
  janitor::clean_names() %>%
  as_tibble()

bd_pm_18_mex <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Municipio/edomex_normal_casilla.csv")%>%
  janitor::clean_names() %>%
  as_tibble()

bd_dl_18_mex <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Distrito local/edomex_normal_casilla.csv")%>%
  janitor::clean_names() %>%
  as_tibble()

bd_dl_21_mex <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2021/Distrito local/edomex_normal_casilla.xlsx",
                           skip = 4,
                           n_max = 20035) %>%
  janitor::clean_names() %>%
  as_tibble()%>%
  rename("pt_morena_naem" = pt_morena_naem_22,
         "pt_morena_naem_cc" = pt_morena_naem_26)

bd_gb_17_mex <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2017/Gobernador/edomex_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

# pm 15
bd_pm_15_mex <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2015/Municipio/edomex_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

# dl 15
bd_dl_15_mex <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2015/Distrito local/edomex_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()


# PM 21 EDOMEX ------------------------------------------------------------------

pm21 <- bd_pm_21_mex   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))

# revisar nombres de varianles

colnames(pm21)

pm21 <- pm21 %>%
  rename("noreg"="no_reg",
         "municipio" = municipio,
         "nombre_municipio" = nombre_municipio)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"))


pm21 <- pm21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm21)

# sufijo para join

final_pm21_mex <- insertar_sufijo(bd=pm21, "pm", "21") %>%
  rename("ele_pt_morena_panal_cc_pm_21" = ele_pt_morena_panal_2_pm_21)

final_pm21_mex <- final_pm21_mex %>%
  mutate(clave_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                   nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                   nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                   nchar(casilla) == 2 ~ paste0(gsub(pattern = "S", "S0",casilla), "00")))%>%
  mutate(estado = "15",
         nombre_estado = "MÉXICO",
         clave_casilla = paste0(estado,seccion,clave_casilla),
         tipo_eleccion = "ORDINARIA")


# guardar rda

final_pm21_mex


rm(pm21)

# PM EXTRAORDINARIAS 21 ---------------------------------------------------------------------------

pmext21 <- bd_pmext_21_mex   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))

# revisar nombres de varianles

colnames(pmext21)

pmext21 <- pmext21 %>%
  rename("noreg"=no_registrados,
         "seccion" = id_seccion,
         "distritol" = id_distrito,
         "nombre_distritol" = distrito,
         "municipio" = id_municipio,
         "nombre_municipio" = municipio,
         "panal" = naem,
         "nominal" = lista_nominal)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"))


pmext21 <- pmext21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pmext21)

# sufijo para join

final_pmext21_mex <- insertar_sufijo(bd=pmext21, "pm", "21")

final_pmext21_mex <- final_pmext21_mex %>%
  mutate(clave_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                   nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                   nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                   nchar(casilla) == 2 ~ paste0(gsub(pattern = "S", "S0",casilla), "00"))) %>%
  mutate(estado = "15",
         nombre_estado = "MÉXICO",
         clave_casilla = paste0(estado,seccion,clave_casilla),
         tipo_eleccion = "EXTRAORDINARIA")


# guardar rda

final_pmext21_mex


rm(pmext21)

# BIND PM 21 Y PMEXT21 ----------------------------------------------------------------------------------------

mex_pm_21 <- final_pm21_mex %>% anti_join(final_pmext21_mex, by = "seccion")


mex_pm_21 <- mex_pm_21 %>% rbind(final_pmext21_mex,fill = T) %>% select(!c(distritol,nombre_distritol,ele_votos_validos_pm_21))

mex_pm_21 %>% write_rds("inst/electoral/mex/pm_21.rda")

## PM 18 EDOMEX -------------------------------------------------------------------------------------

pm18 <- bd_pm_18_mex   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))%>%
  mutate(seccion = formatC(seccion, width = 4,flag = "0"))

# revisar nombres de varianles

colnames(pm18)

pm18 <- pm18 %>%
  rename("noreg"=no_reg,
         "municipio" = municipio,
         "nombre_municipio" = nombre_municipio,
         "distritol" = distrito)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         distritol = formatC(distritol, width = 2, flag = "0"),
         municipio = formatC(municipio, width = 3, flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion))


pm18 <- pm18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm18)

# sufijo para join

final_pm18_mex <- insertar_sufijo(bd=pm18, "pm", "18")

# agregar clave casilla

final_pm18_mex <-  final_pm18_mex %>%
  mutate(clave_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                   nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                   nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                   nchar(casilla) == 2 ~ paste0(gsub(pattern = "S", "S0",casilla), "00")),
         estado = "15",
         nombre_estado = "MÉXICO",
         clave_casilla = paste0(estado,seccion,clave_casilla))%>%
  mutate(seccion = formatC(seccion, width = 4,flag = "0"))



# guardar rda

mex_pm_18 <- final_pm18_mex

mex_pm_18 %>% write_rds("inst/electoral/mex/pm_18.rda")

rm(pm18)

## LOCALES 21 EDOMEX -----------------------------------------------------------

dl21 <- bd_dl_21_mex  %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))  %>%
  rename("noreg"=no_registrados,
         "nominal" = lista_nominal,
         "municipio" = id_municipio,
         "nombre_municipio" = municipio,
         "seccion" = id_seccion,
         "distritol"=id_distrito,
         "nombre_distritol" = distrito,
         "panal" = naem,
         "pt_morena_panal" = pt_morena_naem,
         "pt_morena_panal_cc" = pt_morena_naem_cc,
         "pt_panal" = pt_naem,
         "morena_panal" = morena_naem) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         distritol = formatC(distritol, width = 2, flag = "0"),
         municipio = formatC(municipio, width = 3, flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion))



dl21 <- dl21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(dl21)

# sufijo para join

final_dl21_mex <- insertar_sufijo(bd=dl21, "dl", "21")

# agregar clave casilla

final_dl21_mex <-  final_dl21_mex %>%
  mutate(clave_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                   nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                   nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                   nchar(casilla) == 2 ~ paste0(gsub(pattern = "S", "S0",casilla), "00")),
         mr_rp = if_else(casilla == "S2", "RP","MR"),
         estado = "15",
         nombre_estado = "MÉXICO",
         clave_casilla = paste0(estado,seccion,clave_casilla))



# guardar rda

mex_dl_21 <- final_dl21_mex

mex_dl_21 %>% write_rds("inst/electoral/mex/dl_21.rda")

rm(dl21)


## DL 18 EDOMEX -------------------------------------------------------------------------------------

dl18 <- bd_dl_18_mex   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))

# revisar nombres de varianles

colnames(dl18)

dl18 <- dl18 %>%
  rename("noreg"=no_reg,
         "municipio" = municipio,
         "nombre_municipio" = nombre_municipio,
         "distritol" = distrito)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         distritol = formatC(distritol, width = 2, flag = "0"),
         municipio = formatC(municipio, width = 3, flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion))


dl18 <- dl18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(dl18)

# sufijo para join

final_dl18_mex <- insertar_sufijo(bd=dl18, "dl", "18")

# agregar clave casilla

final_dl18_mex <-  final_dl18_mex %>%
  mutate(clave_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                   nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                   nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                   nchar(casilla) == 5 ~ paste0(gsub(pattern = "[MR]","",casilla),"00")),
         mr_rp = if_else(nchar(casilla) == 5, gsub(pattern = "[[:digit:]]","",casilla),""),
         mr_rp = gsub(pattern = "S","",mr_rp),
         mr_rp = if_else(mr_rp == "","MR",mr_rp),
         estado = "15",
         nombre_estado = "MÉXICO",
         clave_casilla = paste0(estado,seccion,clave_casilla))


# guardar rda

mex_dl_18 <- final_dl18_mex

mex_dl_18 %>% write_rds("inst/electoral/mex/dl_18.rda")


rm(dl18)

## GOBERNADOR 17 EDOMEX ----------------------------------------------------------------------------



gb17 <- bd_gb_17_mex   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))%>%
  mutate(seccion = formatC(seccion, width = 4,flag = "0"))

# revisar nombres de varianles

colnames(gb17)

gb17 <- gb17 %>%
  rename("noreg"=no_reg,
         "municipio" = municipio,
         "nombre_municipio" = nombre_municipio,
         "distritol" = distrito)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         distritol = formatC(distritol, width = 2, flag = "0"),
         municipio = formatC(municipio, width = 3, flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion))


gb17 <- gb17 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(gb17)

# sufijo para join

final_gb17_mex <- insertar_sufijo(bd=gb17, "gb", "17")

# agregar clave casilla

final_gb17_mex <-  final_gb17_mex %>%
  mutate(clave_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                   nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                   nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                   nchar(casilla) == 2 ~ paste0(gsub(pattern = "S", "S0",casilla), "00")),
         estado = "15",
         nombre_estado = "MÉXICO",
         clave_casilla = paste0(estado,seccion,clave_casilla))


# guardar rda

mex_gb_17 <- final_gb17_mex

mex_gb_17 %>% write_rds("inst/electoral/mex/gb_17.rda")

rm(gb17)

## PM 15 EDOMEX ------------


bd_pm_15_mex %>% colnames

pm15 <- bd_pm_15_mex   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))

# revisar nombres de varianles

colnames(pm15)

pm15 <- pm15 %>%
  rename(noreg = no_reg,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         municipio = municipio,
         nombre_municipio = nombre_municipio
  )%>%
  rename_with( ~ gsub("independiente_", "independiente", .x, fixed = TRUE)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritol = formatC(distritol, width = 3, flag = "0"))


pm15 <- pm15 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm15)

# sufijo para join

final_pm15_mex <- insertar_sufijo(bd=pm15, "pm", "15")

#agregar clave casillas


final_pm15_mex <- final_pm15_mex  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "15",
         nombre_estado = "MÉXICO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_pm15_mex %>% count(nchar(clave_casilla))
final_pm15_mex %>% count(id_casilla)

# guardar rda

mex_pm_15 <- final_pm15_mex

mex_pm_15 %>% write_rds("inst/electoral/mex/pm_15.rda")

rm(pm15)

## dl 15 EDOMEX ------------


bd_dl_15_mex %>% colnames

dl15 <- bd_dl_15_mex   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))

# revisar nombres de varianles

colnames(dl15)

dl15 <- dl15 %>%
  rename(noreg = no_reg,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         municipio = municipio,
         nombre_municipio = nombre_municipio
  )%>%
  rename_with( ~ gsub("independiente_", "independiente", .x, fixed = TRUE)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritol = formatC(distritol, width = 3, flag = "0"))


dl15 <- dl15 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(dl15)

# sufijo para join

final_dl15_mex <- insertar_sufijo(bd=dl15, "dl", "15")

#agregar clave casillas


final_dl15_mex <- final_dl15_mex  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "15",
         nombre_estado = "MÉXICO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_dl15_mex %>% count(nchar(clave_casilla))
final_dl15_mex %>% count(id_casilla)

# guardar rda

mex_dl_15 <- final_dl15_mex

mex_dl_15 %>% write_rds("inst/electoral/mex/dl_15.rda")

rm(dl15)


# GB_13 -------------------------------------------------------------------
library(tidyverse)
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2023/Gobernador/mexico_normal_casilla.csv"
path <- "~/Downloads/mexico_normal_casilla.csv"
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

# Procesamiento -----------------------------------------------------------
gb_23 <- read_csv(path) |>
  janitor::clean_names() |>
  rename(
    estado = id_estado,
    distritol_23 = id_distrito_local,
    delfina = pvem_pt_morena,
    nombre_distritol_23 = cabecera_distrital_local,
    nombre_municipio_23 = municipio,
    municipio_23 = id_municipio,
    validos = num_votos_validos,
    noreg = num_votos_can_nreg,
    nulos = num_votos_nulos,
    total = total_votos,
    nominal = lista_nominal
  ) |>
  rename_with(~gsub("naem", "panal", .x), contains("naem")) |>
  mutate(
    estado = as.character(estado),
    distritol_23 = sprintf("%03s", distritol_23),
    municipio_23 = sprintf("%03s", municipio_23),
    seccion = sprintf("%04s", seccion),
    seccion = if_else(grepl("V", casilla), "9999", seccion)
  ) |>
  homologar_bd(estado = "15", nombre_estado = "MEXICO") |>
  select(-c(circunscripcion, estatus_acta:ruta_acta)) |>
  relocate(clave_casilla, .after = seccion) |>
  rename_with(~paste("ele", .x, "gb_23", sep = "_"), pan:nominal)

gb_23 |>
  count(nchar(clave_casilla))

naniar::vis_miss(gb_23)

write_rds(gb_23, "inst/electoral/mex/gb_23.rda")

# Correcciones a bds ------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/PEL/MEX/2015/DIPUTACIONES_LOC_MR_csv/2015_SEE_DIP_LOC_MR_MEX_SEC.csv"
nombres <- read_csv("~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/PEL/MEX/2015/DIPUTACIONES_LOC_MR_csv/2015_SEE_DIP_LOC_MR_MEX_MUN.csv") |>
  janitor::clean_names() |>
  distinct(municipio_15 = sprintf("%03s", id_municipio),
           nombre_municipio_15 = municipio)

nombres_dl <- read_csv("~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/PEL/MEX/") |>
  janitor::clean_names() |>
  distinct(municipio_15 = sprintf("%03s", id_municipio),
           nombre_municipio_15 = municipio)

clave_15 <- read_csv(path) |>
  janitor::clean_names() |>
  distinct(seccion = sprintf("%04s", seccion),
           municipio_15 = sprintf("%03s", id_municipio)) |>
  left_join(nombres)

clave_15 |>
  filter(seccion == "4266")

#DL 15
read_rds("inst/electoral/mex/dl_15.rda") |>
  filter(tepjf != "Anulada" | is.na(tepjf)) |>
  select(-municipio) |>
  rename(distritol_15 = distritol,
         nombre_distritol_15 = nombre_distritol,
         nombre_municipio_15 = nombre_municipio,
  ) |>
  rename_with(~gsub("independiente", "ind", .x), contains("independiente")) |>
  left_join(clave_15) |>
  write_rds("inst/electoral/mex/dl_15.rda")

#PM_15
read_rds("inst/electoral/mex/pm_15.rda") |>
  as_tibble() |>
  filter(tepjf != "Anulada" | is.na(tepjf)) |>
  select(-municipio, -nombre_municipio) |>
  rename(distritol_15 = distritol,
         nombre_distritol_15 = nombre_distritol,
  ) |>
  rename_with(~gsub("independiente", "ind", .x), contains("independiente")) |>
  left_join(clave_15) |>
  write_rds("inst/electoral/mex/pm_15.rda")

# GB 17

read_rds("inst/electoral/mex/gb_17.rda") |>
  as_tibble() |>
  rename(nombre_distritol_17 = nombre_distrito) |>
  mutate(distritol_17 = sprintf("%03s", distritol_17)) |>
  rename_with(~gsub("independiente_", "ind", .x), contains("independiente")) |>
  write_rds("inst/electoral/mex/gb_17.rda")

#DL 18
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/PEL/MEX/2018/DIPUTACIONES_LOC_MR_csv/2018_SEE_DIP_LOC_MR_MEX_DISCAND.csv"
clave_18 <- read_csv(path) |>
  janitor::clean_names() |>
  distinct(distritol_18 = sprintf("%03s", id_distrito_local),
           nombre_distritol_18 = cabecera_distrital_local)

read_rds("inst/electoral/mex/dl_18.rda") |>
  as_tibble() |>
  rename(nombre_municipio_18 = nombre_municipio,
         distritol_18 = distritol,
         municipio_18 = municipio) |>
  mutate(distritol_18 = sprintf("%03s", distritol_18)) |>
  left_join(clave_18) |>
  rename_with(~gsub("independiente_", "ind", .x), contains("independiente")) |>
  write_rds("inst/electoral/mex/dl_18.rda")

## PM_18

read_rds("inst/electoral/mex/pm_18.rda") |>
  as_tibble() |>
  rename(nombre_municipio_18 = nombre_municipio,
         distritol_18 = distritol,
         municipio_18 = municipio) |>
  mutate(distritol_18 = sprintf("%03s", distritol_18)) |>
  left_join(clave_18) |>
  rename_with(~gsub("independiente_", "ind", .x), contains("independiente")) |>
  write_rds("inst/electoral/mex/pm_18.rda")

## DL_21
read_rds("inst/electoral/mex/dl_21.rda") |>
  as_tibble() |>
  rename(distritol_21 = distritol,
         nombre_distritol_21 = nombre_distritol,
         municipio_21 = municipio,
         nombre_municipio_21 = nombre_municipio,
         ele_validos_dl_21 = ele_votos_validos_dl_21) |>
  mutate(distritol_21 = sprintf("%03s", distritol_21)) |>
  write_rds("inst/electoral/mex/dl_21.rda")

## PM_21
clave_21 <- read_csv("~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/PEL/MEX/2021/AYUNTAMIENTOS_csv/2021_SEE_AYUN_MEX_CAS.csv") |>
  janitor::clean_names() |>
  distinct(distritol_21 = sprintf("%03s", id_distrito_local),
           nombre_distritol_21 = cabecera_distrital_local,
           seccion = sprintf("%04s", seccion))

read_rds("inst/electoral/mex/pm_21.rda") |>
  as_tibble() |>
  rename(municipio_21 = municipio,
         nombre_municipio_21 = nombre_municipio) |>
  left_join(clave_21) |>
  rename_with(~gsub("independiente_", "ind", .x), contains("independiente")) |>
  write_rds("inst/electoral/mex/pm_21.rda")
