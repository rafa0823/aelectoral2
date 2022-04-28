
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


# PM 21 EDOMEX ------------------------------------------------------------------

pm21 <- bd_pm_21_mex   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))

# revisar nombres de varianles

colnames(pm21)

pm21 <- pm21 %>%
  rename("noreg"="no_reg",
         "municipio_pm_21" = municipio,
         "nombre_municipio_pm_21" = nombre_municipio)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_pm_21 = formatC(municipio_pm_21, width = 2, flag = "0"))


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
         clave_casilla = paste0(estado,seccion,clave_casilla))


# guardar rda

mex_pm_21 <- final_pm21_mex

mex_pm_21 %>% write_rds("inst/electoral/mex_pm_21.rda")

rm(pm21)

# PM EXTRAORDINARIAS 21 ---------------------------------------------------------------------------

pmext21 <- bd_pmext_21_mex   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))

# revisar nombres de varianles

colnames(pmext21)

pmext21 <- pmext21 %>%
  rename("noreg"=no_registrados,
         "seccion" = id_seccion,
         "distritol_21" = id_distrito,
         "nombre_distritol_21" = distrito,
         "municipio_pm_21" = id_municipio,
         "nombre_municipio_pm_21" = municipio,
         "panal" = naem,
         "nominal" = lista_nominal)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_pm_21 = formatC(municipio_pm_21, width = 2, flag = "0"))


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
         clave_casilla = paste0(estado,seccion,clave_casilla))


# guardar rda

mex_pmext_21 <- final_pmext21_mex

mex_pmext_21 %>% write_rds("inst/electoral/mex_pmext_21.rda")

rm(pmext21)



## PM 18 EDOMEX -------------------------------------------------------------------------------------

pm18 <- bd_pm_18_mex   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))%>%
  mutate(seccion = formatC(seccion, width = 4,flag = "0"))

# revisar nombres de varianles

colnames(pm18)

pm18 <- pm18 %>%
  rename("noreg"=no_reg,
         "municipio_pm_18" = municipio,
         "nombre_municipio_pm_18" = nombre_municipio,
         "distritol_18" = distrito)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         distritol_18 = formatC(distritol_18, width = 2, flag = "0"),
         municipio_pm_18 = formatC(municipio_pm_18, width = 2, flag = "0"),
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

mex_pm_18 %>% write_rds("inst/electoral/mex_pm_18.rda")

rm(pm18)

## LOCALES 21 EDOMEX -----------------------------------------------------------

dl21 <- bd_dl_21_mex  %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))  %>%
  rename("noreg"=no_registrados,
         "nominal" = lista_nominal,
         "municipio_dl_21" = id_municipio,
         "nombre_municipio_dl_21" = municipio,
         "seccion" = id_seccion,
         "distritol_21"=id_distrito,
         "nombre_distritol_21" = distrito,
         "panal" = naem,
         "pt_morena_panal" = pt_morena_naem,
         "pt_morena_panal_cc" = pt_morena_naem_cc,
         "pt_panal" = pt_naem,
         "morena_panal" = morena_naem) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         distritol_21 = formatC(distritol_21, width = 2, flag = "0"),
         municipio_dl_21 = formatC(municipio_dl_21, width = 2, flag = "0"),
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
         mr_rp = if_else(nchar(casilla) == 5, gsub(pattern = "[[:digit:]]","",casilla),""),
         mr_rp = gsub(pattern = "S","",mr_rp),
         estado = "15",
         nombre_estado = "MÉXICO",
         clave_casilla = paste0(estado,seccion,clave_casilla))



# guardar rda

mex_dl_21 <- final_dl21_mex

mex_dl_21 %>% write_rds("inst/electoral/mex_dl_21.rda")

rm(dl21)


## DL 18 EDOMEX -------------------------------------------------------------------------------------

dl18 <- bd_dl_18_mex   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))

# revisar nombres de varianles

colnames(dl18)

dl18 <- dl18 %>%
  rename("noreg"=no_reg,
         "municipio_dl_18" = municipio,
         "nombre_municipio_dl_18" = nombre_municipio,
         "distritol_18" = distrito)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         distritol_18 = formatC(distritol_18, width = 2, flag = "0"),
         municipio_dl_18 = formatC(municipio_dl_18, width = 2, flag = "0"),
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

mex_dl_18 %>% write_rds("inst/electoral/mex_dl_18.rda")


rm(dl18)

## GOBERNADOR 17 EDOMEX ----------------------------------------------------------------------------



gb17 <- bd_gb_17_mex   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))%>%
  mutate(seccion = formatC(seccion, width = 4,flag = "0"))

# revisar nombres de varianles

colnames(gb17)

gb17 <- gb17 %>%
  rename("noreg"=no_reg,
         "municipio_gb_17" = municipio,
         "nombre_municipio_gb_17" = nombre_municipio,
         "distritol_17" = distrito)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         distritol_17 = formatC(distritol_17, width = 2, flag = "0"),
         municipio_gb_17 = formatC(municipio_gb_17, width = 2, flag = "0"),
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

mex_gb_17 %>% write_rds("inst/electoral/mex_gb_17.rda")

rm(gb17)





