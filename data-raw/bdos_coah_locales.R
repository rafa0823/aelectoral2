# COAHUILALOCALES

## CARGAR BASES ------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

bd_pm_21_coah <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2021/Municipio/coahuila_normal_casilla.csv") %>%
  janitor::clean_names()

bd_dl_20_coah <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2020/Distrito local/coahuila_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

bd_pm_18_coah <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Municipio/coahuila_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

bd_gb_17_coah <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2017/Gobernador/coahuila_normal_seccion.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

bd_pm_17_coah <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2017/Municipal/coahuila_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

bd_dl_17_coah <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2017/Distrito local/coahuila_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()


## PM 21 COAHUILA ------------------------------------------------------------------------------------------------------

pm21 <- bd_pm_21_coah   %>%
  mutate(nom_mun = gsub(pattern = "( |)[0-9]",replacement = "",x = nom_mun))

# revisar nombres de varianles

colnames(pm21)

pm21 <- pm21 %>%
  rename(nombre_municipio_21 = nom_mun,
         municipio_21 = municipio,
         distritof_21 = dtto_f,
         distritol_21 = dtto_loc,
         total = total_votos,
         nominal = listado_nominal,
         noreg = cand_nreg) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_21 = formatC(municipio_21, width = 3, flag = "0"),
         distritof_21 = formatC(distritof_21, width = 2, flag = "0"),
         distritol_21 = formatC(distritol_21, width = 2, flag = "0"))

# HOMOLOGAR EXTRANJERO
## agregar claves 00 voto extranjero y nombre de estado si es necesario

pm21 <- pm21 %>%
  mutate(distritof_21 = if_else(seccion == "000", "00", distritof_21),
         distritol_21 = if_else(seccion == "000", "00", distritol_21))




pm21 <- pm21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm21)

# sufijo para join

final_pm21_coah <- insertar_sufijo(bd=pm21, "pm", "21")

final_pm21_coah <- final_pm21_coah  %>%
  mutate(clave_casilla = case_when(nchar(tipo) == 2 ~ paste0(gsub(pattern = "[[:alpha:]]","0", tipo),"00"),
                                   nchar(tipo) == 3 ~ paste0(gsub(pattern = "[[:alpha:]]","", tipo),"00"),
                                   nchar(tipo) == 4 ~ gsub(pattern = "[[:alpha:]]","0", tipo),
                                   nchar(tipo) == 5 ~ gsub(pattern = "E1C","01", tipo)),


         estado = "10",
         nombre_estado = "DURANGO",
         tipo_casilla = substr(tipo,1,1),
         clave_casilla = paste0(estado,seccion,tipo_casilla,clave_casilla))

final_pm21_coah %>% count(tipo) %>% view()


# guardar rda

coah_pm_21 <- final_pm21_coah

coah_pm_21 %>% write_rds("inst/electoral/coah_pm_21.rda")

rm(pm21)


## gb 16 DURANGO  ------------------------------------------------------------------------------------------------------

gb16 <- bd_gb_16_coah   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio),
         pan = 0)

# revisar nombres de varianles

colnames(gb16)

gb16 <- gb16 %>%
  rename("noreg"= no_reg,
         "municipio_16" = municipio,
         "nombre_municipio_16" = nombre_municipio,
         distritol_16 = distrito,
         nombre_distritol_16 = cabecera_distrital,
         pes = es
  )%>%
  mutate(across(pan_prd:pan, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_16 = formatC(municipio_16, width = 3, flag = "0"),
         distritol_16 = formatC(distritol_16, width = 2, flag = "0"))


gb16 <- gb16 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan_prd:pan)

# Identificar los partidos de la elecccion
detectar_partidos(gb16)

# sufijo para join

final_gb16_coah <- insertar_sufijo(bd=gb16, "gb", "16")

final_gb16_coah <- final_gb16_coah  %>%
  mutate(casilla = formatC(casilla, width = 2,flag = "0"),
         tipo_casilla = "X",
         estado = "10",
         nombre_estado = "DURANGO",
         clave_casilla = paste0(estado,seccion,tipo_casilla,casilla,"XX"))


# guardar rda

coah_gb_16 <- final_gb16_coah

coah_gb_16 %>% write_rds("inst/electoral/coah_gb_16.rda")

rm(gb16)

## PM 16 DURANGO ------------------------------------------------------------------------------------------------------

pm16 <- bd_pm_16_coah   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio),
         pan = 0)


# revisar nombres de varianles

colnames(pm16)

pm16 <- pm16 %>%
  rename("noreg"= no_reg,
         "municipio_16" = municipio,
         "nombre_municipio_16" = nombre_municipio,
         distritol_16 = distrito,
         nombre_distritol_16 = nombre_distrito,
  )%>%
  mutate(across(pan_prd:pan, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_16 = formatC(municipio_16, width = 3, flag = "0"),
         distritol_16 = formatC(distritol_16, width = 2, flag = "0"))


pm16 <- pm16 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan_prd:pan)

# Identificar los partidos de la elecccion
detectar_partidos(pm16)

# sufijo para join

final_pm16_coah <- insertar_sufijo(bd=pm16, "pm", "16")

final_pm16_coah <- final_pm16_coah  %>%
  mutate(clave_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                   nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                   nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                   nchar(casilla) == 5 ~ paste0(gsub(pattern = "[MR]","",casilla),"00")),
         mr_rp = if_else(nchar(casilla) == 5, gsub(pattern = "[[:digit:]]","",casilla),""),
         mr_rp = gsub(pattern = "S","",mr_rp),
         mr_rp = if_else(mr_rp == "","MR",mr_rp),
         estado = "10",
         nombre_estado = "DURANGO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,clave_casilla))


# guardar rda

coah_pm_16 <- final_pm16_coah

coah_pm_16 %>% write_rds("inst/electoral/coah_pm_16.rda")

rm(pm16)
