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

bd_gb_17_coah <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2017/Gobernador/coah_normal_casilla.csv") %>%
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


         estado = "05",
         nombre_estado = "COAHUILA",
         tipo_casilla = substr(tipo,1,1),
         clave_casilla = paste0(estado,seccion,tipo_casilla,clave_casilla))

final_pm21_coah %>% count(tipo) %>% view()

final_pm21_coah %>% count(nchar(clave_casilla))


# guardar rda

coah_pm_21 <- final_pm21_coah

coah_pm_21 %>% write_rds("inst/electoral/coah_pm_21.rda")

rm(pm21)


## DL 20 COAHUILA  ------------------------------------------------------------------------------------------------------

dl20 <- bd_dl_20_coah

# revisar nombres de varianles

colnames(dl20)

dl20 <- dl20 %>%
  rename("noreg"= cand_nreg,
         total = total_votos,
         nominal = lista,
         "municipio_20" = municipio,
         nombre_municipio_20 = nom_mun,
         distritol_20 = dtto_loc,
         distritof_20 = dtto_f
  )%>%
  select(cve_cas2:total,nominal,clasificacion,bol_ent,urna_e) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_20 = formatC(municipio_20, width = 3, flag = "0"),
         distritol_20 = formatC(distritol_20, width = 2, flag = "0"),
         distritof_20 = formatC(distritof_20, width = 2, flag = "0"))


dl20 <- dl20 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(dl20)

# sufijo para join

final_dl20_coah <- insertar_sufijo(bd=dl20, "dl", "20")

final_dl20_coah <- final_dl20_coah  %>%
  mutate(tipo_casilla = substr(tipo,1,1),
         id_casilla = case_when(nchar(tipo) == 2 ~ paste0(gsub("[[:alpha:]]","0",tipo),"00"),
                                nchar(tipo) == 3 ~ paste0(gsub("[[:alpha:]]","",tipo),"00"),
                                nchar(tipo) == 4 ~ gsub("[[:alpha:]]","0",tipo),
                                nchar(tipo) == 5 ~ paste0("0",gsub("[[:alpha:]]","",tipo))),
         id_casilla = if_else(id_casilla == "100","0100",id_casilla),
         estado = "05",
         nombre_estado = "COAHUILA",
         clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla))


# guardar rda

coah_dl_20 <- final_dl20_coah

coah_dl_20 %>% write_rds("inst/electoral/coah_dl_20.rda")

rm(dl20)

