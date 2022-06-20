# TAMPS LOCALES

# Cargar bases --------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

#lista nominal 21

lista_nominal_21 <- read_delim("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/diputaciones.csv",
                               delim = "|", escape_double = FALSE, trim_ws = TRUE,  locale = locale(encoding = "CP1252"),
                               skip = 5) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  select(nombre_estado,clave_casilla, lista_nominal_casilla) %>%
  filter(nombre_estado == "TAMAULIPAS") %>%
  mutate(clave_casilla = gsub("[[:punct:]]","",clave_casilla))

# pm 21


bd_pm_21_tamps <- list.files("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2021/Municipio/Tamaulipas",
                             full.names = T) %>%
map_df(~{
    print(.x)
    read_csv(.x,skip = 6) %>% janitor::clean_names() %>%
      mutate(pan = as.numeric(pan))
  }) %>% filter(!is.na(casilla)) %>%
  select(casilla:pt_morena,miguel_rodriguez_salazar:monica_margot_de_leon,votos_nulos,candidatos_no_registrados)

# gb 16

bd_gb_16_tamps <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/tamaulipas_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

# dl 19

setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2019/Distrito local/tamps")


bd_dl_19_tamps <- list.files(full.names = T) %>%
  map_dfr(~{
    print(.x)
    read_csv(.x) %>%
      janitor::clean_names() %>%
      mutate(id_municipio = as.double(id_municipio),
             mr_rp = toupper(substr(gsub("./tamaulipas_normal_casilla_","",.x),1,2)))
  })


setwd("~/Documents/Git/aelectoral2")

# pm 18

bd_pm_18_tamps <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Municipio/tamaulipas_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

# pm 16


bd_pm_16_tamps <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Municipio/tamaulipas_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()


# dl 16


bd_dl_16_tamps <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Distrito local/tamaulipas_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()



## GB 16 TAMAULIPAS ------------------------------------------------------------------------------------------------------

gb16 <- bd_gb_16_tamps   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio)) %>%
  select(!validos)

# revisar nombres de varianles

colnames(gb16)

gb16 <- gb16 %>%
  rename(noreg = no_reg,
         distritol_16 = id_distrito,
         nombre_distritol_16 = cabecera_distrital,
         municipio_16 = id_municipio,
         nombre_municipio_16 = municipio
  )%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_16 = formatC(municipio_16, width = 3, flag = "0"),
         distritol_16 = formatC(distritol_16, width = 3, flag = "0"))


gb16 <- gb16 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion

detectar_partidos(gb16)

# sufijo para join

final_gb16_tamps <- insertar_sufijo(bd=gb16, "gb", "16")

#agregar clave casillas

final_gb16_tamps <- final_gb16_tamps  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                   nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                   nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                   nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "28",
         nombre_estado = "TAMAULIPAS",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))



# guardar rda


tamps_gb_16 <- final_gb16_tamps

tamps_gb_16 %>% write_rds("inst/electoral/tamps/gb_16.rda")

rm(gb16)

## PM 21 TAMAULIPAS ------------------------------------------------------------------------------------------------------

pm21 <- bd_pm_21_tamps

# revisar nombres de varianles

colnames(pm21)

pm21 <- pm21 %>%
  rename(noreg = candidatos_no_registrados,
         nulos = votos_nulos,
         miguelrodriguezsalazar = miguel_rodriguez_salazar,
         josemunozporras = jose_munoz_porras,
         elisapatriciaquintanilla = elisa_patricia_quintanilla,
         victormanuelvergara = victor_manuel_vergara,
         patriciogarzatapia = patricio_garza_tapia,
         julianalejandrocaraveo =  julian_alejandro_caraveo,
         monicamargotdeleon = monica_margot_de_leon,
         arnoldojavierrodriguez = arnoldo_javier_rodriguez,
         joseluisgallardo_flores = jose_luis_gallardo_flores ,
         hirampenagomez = hiram_pena_gomez,
         carloslaramacias = carlos_lara_macias,
         carlosalbertoguerrero = carlos_alberto_guerrero,
         marggidantoniorodriguez = marggid_antonio_rodriguez
  )%>%
  mutate(across(pan:noreg, ~as.numeric(.x)))


pm21 <- pm21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:noreg)

# Identificar los partidos de la elecccion

detectar_partidos(pm21)

# sufijo para join

final_pm21_tamps <- insertar_sufijo(bd=pm21, "pm", "21")

#agregar clave casillas

final_pm21_tamps <- final_pm21_tamps %>%
  separate(casilla,c("seccion","tipo_casilla", "id_casilla", "ext_con")," ", fill = "right") %>%
  mutate(tipo_casilla = if_else(tipo_casilla == "ESPECIAL","S",substr(tipo_casilla,1,1)),
         id_casilla = if_else(nchar(id_casilla) == 1, paste0('0',id_casilla), id_casilla),
         ext_con = if_else(is.na(ext_con),"00",
                           if_else( nchar(ext_con) == 2, gsub('C',"0",ext_con),
                                    gsub('C',"",ext_con))),
         estado = "28",
         nombre_estado = "TAMAULIPAS",
         clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla,ext_con))

final_pm21_tamps %>% count(nchar(clave_casilla))

#agregar lista nominal

final_pm21_tamps <- final_pm21_tamps %>% left_join(lista_nominal_21)

# guardar rda


tamps_pm_21 <- final_pm21_tamps

tamps_pm_21 %>% write_rds("inst/electoral/tamps/pm_21.rda")

rm(pm21)

## DL 19 TAMAULIPAS ------------

bd_dl_19_tamps %>% colnames

dl19 <- bd_dl_19_tamps   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(dl19)

dl19 <- dl19 %>%
  rename(noreg = num_votos_can_nreg,
         distritol_19 = id_distrito_local,
         nombre_distritol_19 = cabecera_distrital_local,
         municipio_19 = id_municipio,
         nombre_municipio_19 = municipio,
         nominal = lista_nominal,
         nulos = num_votos_nulos,
         total = total_votos
  ) %>%
  rename_with( ~ gsub("cand_", "", .x, fixed = TRUE)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_19 = formatC(municipio_19, width = 3, flag = "0"),
         distritol_19 = formatC(distritol_19, width = 3, flag = "0"))


dl19 <- dl19 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(dl19)

# sufijo para join

final_dl19_tamps <- insertar_sufijo(bd=dl19, "dl", "19")

#agregar clave casillas

final_dl19_tamps <- final_dl19_tamps  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 2 ~ gsub("CB","C0100",casilla),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "28",
         nombre_estado = "TAMAULIPAS",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_dl19_tamps %>% count(nchar(clave_casilla))
final_dl19_tamps %>% count(clave_casilla)

# guardar rda

tamps_dl_19 <- final_dl19_tamps

tamps_dl_19 %>% write_rds("inst/electoral/tamps/dl_19.rda")

rm(dl19)

## PM 18 TAMAULIPAS ------------


bd_pm_18_tamps %>% colnames

pm18 <- bd_pm_18_tamps   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(pm18)

pm18 <- pm18 %>%
  rename(noreg = num_votos_can_nreg,
         distritol_18 = id_distrito_local,
         nombre_distritol_18 = cabecera_distrital_local,
         municipio_18 = id_municipio,
         nombre_municipio_18 = municipio,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         panal = na,
         pes = es
  )%>%
  rename_with( ~ gsub("cand_", "", .x, fixed = TRUE)) %>%
  rename_with( ~ gsub("_es", "_pes",.x, fixed = T)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_18 = formatC(municipio_18, width = 3, flag = "0"),
         distritol_18 = formatC(distritol_18, width = 3, flag = "0"))


pm18 <- pm18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm18)

# sufijo para join

final_pm18_tamps <- insertar_sufijo(bd=pm18, "pm", "18")

#agregar clave casillas


final_pm18_tamps <- final_pm18_tamps  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "28",
         nombre_estado = "TAMAULIPAS",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_pm18_tamps %>% count(nchar(clave_casilla))
final_pm18_tamps %>% count(id_casilla)

# guardar rda

tamps_pm_18 <- final_pm18_tamps

tamps_pm_18 %>% write_rds("inst/electoral/tamps/pm_18.rda")

rm(pm18)

## PM 16 TAMAULIPAS ------------


bd_pm_16_tamps %>% colnames

pm16 <- bd_pm_16_tamps   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!validos)

# revisar nombres de varianles

colnames(pm16)

pm16 <- pm16 %>%
  rename(noreg = no_reg,
         distritol_16 = id_distrito,
         nombre_distritol_16 = cabecera_distrital,
         municipio_16 = id_municipio,
         nombre_municipio_16 = municipio,
  )%>%
  rename_with( ~ gsub("independiente_", "independiente", .x, fixed = TRUE)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_16 = formatC(municipio_16, width = 3, flag = "0"),
         distritol_16 = formatC(distritol_16, width = 3, flag = "0"))


pm16 <- pm16 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm16)

# sufijo para join

final_pm16_tamps <- insertar_sufijo(bd=pm16, "pm", "16")

#agregar clave casillas


final_pm16_tamps <- final_pm16_tamps  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 4 ~ gsub("C","00",casilla),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "28",
         nombre_estado = "TAMAULIPAS",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_pm16_tamps %>% count(nchar(clave_casilla))
final_pm16_tamps %>% count(id_casilla)

# guardar rda

tamps_pm_16 <- final_pm16_tamps

tamps_pm_16 %>% write_rds("inst/electoral/tamps/pm_16.rda")

rm(pm16)

## dl 16 TAMAULIPAS ------------


bd_dl_16_tamps %>% colnames

dl16 <- bd_dl_16_tamps   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!validos)

# revisar nombres de varianles

colnames(dl16)

dl16 <- dl16 %>%
  rename(noreg = no_reg,
         distritol_16 = id_distrito,
         nombre_distritol_16 = cabecera_distrital,
         municipio_16 = municipio,
         nombre_municipio_16 = nombre_municipio
  )%>%
  rename_with( ~ gsub("independiente_", "independiente", .x, fixed = TRUE)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_16 = formatC(municipio_16, width = 3, flag = "0"),
         distritol_16 = formatC(distritol_16, width = 3, flag = "0"))


dl16 <- dl16 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(dl16)

# sufijo para join

final_dl16_tamps <- insertar_sufijo(bd=dl16, "dl", "16")

#agregar clave casillas


final_dl16_tamps <- final_dl16_tamps  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "28",
         nombre_estado = "TAMAULIPAS",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_dl16_tamps %>% count(nchar(clave_casilla))
final_dl16_tamps %>% count(id_casilla)

# guardar rda

tamps_dl_16 <- final_dl16_tamps

tamps_dl_16 %>% write_rds("inst/electoral/tamps/dl_16.rda")

rm(dl16)




