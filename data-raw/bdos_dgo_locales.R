# DURANGO LOCALES

## CARGAR BASES ------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

#pm19

bd_pm_19_dgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2019/Municipio/durango_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

#dl18

bd_dl_18_dgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Distrito local/durango_dl_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  select(distrito:pt_morena,independiente_1,no_reg:nominal)

#dl21

path <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2021/Distrito local/durango_normal_21_casilla.xlsx"

bd_dl_21_dgo <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(~ read_excel(path = path, sheet = .x)%>%
           janitor::clean_names() %>%
           filter(!str_detect("[[:alpha:]]",string = numero)) %>%
           as_tibble() %>%
           mutate(across(pan:nominal,~as.double(.x)),
                  seccion = as.double(seccion),
                  distrito = as.double(distrito)),
         .id = "Sheet") %>%
  select(!c(x28,x29,x30,x31,x24)) %>%
  select(Sheet:morena_pt_pvem,independiente_1,no_reg:participacion) %>%
  rename(pan_pri_prd_total = pan_pri_prd_cc,
         morena_pt_total = morena_pt_pvem)



bd_dl_21_dgo %>% filter(Sheet == "01. D. DURANGO") %>% summarise(sum(nominal))

# extraer dtos y mpos de durango 2016

dto_mpo_16 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Distrito local/durango_dl_16_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  select(estado,distrito,nombre_distrito, municipio, nombre_municipio,seccion) %>%
  unique()


# GB 16 DURANGO

path <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/durango_normal_16_casilla.xlsx"

bd_gb_16_dgo <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(~ read_excel(path = path, sheet = .x, skip =5)%>%
           janitor::clean_names() %>%
           filter(!str_detect("[[:alpha:]]",string = no_y_dtto)) %>%
           as_tibble() %>%
           mutate(seccion = as.double(seccion),
                  x4 = as.double(x4),
                  pan_prd = as.double(pan_prd),
                  pri_pvem_pd_pna = as.double(pri_pvem_pd_pna),
                  pt = as.double(pt),
                  morena = as.double(morena),
                  pes = as.double(pes),
                  dr_campa = as.double(dr_campa),
                  noreg = as.double(noreg),
                  votos_nulos = as.double(votos_nulos),
                  total = as.double(total),
                  l_nominal = as.double(l_nominal),
                  percent = as.double(percent)), .id = "Sheet")

bd_gb_16_dgo %>% filter(Sheet == "05. DURANGO") %>% summarise(sum(l_nominal))

# PM 16

bd_pm_16_dgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Municipio/durango_pm_16_normal_casilla.csv") %>%
  janitor::clean_names()


# bd_dl_16_dgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Distrito local/durango_normal_dl_16_casilla.csv",
#                         # skip = 5, n_max =  1102
#                          ) %>%
#   janitor::clean_names() %>%
#   filter(!str_detect("[[:alpha:]]",string = no)) %>%
#   as_tibble()

# # prueba lista nominal
# bd_dl_16_dgo %>% summarise(sum(l_nominal,na.rm = T))

# dl 16

bd_dl_16_dgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Distrito local/durango_dl_16_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()


## PM 19 DURANGO ------------------------------------------------------------------------------------------------------

pm19 <- bd_pm_19_dgo   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio)) %>%
  select(!validos)

# revisar nombres de varianles

colnames(pm19)

pm19 <- pm19 %>%
  rename("noreg"= no_reg,
         "municipio_19" = municipio,
         "nombre_municipio_19" = nombre_municipio
  )%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_19 = formatC(municipio_19, width = 3, flag = "0"))


pm19 <- pm19 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm19)

# sufijo para join

final_pm19_dgo <- insertar_sufijo(bd=pm19, "pm", "19")

final_pm19_dgo <- final_pm19_dgo  %>%
  mutate(clave_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                   nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                   nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla)),

         estado = "10",
         nombre_estado = "DURANGO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,clave_casilla))


# guardar rda

dgo_pm_19 <- final_pm19_dgo

dgo_pm_19 %>% write_rds("inst/electoral/dgo_pm_19.rda")

rm(pm19)

## DL 21 DURANGO  ------------------------------------------------------------------------------------------------------

dl21 <- bd_dl_21_dgo %>%
  mutate(nombre_distritol_21 = str_squish(gsub(pattern = "[[:digit:]]|[[:punct:]]",replacement = "",x = Sheet))) %>%
  mutate(nombre_distritol_21 = str_squish(gsub(pattern = "D ",replacement = "",x = nombre_distritol_21)))

# revisar nombres de varianles

colnames(dl21)

dl21 <- dl21 %>%
  rename(distritol_21 = distrito,
         noreg = no_reg) %>%
  select(distritol_21,
         nombre_distritol_21,seccion:participacion) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         distritol_21 = formatC(distritol_21,width = 2, flag = "0"))


dl21 <- dl21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(dl21)

# sufijo para join

final_dl21_dgo <- insertar_sufijo(bd=dl21, "dl", "21")

final_dl21_dgo <- final_dl21_dgo  %>%
  mutate(tipo_casilla = substr(casilla,1,1),
         id_casilla = case_when(nchar(casilla) == 2 ~  paste0(gsub("[[:alpha:]]","0",casilla),"00"),
                                nchar(casilla) == 3 ~  paste0(gsub("[[:alpha:]]","",casilla),"00"),
                                nchar(casilla) == 5 ~  gsub("E1 C","010",casilla)),
         estado = "10",
         nombre_estado = "DURANGO",
         clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla),
         mr_rp = "MR")

final_dl21_dgo %>% count(nchar(clave_casilla)) %>% view

final_dl21_dgo %>% filter(casilla == 3)

# guardar rda

dgo_dl_21 <- final_dl21_dgo

dgo_dl_21 %>% write_rds("inst/electoral/dgo_dl_21.rda")

rm(dl21)

# DL 18 DURANGO ----------------------------------------------------------------------------------------------------

dl18 <- bd_dl_18_dgo

# revisar nombres de varianles

colnames(dl18)

dl18 <- dl18 %>%
  mutate(prd = 0,
         pd = 0,
         pan = 0) %>%
  rename(distritol_18 = distrito,
         noreg = no_reg) %>%
  mutate(across(pan_prd_pd:pan, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         distritol_18 = formatC(distritol_18,width = 2, flag = "0"))


dl18 <- dl18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan_prd_pd:pan)

# Identificar los partidos de la elecccion
detectar_partidos(dl18)

# sufijo para join

final_dl18_dgo <- insertar_sufijo(bd=dl18, "dl", "18")

final_dl18_dgo <- final_dl18_dgo  %>%
  mutate(tipo_casilla = substr(casilla,1,1),
         id_casilla = case_when(nchar(casilla) == 1 ~  gsub(pattern = "[[:alpha:]]","0100", casilla),
                                nchar(casilla) == 2 ~  paste0(gsub("[[:alpha:]]","0",casilla),"00"),
                                nchar(casilla) == 3 ~  paste0(gsub("[[:alpha:]]","",casilla),"00"),
                                nchar(casilla) == 5 ~  gsub("E1 C","010",casilla)),
         estado = "10",
         nombre_estado = "DURANGO",
         clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla),
         mr_rp = "MR")

final_dl18_dgo %>% count(nchar(clave_casilla)) %>% view


# guardar rda

dgo_dl_18 <- final_dl18_dgo

dgo_dl_18 %>% write_rds("inst/electoral/dgo/dl_18.rda")

rm(dl18)



## GB 16 DURANGO  ------------------------------------------------------------------------------------------------------

gb16 <- bd_gb_16_dgo %>%
  mutate(nombre_municipio_16 = str_squish(gsub(pattern = "[[:digit:]]|[[:punct:]]",replacement = "",x = Sheet)),
         municipio_16 = str_squish(gsub(pattern = "[[:alpha:]]|[[:punct:]]",replacement = "",x = Sheet)),
         pri = 0,
         pvem = 0,
         pd = 0,
         panal = 0,
         prd = 0,
         pan = 0)

# revisar nombres de varianles

colnames(gb16)

gb16 <- gb16 %>%
  rename(drcampa = dr_campa,
         pri_pvem_pd_panal = pri_pvem_pd_pna) %>%
  select(municipio_16,
         nombre_municipio_16,seccion, casilla,pan_prd:pan) %>%
  mutate(across(pan_prd:pan, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio_16 = formatC(as.double(municipio_16),width = 3, flag = "0"))


gb16 <- gb16 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan_prd:pan)

# Identificar los partidos de la elecccion
detectar_partidos(gb16)

# sufijo para join

final_gb16_dgo <- insertar_sufijo(bd=gb16, "gb", "16")

final_gb16_dgo <- final_gb16_dgo  %>%
  mutate(tipo_casilla = if_else(str_detect(pattern = "ESPECIAL",casilla),"S",substr(casilla,1,1)),
         id_casilla = gsub(pattern = "[[:alpha:]]","",casilla),
         id_casilla = case_when(nchar(id_casilla) == 0 ~ "0100",
                                nchar(id_casilla) == 2 ~  paste0(gsub(" ","0",id_casilla),"00"),
                                nchar(id_casilla) == 3 ~  paste0(gsub(" ","",id_casilla),"00"),
                                nchar(id_casilla) == 5 ~  gsub(" 1  ","010",id_casilla)),
         id_casilla = if_else(nchar(id_casilla) == 3, gsub("100","0100", id_casilla),
                              id_casilla),
         estado = "10",
         nombre_estado = "DURANGO",
         clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla))


# guardar rda

dgo_gb_16 <- final_gb16_dgo

dgo_gb_16 %>% write_rds("inst/electoral/dgo/gb_16.rda")

rm(gb16)

## PM 16 DURANGO ------------------------------------------------------------------------------------------------------

pm16 <- bd_pm_16_dgo  %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio),
         prd = 0,
         pan = 0)


# revisar nombres de varianles

colnames(pm16)

pm16 <- pm16 %>%
  rename("municipio_16" = municipio,
         "nombre_municipio_16" = nombre_municipio,
         distritol_16 = distrito,
         nombre_distritol_16 = nombre_distrito
  )%>%
  mutate(across(pan_prd:pan, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio_16 = formatC(municipio_16, width = 3, flag = "0"),
         distritol_16 = formatC(distritol_16, width = 2, flag = "0"))



pm16 <- pm16 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan_prd:pan)

# Identificar los partidos de la elecccion
detectar_partidos(pm16)

# sufijo para join

final_pm16_dgo <- insertar_sufijo(bd=pm16, "pm", "16")

final_pm16_dgo <- final_pm16_dgo  %>%
  mutate(tipo_casilla = substr(casilla,1,1),
         id_casilla = case_when(nchar(casilla) == 1 ~ "0100",
                                nchar(casilla) == 3 ~  paste0(gsub("[[:alpha:]]","",casilla),"00"),
                                nchar(casilla) == 5 ~  paste0(gsub("[[:alpha:]]","",casilla),"00"),
                                nchar(casilla) == 6 ~  gsub("[[:alpha:]]","",casilla)),
         estado = "10",
         nombre_estado = "DURANGO",
         clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla))


# guardar rda

dgo_pm_16 <- final_pm16_dgo

dgo_pm_16 %>% write_rds("inst/electoral/dgo_pm_16.rda")

rm(pm16)

## DL 16 DURANGO ------------------------------------------------------------------------------------------------------

dl16 <- bd_pm_16_dgo  %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio),
         prd = 0,
         pan = 0)


# revisar nombres de varianles

colnames(dl16)

dl16 <- dl16 %>%
  rename("municipio_16" = municipio,
         "nombre_municipio_16" = nombre_municipio,
         distritol_16 = distrito,
         nombre_distritol_16 = nombre_distrito
  )%>%
  mutate(across(pan_prd:pan, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio_16 = formatC(municipio_16, width = 3, flag = "0"),
         distritol_16 = formatC(distritol_16, width = 2, flag = "0"))



dl16 <- dl16 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan_prd:pan)

# Identificar los partidos de la elecccion
detectar_partidos(dl16)

# sufijo para join

final_dl16_dgo <- insertar_sufijo(bd=dl16, "pm", "16")

final_dl16_dgo <- final_dl16_dgo  %>%
  mutate(tipo_casilla = substr(casilla,1,1),
         id_casilla = case_when(nchar(casilla) == 1 ~ "0100",
                                nchar(casilla) == 3 ~  paste0(gsub("[[:alpha:]]","",casilla),"00"),
                                nchar(casilla) == 5 ~  paste0(gsub("[[:alpha:]]","",casilla),"00"),
                                nchar(casilla) == 6 ~  gsub("[[:alpha:]]","",casilla)),
         estado = "10",
         nombre_estado = "DURANGO",
         clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla),
         mr_rp = "MR")

# prubas casillas

final_dl16_dgo %>% count(casilla) %>% view()

#especiales solo hay SMR01 por lpo que se asume que en esta base no se contemplan los diputados de RP


final_dl16_dgo %>% count(nchar(clave_casilla))


# guardar rda

dgo_dl_16 <- final_dl16_dgo

dgo_dl_16 %>% write_rds("inst/electoral/dgo_dl_16.rda")

rm(dl16)


################################## PRUEBAS

