# DURANGO LOCALES

## CARGAR BASES ------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

bd_pm_19_dgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2019/Municipio/durango_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

bd_dl_18_dgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Distrito local/durango_dl_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

# extraer dtos y mpos de durango 2016

dto_mpo_16 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Distrito local/durango_dl_16_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  select(estado,distrito,nombre_distrito, municipio, nombre_municipio,seccion) %>%
  unique()

# GB 16

path <- "~/Downloads/Tabla Gobernador Municipio.xlsx"

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

## PM 19 DURANGO ------------------------------------------------------------------------------------------------------

pm19 <- bd_pm_19_dgo   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))

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


## gb 16 DURANGO  ------------------------------------------------------------------------------------------------------

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
         municipio_16 = formatC(municipio_16,width = 3, flag = "0"))


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

dgo_gb_16 %>% write_rds("inst/electoral/dgo_gb_16.rda")

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

################################## PRUEBAS

