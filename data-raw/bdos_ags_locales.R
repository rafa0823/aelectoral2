# AGUASCALIENTES LOCALES

# Cargar bases --------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

#lista nominal 21

lista_nominal_21 <- read_delim("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/diputaciones.csv",
                               delim = "|", escape_double = FALSE, trim_ws = TRUE,  locale = locale(encoding = "CP1252"),
                               skip = 5) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  select(nombre_estado,clave_casilla, lista_nominal_casilla) %>%
  filter(nombre_estado == "AGUASCALIENTES") %>%
  mutate(clave_casilla = gsub("[[:punct:]]","",clave_casilla))

# gb 16

bd_gb_16_ags <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/aguascalientes_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

#dl 21
setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2021/Distrito local/Aguascalientes 2021")

bd_dl_21_ags <- list.files(full.names = T) %>%
  map_dfr(~{
    print(.x)
    read_csv(.x,skip = 2) %>%
      janitor::clean_names() %>%
      mutate(distrito = .x)
  }) %>%
  mutate(distrito = gsub(pattern = "[[:alpha:]]|[[:punct:]]",
                         replacement = "",
                         distrito)) %>%
  filter(!casillas %in% c("TOTAL",NA))

#pm 21

setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2021/Municipio/Aguascalientes")

bd_pm_21_ags <- list.files(full.names = T) %>%
  map_dfr(~{
    print(.x)
    read_csv(.x,skip = 2) %>%
      janitor::clean_names() %>%
      mutate(municipio = .x)
  }) %>%
  mutate(municipio = gsub(pattern = "[[:alpha:]]|[[:punct:]]",
                         replacement = "",
                         municipio)) %>%
  filter(!casillas %in% c("TOTAL",NA))


# pm 19

bd_pm_19_ags <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2019/Municipio/aguascalientes_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

# dl 18

setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Distrito local/ags")


bd_dl_18_ags <- list.files(full.names = T) %>%
  map_dfr(~{
    print(.x)
    read_csv(.x) %>%
      janitor::clean_names() %>%
      mutate(id_municipio = as.double(id_municipio),
             mr_rp = toupper(substr(gsub("./aguascalientes_normal_casilla_","",.x),1,2)))
  })


setwd("~/Documents/Git/aelectoral2")

# pm 16

bd_pm_16_ags <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Municipio/aguascalientes_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

# dl 16

bd_dl_16_ags <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Distrito local/aguascalientes_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()



## GB 16 AGUASCALIENTES ------------------------------------------------------------------------------------------------------

gb16 <- bd_gb_16_ags   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!validos)

# revisar nombres de varianles

colnames(gb16)

gb16 <- gb16 %>%
  rename(noreg = no_reg,
         distritol_16 = id_distrito,
         nombre_distritol_16 = cabecera_distrital,
         municipio_16 = id_municipio,
         nombre_municipio_16 = municipio
  ) %>%
  rename_with( ~ gsub("cand_", "", .x, fixed = TRUE)) %>%
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

final_gb16_ags <- insertar_sufijo(bd=gb16, "gb", "16")

#agregar clave casillas

final_gb16_ags <- final_gb16_ags  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "01",
         nombre_estado = "AGUASCALIENTES",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_gb16_ags %>% count(nchar(clave_casilla))
final_gb16_ags %>% count(id_casilla)

# guardar rda

ags_gb_16 <- final_gb16_ags

ags_gb_16 %>% write_rds("inst/electoral/ags/gb_16.rda")

rm(gb16)

## DL 21 AGUASCALIENTES -------------------------------------------------------------------------------------


dl21 <- bd_dl_21_ags %>%
  select(casillas,distrito,pan:total)

# revisar nombres de varianles

colnames(dl21)

dl21 <- dl21 %>%
  rename(casilla = casillas,
         distritol_21 = distrito,
         panal = naa,
         pan_prd = co_pan_prd,
         pt_morena_panal = co_pt_morena_naa,
         pt_panal = pt_naa,
         morena_panal = morena_naa,
         noreg = num_votos_can_nreg,
         nulos = num_votos_nulos
  )%>%
  mutate(across(pan:total, ~as.numeric(.x)),
         distritol_21 = formatC(as.numeric(distritol_21), width = 3, flag = "0"))


dl21 <- dl21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:total)

# Identificar los partidos de la elecccion
detectar_partidos(dl21)

# sufijo para join

final_dl21_ags <- insertar_sufijo(bd=dl21, "dl", "21")

#agregar clave casillas

final_dl21_ags <- final_dl21_ags  %>%
  separate(casilla,c("seccion","casilla", "ext_con")," ", fill = "right") %>%
  mutate(seccion = formatC(as.numeric(seccion),width = 4,flag = "0"),
         tipo_casilla = if_else(casilla == "ESPECIAL","S",substr(casilla,1,1)),
         id_casilla = if_else(casilla == "B","01",
                           formatC(as.numeric(gsub("[[:alpha:]]","",casilla)),width = 2,flag = "0")),
         ext_con = if_else(is.na(ext_con),"00",gsub("C","0",ext_con)),
         estado = "01",
         nombre_estado = "AGUASCALIENTES",
         clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla,ext_con))

#pruebas

final_dl21_ags %>% count(nchar(clave_casilla))
final_dl21_ags %>% count(casilla)

#agregar lista nominal

final_dl21_ags <- final_dl21_ags %>% left_join(lista_nominal_21) %>%
  rename(ele_nominal_dl_21 = lista_nominal_casilla)

# guardar rda

ags_dl_21 <- final_dl21_ags

ags_dl_21 %>% write_rds("inst/electoral/ags/dl_21.rda")

rm(dl21)



## PM 21 AGUASCALIENTES -------------------------------------------------------------------------------------


pm21 <- bd_pm_21_ags %>%
  select(casillas,municipio,pan:total)

# revisar nombres de varianles

colnames(pm21)

pm21 <- pm21 %>%
  rename(casilla = casillas,
         municipio_21 = municipio,
         panal = naa,
         pan_prd = co_pan_prd,
         pt_morena_panal = co_pt_morena_naa,
         pt_panal = pt_naa,
         morena_panal = morena_naa,
         noreg = num_votos_can_nreg,
         nulos = num_votos_nulos
  )%>%
  mutate(across(pan:total, ~as.numeric(.x)),
         municipio_21 = formatC(as.numeric(municipio_21), width = 3, flag = "0"))


pm21 <- pm21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:total)

# Identificar los partidos de la elecccion
detectar_partidos(pm21)

# sufijo para join

final_pm21_ags <- insertar_sufijo(bd=pm21, "pm", "21")

#agregar clave casillas

final_pm21_ags <- final_pm21_ags  %>%
  separate(casilla,c("seccion","casilla", "ext_con")," ", fill = "right") %>%
  mutate(seccion = formatC(as.numeric(seccion),width = 4,flag = "0"),
         tipo_casilla = if_else(casilla == "ESPECIAL","S",substr(casilla,1,1)),
         id_casilla = if_else(casilla == "B","01",
                              formatC(as.numeric(gsub("[[:alpha:]]","",casilla)),width = 2,flag = "0")),
         ext_con = if_else(is.na(ext_con),"00",gsub("C","0",ext_con)),
         estado = "01",
         nombre_estado = "AGUASCALIENTES",
         clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla,ext_con))

#pruebas

final_pm21_ags %>% count(nchar(clave_casilla))
final_pm21_ags %>% count(casilla)

#agregar lista nominal

final_pm21_ags <- final_pm21_ags %>% left_join(lista_nominal_21) %>%
  rename(ele_nominal_pm21 = lista_nominal_casilla)


# guardar rda

ags_pm_21 <- final_pm21_ags

ags_pm_21 %>% write_rds("inst/electoral/ags/pm_21.rda")

rm(pm21)

## PM 19 AGUASCALIENTES ------------

bd_pm_19_ags %>% colnames

pm19 <- bd_pm_19_ags   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(pm19)

pm19 <- pm19 %>%
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


pm19 <- pm19 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm19)

# sufijo para join

final_pm19_ags <- insertar_sufijo(bd=pm19, "pm", "19")

#agregar clave casillas

final_pm19_ags <- final_pm19_ags  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "01",
         nombre_estado = "AGUASCALIENTES",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_pm19_ags %>% count(nchar(clave_casilla))
final_pm19_ags %>% count(id_casilla)

# guardar rda

ags_pm_19 <- final_pm19_ags

ags_pm_19 %>% write_rds("inst/electoral/ags/pm_19.rda")

rm(pm19)

## DL 18 AGUASCALIENTES ------------

bd_dl_18_ags %>% colnames

dl18 <- bd_dl_18_ags   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(dl18)

dl18 <- dl18 %>%
  rename(noreg = num_votos_can_nreg,
         distritol_18 = id_distrito_local,
         nombre_distritol_18 = cabecera_distrital_local,
         municipio_18 = id_municipio,
         nombre_municipio_18 = municipio,
         nominal = lista_nominal,
         nulos = num_votos_nulos,
         total = total_votos
  ) %>%
  rename_with( ~ gsub("cand_", "", .x, fixed = TRUE)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_18 = formatC(municipio_18, width = 3, flag = "0"),
         distritol_18 = formatC(distritol_18, width = 3, flag = "0"))


dl18 <- dl18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(dl18)

# sufijo para join

final_dl18_ags <- insertar_sufijo(bd=dl18, "dl", "18")

#agregar clave casillas

final_dl18_ags <- final_dl18_ags  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 2 ~ gsub("CB","C0100",casilla),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "01",
         nombre_estado = "AGUASCALIENTES",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_dl18_ags %>% count(nchar(clave_casilla))
final_dl18_ags %>% count(clave_casilla)

# guardar rda

ags_dl_18 <- final_dl18_ags

ags_dl_18 %>% write_rds("inst/electoral/ags/dl_18.rda")

rm(dl18)

## PM 16 AGUASCALIENTES ------------


bd_pm_16_ags %>% colnames

pm16 <- bd_pm_16_ags   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!validos)

# revisar nombres de varianles

colnames(pm16)

pm16 <- pm16 %>%
  rename(noreg = no_reg,
         distritol_16 = id_distrito,
         nombre_distritol_16 = cabecera_distrital,
         municipio_16 = id_municipio,
         nombre_municipio_16 = municipio
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

final_pm16_ags <- insertar_sufijo(bd=pm16, "pm", "16")

#agregar clave casillas


final_pm16_ags <- final_pm16_ags  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "01",
         nombre_estado = "AGUASCALIENTES",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_pm16_ags %>% count(nchar(clave_casilla))
final_pm16_ags %>% count(id_casilla)

# guardar rda

ags_pm_16 <- final_pm16_ags

ags_pm_16 %>% write_rds("inst/electoral/ags/pm_16.rda")

rm(pm16)

## dl 16 AGUASCALIENTES ------------


bd_dl_16_ags %>% colnames

dl16 <- bd_dl_16_ags   %>%
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

final_dl16_ags <- insertar_sufijo(bd=dl16, "dl", "16")

#agregar clave casillas


final_dl16_ags <- final_dl16_ags  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "01",
         nombre_estado = "AGUASCALIENTES",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_dl16_ags %>% count(nchar(clave_casilla))
final_dl16_ags %>% count(id_casilla)

# guardar rda

ags_dl_16 <- final_dl16_ags

ags_dl_16 %>% write_rds("inst/electoral/ags/dl_16.rda")

rm(dl16)







