# HIDALGO LOCALES

# Cargar bases --------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

#GB16
bd_gb_16_hgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/hidalgo_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

#PM20

bd_pm_20_hgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2020/Municipio/hgo_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

#DL 18

setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Distrito local/hgo")

bd_dl_18_hgo <- list.files(full.names = T) %>%
  map_dfr(~{
    print(.x)
    read_csv(.x) %>%
      janitor::clean_names() %>%
      mutate(id_municipio = as.double(id_municipio),
             mr_rp = toupper(substr(gsub("./hidalgo_normal_casilla_","",.x),1,2)))
  })


setwd("~/Documents/Git/aelectoral2")

# pm 16

bd_pm_16_hgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Municipio/hidalgo_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

# dl 16

bd_dl_16_hgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Distrito local/hidalgo_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()




## GB 16 HIDALGO ------------------------------------------------------------------------------------------------------

gb16 <- bd_gb_16_hgo   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!validos)

# revisar nombres de varianles

colnames(gb16)

gb16 <- gb16 %>%
  rename(noreg = no_reg,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         municipio = municipio,
         nombre_municipio = nombre_municipio,
  ) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritol = formatC(distritol, width = 3, flag = "0"))


gb16 <- gb16 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(gb16)

# sufijo para join

final_gb16_hgo <- insertar_sufijo(bd=gb16, "gb", "16")

#agregar clave casillas

final_gb16_hgo <- final_gb16_hgo  %>%
  mutate(casilla = gsub(" ","",casilla),
         id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "13",
         nombre_estado = "HIDALGO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_gb16_hgo %>% count(casilla)

final_gb16_hgo %>% count(nchar(clave_casilla))

# guardar rda

hgo_gb_16 <- final_gb16_hgo

hgo_gb_16 %>% write_rds("inst/electoral/hgo/gb_16.rda")

rm(gb16)

## PM 20 HIDALGO ------------------------------------------------------------------------------------------------------

pm20 <- bd_pm_20_hgo   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(pm20)

pm20 <- pm20 %>%
  rename(estado = id_estado,
         noreg = num_votos_can_nreg,
         distritol = id_distrito_local,
         nombre_distritol = cabecera_distrital_local,
         municipio = id_municipio,
         nombre_municipio = municipio,
         panal = nva_alianza,
         pes = esh,
         pvem_pt_morena_pes = pvem_pt_morena_esh,
         pvem_pt_pes = pvem_pt_esh,
         pvem_pes = pvem_esh,
         pvem_morena_pes = pvem_morena_esh,
         pt_morena_pes = pt_morena_esh,
         pt_pes = pt_esh,
         morena_pes = morena_esh,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal
  )%>%
  rename_with(~gsub("cand_ind", "candind", .x, fixed = TRUE)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritol = formatC(distritol, width = 3, flag = "0"))


pm20 <- pm20 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm20)

# sufijo para join

final_pm20_hgo <- insertar_sufijo(bd=pm20, "pm", "20")

#agregar clave casillas

final_pm20_hgo <- final_pm20_hgo  %>%
  mutate(casilla = gsub(" ","",casilla),
         id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "13",
         nombre_estado = "HIDALGO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_pm20_hgo %>% count(id_casilla)

final_pm20_hgo %>% count(nchar(clave_casilla))

# guardar rda

hgo_pm_20 <- final_pm20_hgo

hgo_pm_20 %>% write_rds("inst/electoral/hgo/pm_20.rda")

rm(pm20)

## DL 18 HIDALGO ------------

bd_dl_18_hgo %>% colnames

dl18 <- bd_dl_18_hgo   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(dl18)

dl18 <- dl18 %>%
  rename(noreg = num_votos_can_nreg,
         distritol = id_distrito_local,
         nombre_distritol = cabecera_distrital_local,
         municipio = id_municipio,
         nombre_municipio = municipio,
         nominal = lista_nominal,
         nulos = num_votos_nulos,
         total = total_votos,
         panal = na,
         pes = es,
         panal_es = na_es
  ) %>%
  rename_with( ~ gsub("cand_", "", .x, fixed = TRUE)) %>%
  rename_with( ~ gsub("_na", "_panal", .x, fixed = TRUE)) %>%
  rename_with( ~ gsub("_es", "_pes", .x, fixed = TRUE)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritol = formatC(distritol, width = 3, flag = "0"))


dl18 <- dl18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(dl18)

# sufijo para join

final_dl18_hgo <- insertar_sufijo(bd=dl18, "dl", "18")

#agregar clave casillas

final_dl18_hgo <- final_dl18_hgo  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 2 ~ gsub("CB","C0100",casilla),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "13",
         nombre_estado = "HIDALGO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_dl18_hgo %>% count(nchar(clave_casilla))
final_dl18_hgo %>% count(clave_casilla)

# guardar rda

hgo_dl_18 <- final_dl18_hgo

hgo_dl_18 %>% write_rds("inst/electoral/hgo/dl_18.rda")

rm(dl18)

## PM 16 hidalgo ------------


bd_pm_16_hgo %>% colnames

pm16 <- bd_pm_16_hgo   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(pm16)

pm16 <- pm16 %>%
  rename(noreg = num_votos_can_nreg,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         municipio = id_municipio,
         nombre_municipio = municipio,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         panal = nva_alianza,
         pes = es,
         pri_pvem_panal = pri_pvem_nva_alianza,
         pri_panal = pri_nva_alianza,
         pvem_panal = pvem_nva_alianza
  )%>%
  rename_with( ~ gsub("cand_", "cand", .x, fixed = TRUE)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritol = formatC(distritol, width = 3, flag = "0"))


pm16 <- pm16 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm16)

# sufijo para join

final_pm16_hgo <- insertar_sufijo(bd=pm16, "pm", "16")

#agregar clave casillas


final_pm16_hgo <- final_pm16_hgo  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "13",
         nombre_estado = "HIDALGO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_pm16_hgo %>% count(nchar(clave_casilla))
final_pm16_hgo %>% count(id_casilla)

# guardar rda

hgo_pm_16 <- final_pm16_hgo

hgo_pm_16 %>% write_rds("inst/electoral/hgo/pm_16.rda")

rm(pm16)

## dl 16 hidalgo ------------


bd_dl_16_hgo %>% colnames

dl16 <- bd_dl_16_hgo   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(dl16)

dl16 <- dl16 %>%
  rename(noreg = num_votos_can_nreg,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         municipio = id_municipio,
         nombre_municipio = municipio,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         panal = nva_alianza,
         pes = es,
         pri_pvem_panal = pri_pvem_nva_alianza,
         pri_panal = pri_nva_alianza,
         pvem_panal = pvem_nva_alianza
  )%>%
  rename_with( ~ gsub("independiente_", "independiente", .x, fixed = TRUE)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritol = formatC(distritol, width = 3, flag = "0"))


dl16 <- dl16 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(dl16)

# sufijo para join

final_dl16_hgo <- insertar_sufijo(bd=dl16, "dl", "16")

#agregar clave casillas


final_dl16_hgo <- final_dl16_hgo  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "13",
         nombre_estado = "HIDALGO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_dl16_hgo %>% count(nchar(clave_casilla))
final_dl16_hgo %>% count(id_casilla)

# guardar rda

hgo_dl_16 <- final_dl16_hgo

hgo_dl_16 %>% write_rds("inst/electoral/hgo/dl_16.rda")

rm(dl16)






