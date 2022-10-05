# QUINTANA ROO LOCALES

# Cargar bases --------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# DL 19

setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2019/Distrito local/q_roo")


bd_dl_19_q_roo <- list.files(full.names = T) %>%
  map_dfr(~{
    print(.x)
    read_excel(.x) %>%
      janitor::clean_names() %>%
      mutate(
             mr_rp = toupper(substr(gsub("./quintanaroo_normal_casilla_","",.x),1,2)))
  })


setwd("~/Documents/Git/aelectoral2")

# pm 18

bd_pm_18_q_roo <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Municipio/quintanaroo_normal_casilla.xlsx") %>%
  janitor::clean_names() %>%
  as_tibble()


# dl 16

setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Distrito local/q_roo")


bd_dl_16_q_roo <- list.files(full.names = T) %>%
  map_dfr(~{
    print(.x)
    read_excel(.x) %>%
      janitor::clean_names() %>%
      mutate(
        mr_rp = toupper(substr(gsub("./quintanaroo_normal_casilla_","",.x),1,2)))
  })


setwd("~/Documents/Git/aelectoral2")

# pm 16

bd_pm_16_q_roo <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Municipio/qintanaroo_normal_casilla.xlsx") %>%
  janitor::clean_names() %>%
  as_tibble()

# gb 16

bd_gb_16_q_roo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/quintanaroo_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()


## DL 19 QUINTANA ROO ------------------------------------------------------------------------------------------------------


bd_dl_19_q_roo %>% colnames

dl19 <- bd_dl_19_q_roo   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!c(num_votos_validos, circunscripcion))

# revisar nombres de varianles

colnames(dl19)

dl19 <- dl19 %>%
  rename(noreg = num_votos_can_nreg,
         distritol = id_distrito_local,
         nombre_distritol = cabecera_distrital_local,
         municipio = id_municipio,
         nombre_municipio = municipio,
         nominal = lista_nominal,
         nulos = num_votos_nulos,
         total = total_votos,
         estado = id_estado
  ) %>%
  rename_with( ~ gsub("cand_", "", .x, fixed = TRUE)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritol = formatC(distritol, width = 3, flag = "0"))


dl19 <- dl19 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(dl19)

# sufijo para join

final_dl19_q_roo <- insertar_sufijo(bd=dl19, "dl", "19")

#agregar clave casillas

final_dl19_q_roo <- final_dl19_q_roo  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 2 ~ gsub("CB","C0100",casilla),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "23",
         nombre_estado = "QUINTANA ROO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_dl19_q_roo %>% count(nchar(clave_casilla))
final_dl19_q_roo %>% count(clave_casilla)

# guardar rda

q_roo_dl_19 <- final_dl19_q_roo

q_roo_dl_19 %>% write_rds("inst/electoral/q_roo/dl_19.rda")

rm(dl19)

## PM 18 QUINTANA ROO ------------------------------------------------------------------------------------------------------

pm18 <- bd_pm_18_q_roo   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!c(circunscripcion, num_votos_validos))

# revisar nombres de varianles

colnames(pm18)

pm18 <- pm18 %>%
  rename(noreg = num_votos_can_nreg,
         distritol = id_distrito_local,
         nombre_distritol = cabecera_distrital_local,
         municipio = id_municipio,
         nombre_municipio = municipio,
         estado = id_estado,
         panal = na,
         pri_pvem_panal = pri_pvem_na,
         pri_panal = pri_na,
         pvem_panal = pvem_na,
         pes = es,
         ind1 = cand_ind1,
         ind2 = cand_ind2,
         ind3 = cand_ind3,
         ind4 = cand_ind4,
         ind5 = cand_ind5,
         nominal = lista_nominal,
         total = total_votos,
         nulos = num_votos_nulos


  )%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritol = formatC(distritol, width = 3, flag = "0"))


pm18 <- pm18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm18)

# sufijo para join

final_pm18_q_roo <- insertar_sufijo(bd=pm18, "pm", "18")

#agregar clave casillas

final_pm18_q_roo <- final_pm18_q_roo  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "23",
         nombre_estado = "QUINTANA ROO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_pm18_q_roo %>% count(nchar(clave_casilla))

# guardar rda

q_roo_pm_18 <- final_pm18_q_roo

q_roo_pm_18 %>% write_rds("inst/electoral/q_roo/pm_18.rda")

rm(pm18)



## DL 16 QUINTANA ROO ------------------------------------------------------------------------------------------------------


bd_dl_16_q_roo %>% colnames

dl16 <- bd_dl_16_q_roo   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!c(num_votos_validos, circunscripcion))

# revisar nombres de varianles

colnames(dl16)

dl16 <- dl16 %>%
  rename(noreg = num_votos_can_nreg,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         municipio = id_municipio,
         nombre_municipio = municipio,
         nominal = lista_nominal,
         nulos = num_votos_nulos,
         total = total_votos,
         estado = id_estado,
         panal = nva_alianza,
         pes = es,
         pri_pvem_panal = pri_pvem_nva_alianza,
         pri_panal = pri_nva_alianza,
         pvem_panal = pvem_nva_alianza
  ) %>%
  rename_with( ~ gsub("cand_", "", .x, fixed = TRUE)) %>%
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

final_dl16_q_roo <- insertar_sufijo(bd=dl16, "dl", "16")

#agregar clave casillas

final_dl16_q_roo <- final_dl16_q_roo  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 2 ~ gsub("CB","C0100",casilla),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "23",
         nombre_estado = "QUINTANA ROO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#pruebas

final_dl16_q_roo %>% count(nchar(clave_casilla))
final_dl16_q_roo %>% count(clave_casilla)

# guardar rda

q_roo_dl_16 <- final_dl16_q_roo

q_roo_dl_16 %>% write_rds("inst/electoral/q_roo/dl_16.rda")

rm(dl16)



## PM 16 QUINTANA ROO ------------------------------------------------------------------------------------------------------

names(bd_pm_16_q_roo)

pm16 <- bd_pm_16_q_roo   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!c(num_votos_validos, circunscripcion))

# revisar nombres de varianles

colnames(pm16)

pm16 <- pm16 %>%
  rename(estado = id_estado,
         noreg = num_votos_can_nreg,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         municipio = id_municipio,
         nombre_municipio = municipio,
         panal = nva_alianza,
         pes = es,
         pri_pvem_panal = pri_pvem_nva_alianza,
         pri_panal = pri_nva_alianza,
         pvem_panal = pvem_nva_alianza,
         nominal = lista_nominal,
         total = total_votos,
         nulos = num_votos_nulos
  )%>%
  rename_with( ~ gsub("cand_", "", .x, fixed = TRUE)) %>%
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

final_pm16_q_roo <- insertar_sufijo(bd=pm16, "pm", "16")

#agregar clave casillas

final_pm16_q_roo <- final_pm16_q_roo  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "23",
         nombre_estado = "QUINTANA ROO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_pm16_q_roo %>% count(nchar(clave_casilla))

names(final_pm16_q_roo)

# guardar rda

q_roo_pm_16 <- final_pm16_q_roo

q_roo_pm_16 %>% write_rds("inst/electoral/q_roo/pm_16.rda")

rm(pm16)

## GB 16 QUINTANA ROO ------------------------------------------------------------------------------------------------------

gb16 <- bd_gb_16_q_roo   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!validos)

# revisar nombres de varianles

colnames(gb16)

gb16 <- gb16 %>%
  rename(noreg = no_reg,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         municipio = id_municipio,
         nombre_municipio = municipio
  )%>%
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

final_gb16_q_roo <- insertar_sufijo(bd=gb16, "gb", "16")

#agregar clave casillas

final_gb16_q_roo <- final_gb16_q_roo  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "23",
         nombre_estado = "QUINTANA ROO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_gb16_q_roo %>% count(nchar(clave_casilla))

# guardar rda

q_roo_gb_16 <- final_gb16_q_roo

q_roo_gb_16 %>% write_rds("inst/electoral/q_roo/gb_16.rda")

rm(gb16)

