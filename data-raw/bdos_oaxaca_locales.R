# OAXACA LOCALES

# Cargar bases --------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# PM 18

bd_pm_18_oaxaca <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Municipio/oaxaca_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  filter(nombre_municipio != "SAN JUAN IHUALTEPEC")

# PM 18 EXT

bd_pm_ext_18_oaxaca <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Municipio/oaxaca_extraordinaria_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

# JUNTAR PM EXTRAORDINARIA

bd_pm_18_oaxaca <- bd_pm_18_oaxaca %>% bind_rows(bd_pm_ext_18_oaxaca)

# DL 18

setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Distrito local/oaxaca")


bd_dl_18_oaxaca <- list.files(full.names = T) %>%
  map_dfr(~{
    print(.x)
    read_excel(.x) %>%
      janitor::clean_names() %>%
      mutate(
        mr_rp = toupper(substr(gsub("./oaxaca_normal_casilla_","",.x),1,2)))
  })

bd_dl_18_oaxaca %>% select_if(is.logical) %>% names()

bd_dl_18_oaxaca <- bd_dl_18_oaxaca %>%  mutate(c_comun_pri_pvem_na =  as.double(c_comun_pri_pvem_na),
                                               pri_na_39 =  as.double(pri_na_39),
                                               pvem_na_40 =  as.double(pvem_na_40),
                                               c_comun_pri_pvem  =  as.double(c_comun_pri_pvem),
                                               cand_ind1  =  as.double(cand_ind1),
                                               pri_pvem_38  =  as.double(pri_pvem_38))

bd_dl_18_oaxaca <- bd_dl_18_oaxaca %>% mutate(pri_na = if_else(is.na(pri_na_31), pri_na_39, pri_na_31),
                                              pvem_na = if_else(is.na(pvem_na_32), pvem_na_40, pvem_na_32),
                                              pri_pvem = if_else(is.na(pri_pvem_30), pri_pvem_38, pri_pvem_30)) %>%
  select(!c(pri_na_31, pri_na_39, pvem_na_32, pvem_na_40, pri_pvem_30, pri_pvem_38)) %>%
  mutate(pri_na = if_else(is.na(pri_na), pri_na, c_comun_pri_na),
         pri_pvem_na = if_else(is.na(pri_pvem_na), c_comun_pri_pvem_na, pri_pvem_na),
         pri_pvem = if_else(is.na(pri_pvem), c_comun_pri_pvem, pri_pvem)) %>%
  select(!c(c_comun_pri_na, c_comun_pri_pvem_na, c_comun_pri_pvem))



setwd("~/Documents/Git/aelectoral2")




# GB 16

bd_gb_16_oaxaca <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/oaxaca_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

#DL 16

bd_dl_16_oaxaca <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Distrito local/oaxaca_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()



## PM 18 OAXACA ------------------------------------------------------------------------------------------------------


names(bd_pm_18_oaxaca)

pm18 <- bd_pm_18_oaxaca   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(nombre_distrito, municipio, seccion)

# revisar nombres de varianles

colnames(dl18)

dl18 <- dl18 %>%
  rename(noreg = no_reg,
         distritol = id_distrito_local,
         nombre_distritol = cabecera_distrital_local,
         panal =  na,
         pes = es,
         pri_pvem_panal = pri_pvem_na,
         pt_morena_pes = pt_morena_es,
         pt_pes = pt_es,
         morena_pes = morena_es,
         ind1 = cand_ind1,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         pri_panal = pri_na,
         pvem_panal = pvem_na
  ) %>%
  select(circunscripcion:pri_pvem_panal, pri_panal,pvem_panal, pri_pvem,pt_morena_pes:mr_rp) %>%
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

final_dl18_oaxaca <- insertar_sufijo(bd=dl18, "dl", "18")

#agregar clave casillas

final_dl18_oaxaca <- final_dl18_oaxaca  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "20",
         nombre_estado = "OAXACA",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_dl18_oaxaca %>% count(nchar(clave_casilla))

# guardar rda

oaxaca_dl_18 <- final_dl18_oaxaca

oaxaca_dl_18 %>% write_rds("inst/electoral/oaxaca/dl_18.rda")

rm(dl18)





## DL 18 OAXACA ------------------------------------------------------------------------------------------------------


names(bd_dl_18_oaxaca)

dl18 <- bd_dl_18_oaxaca   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(dl18)

dl18 <- dl18 %>%
  rename(noreg = num_votos_can_nreg,
         distritol = id_distrito_local,
         nombre_distritol = cabecera_distrital_local,
         panal =  na,
         pes = es,
         pri_pvem_panal = pri_pvem_na,
         pt_morena_pes = pt_morena_es,
         pt_pes = pt_es,
         morena_pes = morena_es,
         ind1 = cand_ind1,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         pri_panal = pri_na,
         pvem_panal = pvem_na
  ) %>%
  select(circunscripcion:pri_pvem_panal, pri_panal,pvem_panal, pri_pvem,pt_morena_pes:mr_rp) %>%
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

final_dl18_oaxaca <- insertar_sufijo(bd=dl18, "dl", "18")

#agregar clave casillas

final_dl18_oaxaca <- final_dl18_oaxaca  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "20",
         nombre_estado = "OAXACA",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_dl18_oaxaca %>% count(nchar(clave_casilla))

# guardar rda

oaxaca_dl_18 <- final_dl18_oaxaca

oaxaca_dl_18 %>% write_rds("inst/electoral/oaxaca/dl_18.rda")

rm(dl18)





## DL 16 OAXACA ------------------------------------------------------------------------------------------------------

dl16 <- bd_dl_16_oaxaca   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!validos)

# revisar nombres de varianles

colnames(dl16)

dl16 <- dl16 %>%
  rename(noreg = no_reg,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         ind1 =  independiente_1,
         ind2 = independiente_2,
         ind3 = independiente_3,
         ind4 = independiente_4,
         ind5 = independiente_5
  )%>%
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

final_dl16_oaxaca <- insertar_sufijo(bd=dl16, "dl", "16")

#agregar clave casillas

final_dl16_oaxaca <- final_dl16_oaxaca  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "20",
         nombre_estado = "OAXACA",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_dl16_oaxaca %>% count(nchar(clave_casilla))

# guardar rda

oaxaca_dl_16 <- final_dl16_oaxaca

oaxaca_dl_16 %>% write_rds("inst/electoral/oaxaca/dl_16.rda")

rm(dl16)




## GB 16 OAXACA ------------------------------------------------------------------------------------------------------

gb16 <- bd_gb_16_oaxaca   %>%
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

final_gb16_oaxaca <- insertar_sufijo(bd=gb16, "gb", "16")

#agregar clave casillas

final_gb16_oaxaca <- final_gb16_oaxaca  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "20",
         nombre_estado = "OAXACA",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_gb16_oaxaca %>% count(nchar(clave_casilla))

# guardar rda

oaxaca_gb_16 <- final_gb16_oaxaca

oaxaca_gb_16 %>% write_rds("inst/electoral/oaxaca/gb_16.rda")

rm(gb16)
