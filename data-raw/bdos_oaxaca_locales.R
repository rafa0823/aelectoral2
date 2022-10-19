# OAXACA LOCALES

# Cargar bases --------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# PM 18

bd_pm_18_oaxaca <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Municipio/oaxaca_normal_casilla.xlsx") %>%
  janitor::clean_names() %>%
  as_tibble() %>% mutate(pan_prd_26  =  as.double(pan_prd_26),
                         pan_mc_27    =  as.double(pan_mc_27),
                         prd_mc_28    =  as.double(prd_mc_28),
                         pri_pvem_30    =  as.double(pri_pvem_30),
                         pri_na_31     =  as.double(pri_na_31),
                         pvem_na_32     =  as.double(pvem_na_32),
                         pan_prd_38     =  as.double(pan_prd_38),
                         pan_mc_39  =  as.double(pan_mc_39),
                         prd_mc_40  =  as.double(prd_mc_40),
                         pri_pvem_42  =  as.double(pri_pvem_42),
                         pri_na_43  =  as.double(pri_na_43),
                         pvem_na_44  =  as.double(pvem_na_44),
                         prd_mc_46  =  as.double(prd_mc_46),
                         c_comun_pvem_na = as.double(c_comun_pvem_na)) %>%
  rename( pan_prd = pan_prd_26,
          pan_mc =  pan_mc_27  ,
          prd_mc =  prd_mc_28  ,
          pri_pvem =  pri_pvem_30,
          pri_na =  pri_na_31  ,
          pvem_na=  pvem_na_32
  )

bd_pm_18_oaxaca <- bd_pm_18_oaxaca %>% mutate(pan_prd = if_else(is.na(pan_prd), pan_prd_38, pan_prd),
                                              pan_mc = if_else(is.na(pan_mc), pan_mc_39, pan_mc),
                                              prd_mc = if_else(is.na(prd_mc), prd_mc_40, prd_mc),
                                              prd_mc = if_else(is.na(prd_mc), prd_mc_46, prd_mc),
                                              pri_pvem = if_else(is.na(pri_pvem), pri_pvem_42, pri_pvem),
                                              pri_na = if_else(is.na(pri_na), pri_na_43, pri_na),
                                              pvem_na = if_else(is.na(pvem_na), pvem_na_44, pvem_na)) %>%
  select(!c(pan_prd_38,pan_mc_39, prd_mc_40, pri_pvem_42, pri_na_43, pvem_na_44, prd_mc_46)) %>%
  mutate(pan_prd_mc = if_else(is.na(pan_prd_mc), c_comun_pan_prd_mc, pan_prd_mc),
         pri_pvem_na = if_else(is.na(pri_pvem_na), c_comun_pri_pvem_na, pri_pvem_na),
         prd_mc_pup = c_comun_prd_mc_pup,
         pri_pvem = if_else(is.na(pri_pvem), c_comun_pri_pvem, pri_pvem),
         pri_na = if_else(is.na(pri_na), c_comun_pri_na, pri_na),
         pvem_na = if_else(is.na(pvem_na), c_comun_pvem_na, pvem_na),
         prd_mc = if_else(is.na(prd_mc), c_comun_prd_mc, prd_mc)) %>%
  select(!c(c_comun_pan_prd_mc, c_comun_pri_pvem_na, c_comun_prd_mc_pup,c_comun_pri_pvem, c_comun_pvem_na, c_comun_prd_mc, c_comun_pri_na))


bd_pm_18_oaxaca %>% names

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

bd_dl_18_oaxaca %>% names()

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

bd_gb_16_oaxaca <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/oaxaca_normal_casilla.xlsx") %>%
  janitor::clean_names() %>%
  as_tibble()

#DL 16

setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Distrito local/oaxaca")


bd_dl_16_oaxaca <- list.files(full.names = T) %>%
  map_dfr(~{
    print(.x)
    read_excel(.x,
               col_types = c("numeric", "numeric", "text",
                             "numeric", "text", "numeric", "text",
                             "numeric", "text", "numeric", "numeric",
                             "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric",
                             "numeric", "numeric", "numeric")) %>%
      janitor::clean_names() %>%
      mutate(
        mr_rp = toupper(substr(gsub("./oaxaca_normal_casilla_","",.x),1,2)))
  }) %>%
  rename(pri_pes = c_comun_pri_es)

bd_dl_16_oaxaca %>% names()

setwd("~/Documents/Git/aelectoral2")


# pm 16

bd_pm_16_oaxaca <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Municipio/oaxaca_normal_casilla.xlsx") %>%
  janitor::clean_names() %>%
  as_tibble()


## PM 18 OAXACA ------------------------------------------------------------------------------------------------------

names(bd_pm_18_oaxaca)

pm18 <- bd_pm_18_oaxaca  %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]", replacement = "", x = municipio)) %>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(pm18)

pm18 <- pm18 %>%
  rename(distritol = id_distrito_local,
         nombre_distritol = cabecera_distrital_local,
         nombre_municipio = municipio,
         municipio = id_municipio,
         estado = id_estado,
         panal = na,
         pes = es,
         pri_pvem_panal = pri_pvem_na,
         pri_panal = pri_na,
         pvem_panal = pvem_na,
         pt_morena_pes = pt_morena_es,
         pt_pes = pt_es,
         morena_pes = morena_es,
         noreg = num_votos_can_nreg,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal
  ) %>%
  rename_with( ~ gsub("cand_", "", .x, fixed = TRUE)) %>%
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

final_pm18_oaxaca <- insertar_sufijo(bd=pm18, "pm", "18")

#agregar clave casillas

final_pm18_oaxaca <- final_pm18_oaxaca  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "20",
         nombre_estado = "OAXACA",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_pm18_oaxaca %>% count(nchar(clave_casilla))

# guardar rda

oaxaca_pm_18 <- final_pm18_oaxaca

oaxaca_pm_18 %>% write_rds("inst/electoral/oaxaca/pm_18.rda")

rm(pm18)





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

bd_dl_16_oaxaca %>% names

dl16 <- bd_dl_16_oaxaca   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(dl16)

dl16 <- dl16 %>%
  rename(noreg = num_votos_can_nreg,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         estado = id_estado,
         nombre_municipio = municipio,
         municipio = id_municipio,
         pes = es,
         panal = nva_alianza,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal
  )%>%
  rename_with(~ gsub("cand_", "", .x, fixed = T)) %>%
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
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(gb16)

gb16 <- gb16 %>%
  rename(estado = id_estado,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         nombre_municipio = municipio,
         municipio = id_municipio,
         panal = nva_alianza,
         pri_pvem_panal = pri_pvem_nva_alianza,
         pri_panal = pri_nva_alianza,
         pvem_nva_alianza = pvem_nva_alianza,
         noreg = num_votos_can_nreg,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal

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




## GB 16 OAXACA ------------------------------------------------------------------------------------------------------

pm16 <- bd_pm_16_oaxaca   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(pm16)

pm16 <- pm16 %>%
  rename(estado = id_estado,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         nombre_municipio = municipio,
         municipio = id_municipio,
         panal = nva_alianza,
         pes = es,
         panal_psd = c_comun_nva_alianza_psd,
         planillaunica = planilla_unica,
         noreg = num_votos_can_nreg,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal
  )%>%
  rename_with(~gsub("cand_", "", .x, fixed = T)) %>%
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

final_pm16_oaxaca <- insertar_sufijo(bd=pm16, "pm", "16")

#agregar clave casillas

final_pm16_oaxaca <- final_pm16_oaxaca  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "20",
         nombre_estado = "OAXACA",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_pm16_oaxaca %>% count(nchar(clave_casilla))

# guardar rda

oaxaca_pm_16 <- final_pm16_oaxaca

oaxaca_pm_16 %>% write_rds("inst/electoral/oaxaca/pm_16.rda")

rm(pm16)





