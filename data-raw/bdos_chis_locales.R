
# CHIAPAS LOCALES

# CARGAR BASES -------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

bd_pm_21_chis <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Chiapas/2021_AYUN_LOC_MR_CHIS_CAS.xlsx",
                            n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()

bd_pmext_22_chis <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2022/Municipio/chiapas_extraordinaria_casilla_2022.csv") %>%
  janitor::clean_names() %>%
  as_tibble()


bd_pm_18_chis <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Municipio/chiapas_normal_casillacsv.csv")%>%
  janitor::clean_names() %>%
  as_tibble()

bd_pmext_18_chis <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Municipio/chiapas_extraordinaria_casilla.csv")%>%
  janitor::clean_names() %>%
  as_tibble()

bd_pm_15_chis <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2015/Municipio/chiapas_normal_casilla.csv")%>%
  janitor::clean_names() %>%
  as_tibble()

bd_pmext_15_chis <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2015/Municipio/chiapas_extraordinaria_casilla.csv")%>%
  janitor::clean_names() %>%
  as_tibble()

bd_gb_18_chis <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Gobernador/chiapas_normal_casilla.csv")%>%
  janitor::clean_names() %>%
  as_tibble()

bd_dl_21_chis <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2021/Distrito local/chiapas_normal_casilla.csv")%>%
  janitor::clean_names() %>%
  as_tibble()



# PM 21 CHIAPAS ------------------------------------------

pm21 <- bd_pm_21_chis   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))

# revisar nombres de varianles

colnames(pm21)

pm21 <- pm21 %>%
  rename("estado" = id_estado,
         "nombre_estado" = estado,
         "noreg"= no_registrados,
         "municipio" = id_municipio,
         "nombre_municipio" = municipio,
         "distritol" = id_distrito_loc,
         "nombre_distritol" = distrito_loc,
         "nominal" = lista_nominal,
         "total" = total_votos,
         fxm = fsm,
         panal = pna
  )%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         id_casilla = formatC(id_casilla, width = 2, flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2, flag = "0"),
         distritol = formatC(distritol, width = 2, flag = "0"),
         tipo_eleccion = "ORDINARIA")


pm21 <- pm21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm21)

# sufijo para join

final_pm21_chis <- insertar_sufijo(bd=pm21, "pm", "21")

final_pm21_chis <- final_pm21_chis %>%
  mutate(estado = "07",
         nombre_estado = "CHIAPAS",
         clave_casilla = paste0(estado,seccion,casilla,id_casilla,ext_contigua))


# guardar rda

final_pm21_chis


## EXT PM 22 CHIAPAS -------------------------------------

pmext22 <- bd_pmext_22_chis   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))

# revisar nombres de varianles

colnames(pmext22)

pmext22 <- pmext22 %>%
  rename("estado" = estado,
         "noreg"= no_reg,
         "municipio" = municipio,
         "nombre_municipio" = nombre_municipio,
         "total" = votos,
         "noreg" = no_reg,
         "distritol" = distritol,
         "nombre_distritol" = nombre_distritol,
         fxm = fsm)%>%
  mutate(seccion = substr(clave_casilla, 3,6)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         estado = formatC(estado, width = 2, flag = "0"),
         seccion = formatC(seccion, width = 4,flag = "0"),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritol = formatC(distritol, width = 2, flag = "0"),
         tipo_eleccion = "EXTRAORDINARIA")


pmext22 <- pmext22 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pmext22)

# sufijo para join

final_pmext22_chis <- insertar_sufijo(bd=pmext22, "pm", "21")

final_pmext22_chis


# guardar rda

final_pmext22_chis

rm(pmext22)


# BIND PM 21 Y PMEXT22 ----------------------------------------------------------------------------------------

chis_pm_21 <- final_pm21_chis %>% anti_join(final_pmext22_chis, by = "seccion")

chis_pm_21 <- chis_pm_21 %>% rbind(final_pmext22_chis,fill = T)

chis_pm_21%>% write_rds("inst/electoral/chis/pm_21.rda")


# PM 18 CHIAPAS ------------------------------------------

pm18 <- bd_pm_18_chis   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))

# revisar nombres de varianles

colnames(pm18)

pm18 <- pm18 %>%
  rename("id_estado" = id_estado,
         "noreg"= num_votos_can_nreg,
         "municipio" = id_municipio,
         "nombre_municipio" = municipio,
         "nominal" = lista_nominal,
         "total" = total_votos,
         "nombre_distritol" = cabecera_distrital_local,
         "distritol" = id_distrito_local,
         "pes" = es,
         "pt_morena_pes" = pt_morena_es,
         "pt_pes" = pt_es,
         "morena_pes" = morena_es,
         panal = na,
         c_comun_pri_pvem_panal_pcu = c_comun_pri_pvem_na_pcu,
         pri_pvem_panal = pri_pvem_na,
         pri_panal_pcu = pri_na_pcu,
         pvem_panal_pcu = pvem_na_pcu,
         pri_panal = pri_na,
         pvem_panal = pvem_na,
         panal_pcu = na_pcu)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         id_casilla = formatC(id_casilla, width = 2, flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2, flag = "0"),
         distritol = formatC(distritol, width = 2, flag = "0"))


pm18 <- pm18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm18)

# sufijo para join

final_pm18_chis <- insertar_sufijo(bd=pm18, "pm", "18")

final_pm18_chis <- final_pm18_chis %>%
  mutate(estado = "07",
         nombre_estado = "CHIAPAS",
         clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla,ext_contigua),
         tipo_eleccion = "ORDINARIA")


# guardar rda

final_pm18_chis

rm(pm18)


# EXT PM 18 CHIAPAS ------------------------------------------

pmext_18 <- bd_pmext_18_chis   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))

# revisar nombres de varianles

colnames(pmext_18)

pmext_18 <- pmext_18 %>%
  rename("noreg"= no_reg,
         "municipio" = municipio,
         "nombre_municipio" = nombre_municipio,
         "total" = votos)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         tipo_eleccion = "EXTRAORDINARIA")


pmext_18 <- pmext_18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pmext_18)

# sufijo para join

final_pmext_18_chis <- insertar_sufijo(bd=pmext_18, "pm", "18")


# guardar rda

final_pmext_18_chis


rm(pmext_18)

# BIND PM 18 Y PMEXT18 ----------------------------------------------------------------------------------------

chis_pm_18 <- final_pm18_chis %>% anti_join(final_pmext_18_chis, by = "seccion")


chis_pm_18 <- chis_pm_18 %>% rbind(final_pmext_18_chis,fill = T)

chis_pm_18 %>% write_rds("inst/electoral/chis/pm_18.rda")



# PM 15 CHIAPAS ------------------------------------------

pm15 <- bd_pm_15_chis  %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(pm15)

pm15 <- pm15 %>%
  rename(noreg = num_votos_can_nreg,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         municipio = id_municipio,
         nombre_municipio = municipio,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         panal = nva_alianza,
         pri_pvem_panal_pcu = pri_pvem_nva_alianza_pcu,
         pri_pvem_panal = pri_pvem_nva_alianza,
         pri_panala_pcu = pri_nva_alianza_pcu ,
         pvem_panal_pcu = pvem_nva_alianza_pcu,
         pri_panal = pri_nva_alianza,
         pvem_panal = pvem_nva_alianza,
         panal_pcu = nva_alianza_pcu,
         independiente1 = cand_ind1,
         independiente2 = cand_ind2,
         independiente3 = cand_ind3,
         independiente4 = cand_ind4,
         independiente5 = cand_ind5,
         independiente6 = cand_ind6,
         independiente7 = cand_ind7,
         independiente8 = cand_ind8,
         pes = es
  ) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritol = formatC(distritol, width = 3, flag = "0"))


pm15 <- pm15 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm15)

# sufijo para join

final_pm15_chis <- insertar_sufijo(bd=pm15, "pm", "15")

#agregar clave casillas

final_pm15_chis <- final_pm15_chis  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "07",
         nombre_estado = "Chiapas",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_pm15_chis %>% count(nchar(clave_casilla))

# guardar rda

final_pm15_chis

rm(pm15)


# PM EXTRAORDINARIA 15 CHIAPAS ------------------------------------------

pm15 <- bd_pmext_15_chis  %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(pm15)

pm15 <- pm15 %>%
  rename(noreg = num_votos_can_nreg,
         distritol = id_distrito,
         nombre_distritol = cabecera_distrital,
         municipio = id_municipio,
         nombre_municipio = municipio,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal,
         pes = es
  ) %>%
  mutate(across(prd:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritol = formatC(distritol, width = 3, flag = "0"))


pm15 <- pm15 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = prd:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm15)

# sufijo para join

final_pmext15_chis <- insertar_sufijo(bd=pm15, "pm", "15")

#agregar clave casillas

final_pmext15_chis <- final_pmext15_chis  %>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "07",
         nombre_estado = "Chiapas",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_pmext15_chis %>% count(nchar(clave_casilla))

# JUNTAR 15 Y 15 EXT


chis_pm_15 <- final_pm15_chis %>% anti_join(final_pmext15_chis, by = "clave_casilla")

chis_pm_15 <- chis_pm_15 %>% rbind(final_pmext15_chis,fill = T)

chis_pm_15 %>% write_rds("inst/electoral/chis/pm_15.rda")

rm(pm15)

# GB 18 CHIAPAS ------------------------------------------


# revisar nombres de varianles

colnames(bd_gb_18_chis)

gb_18 <- bd_gb_18_chis %>%
  rename("noreg"= no_reg,
         "total" = total_votos_calculado,
         pes = es,
         pt_morena_pes = pt_morena_es,
         pt_pes = pt_es,
         morena_pes = morena_es,
         pvem = pv,
         pvem_csu_mcs = pv_csu_mcs,
         pvem_csu = pv_csu,
         pvem_mcs = pv_mcs,
         nombre_distritol = distrito_local_nombre,
         distritol = distrito_local)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         estado = formatC(estado, width = 2, flag = "0"),
         distritol = formatC(distritol, width = 2, flag = "0"),
         casilla = formatC(casilla, width = 2, flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2, flag = "0"))


gb_18 <- gb_18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(gb_18)

# sufijo para join

final_gb_18_chis <- insertar_sufijo(bd=gb_18, "gb", "18")

# clave casilla


final_gb_18_chis <- final_gb_18_chis %>% mutate(clave_casilla = substr(clave_casilla,2,nchar(clave_casilla)))

# guardar rda

chis_gb_18 <- final_gb_18_chis

chis_gb_18 %>% write_rds("inst/electoral/chis/gb_18.rda")

rm(gb_18)


# DL 21 CHIAPAS --------------------------------------------

dl21 <- bd_dl_21_chis   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))

# revisar nombres de varianles

colnames(dl21)

dl21 <- dl21 %>%
  rename("estado" = id_estado,
         "noreg"= no_votos_can_nreg,
         "municipio" = id_municipio,
         "nombre_municipio" = municipio,
         "distritol" = id_distrito_local,
         nombre_distritol = cabecera_distrital_local,
         fxm = fsm,
         panal = pna,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal_casilla)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         id_casilla = formatC(id_casilla, width = 2, flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2, flag = "0"),
         distritol = formatC(distritol, width = 2, flag = "0"),
         estado = formatC(estado, width = 2, flag = "0"))


dl21 <- dl21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(dl21)

# sufijo para join

final_dl21_chis <- insertar_sufijo(bd=dl21, "dl", "21")

final_dl21_chis <- final_dl21_chis %>%
  mutate(estado = "07",
         nombre_estado = "CHIAPAS",
         clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla,ext_contigua))


# guardar rda

chis_dl_21 <- final_dl21_chis

chis_dl_21 %>% write_rds("inst/electoral/chis/dl_21.rda")

rm(dl21)





