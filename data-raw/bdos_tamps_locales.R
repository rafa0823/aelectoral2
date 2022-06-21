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


bd_gb_16_tamps <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/tamaulipas_normal_casilla.csv") %>%
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



