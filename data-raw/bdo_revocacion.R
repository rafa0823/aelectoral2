
pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# RESULTADOS REVOCACIÓN DE MANDATO 2022

cp22 <- read_csv("../revocacion/data_raw/20220411_1845_COMPUTOS_RM2022.csv", skip = 5,locale = locale(encoding = "CP1252")) %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(seccion = as.numeric(seccion))

cp22[cp22 == "-"] <- NA

cp22 <- cp22  %>%
  mutate(clave_casilla = substr(clave_casilla,2,nchar(clave_casilla)-1)) %>%
  rename("estado_cp_22" = id_entidad,
         "nombre_estado_cp_22" = entidad,
         "distritof_22" = id_distrito_federal,
         "nombre_distritof_22" = distrito_federal,
         "revoca" = que_se_le_revoque_el_mandato_por_perdida_de_la_confianza,
         "continua" = que_siga_en_la_presidencia_de_la_republica,
         "total" = total_votos_calculados,
         "nominal" = lista_nominal) %>%
  mutate(seccion = formatC(seccion, width = 4, flag = "0"),
         distritof_22 = formatC(distritof_22, width = 2, flag = "0"),
         estado_cp_22 = formatC(estado_cp_22, width = 2, flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2, flag = "0"))

# prefijo

cp22 <- cp22 %>%
  rename_with.(~paste0('cp_', .x),
               .cols = revoca:nominal)


# sufijo para join

final_cp22 <- insertar_sufijo(bd=cp22, "22") %>%
  mutate(clave_casilla = substr(clave_casilla,2,nchar(clave_casilla)-1))

# guardar rda

nac_cp_22 <- final_cp22

nac_cp_22 %>% write_rds("inst/electoral/nac_cp_22.rda")

rm(cp22)




#CATALOGO DE UNIDADES TERRITOTIALES 2022

 library(tidyverse)
 cat_utm_22 <- read_delim("../revocacion/data_raw/CATALOGO_UNIDADES_TERRITORIALES_RM2022.csv",
                   delim = ",", escape_double = FALSE, trim_ws = TRUE, locale = locale(encoding = "UTF-8")) %>%
   janitor::clean_names() %>%
   as_tibble()  %>%
   rename("estado" = id_entidad,
          "nombre_estado" = entidad,
          "distritof_22" = id_distrito_federal,
          "nombre_distritof_22" = distrito_federal) %>%
   mutate(seccion = formatC(seccion, width = 4, flag = "0"),
          unidad_territorial = formatC(unidad_territorial, width = 2, flag = "0"),
          distritof_22 = formatC(distritof_22, width = 2, flag = "0"),
          estado = formatC(estado, width = 2, flag = "0"))


 usethis::use_data(cat_utm_22, overwrite = F)

 # REGIONES

 regiones <- read_rds("../revocacion/src/regiones.rda") %>%
   mutate(estado = "15",
          nombre_estado = "MÉXICO")

 usethis::use_data(regiones)


 # PRESIDENTES MUNICIPALES EDOMEX

 presidentes_mpos_mex <- read_excel("../revocacion/data_raw/Presidentes Municipales Chiapas 2022.xlsx") %>% janitor::clean_names() %>%
   rename("nombre_municipio" = municipio) %>%
   mutate(nombre_municipio=toupper(nombre_municipio) %>% stringi::stri_trans_general("Latin-ASCII"),
          estado = "15",
          nombre_estado = "MÉXICO")

 usethis::use_data(presidentes_mpos_mex)


