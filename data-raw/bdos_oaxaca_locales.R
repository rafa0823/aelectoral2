# OAXACA LOCALES

# Cargar bases --------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# GB 16

bd_gb_16_oaxaca <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/oaxaca_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

#DL 16

bd_dl_16_oaxaca <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Distrito local/oaxaca_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()




## GB 21 OAXACA ------------------------------------------------------------------------------------------------------

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
