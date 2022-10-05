# QUINTANA ROO LOCALES

# Cargar bases --------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)
















#gb 16

bd_gb_16_q_roo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/quintanaroo_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()





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

