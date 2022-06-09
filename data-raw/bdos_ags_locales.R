# AGUASCALIENTES LOCALES

# Cargar bases --------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

bd_gb_16_ags <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/aguascalientes_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

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

setwd("~/Documents/Git/aelectoral2")



## GB 21 AGUASCALIENTES ------------------------------------------------------------------------------------------------------

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

### DL 21 AGUASCALIENTES -------------------------------------------------------------------------------------


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
         tipo_casilla = substr(casilla,1,1),
         id_casilla = if_else(casilla == "B","01",
                           formatC(as.numeric(gsub("[[:alpha:]]","",casilla)),width = 2,flag = "0")),
         ext_con = if_else(is.na(ext_con),"00",gsub("C","0",ext_con)),
         estado = "01",
         nombre_estado = "AGUASCALIENTES",
         clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla,ext_con))

#pruebas

final_dl21_ags %>% count(nchar(clave_casilla))
final_dl21_ags %>% count(casilla)

# guardar rda

ags_dl_21 <- final_dl21_ags

ags_dl_21 %>% write_rds("inst/electoral/ags/dl_21.rda")

rm(dl21)




