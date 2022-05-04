# DURANGO LOCALES

## CARGAR BASES ------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

bd_pm_19_dgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2019/Municipio/durango_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

bd_dl_18_dgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2018/Distrito local/durango_dl_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

#### gb 16 por seccion
bd_gb_16_dgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/durango_gb_16_normal_seccion.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

bd_pm_16_dgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Municipio/durango_pm_16_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

bd_dl_16_dgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Distrito local/durango_dl_16_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

## PM 19 DURANGO ------------------------------------------------------------------------------------------------------

pm19 <- bd_pm_19_dgo   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))

# revisar nombres de varianles

colnames(pm19)

pm19 <- pm19 %>%
  rename("noreg"= no_reg,
         "municipio_19" = municipio,
         "nombre_municipio_19" = nombre_municipio
  )%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_19 = formatC(municipio_19, width = 3, flag = "0"))


pm19 <- pm19 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm19)

# sufijo para join

final_pm19_dgo <- insertar_sufijo(bd=pm19, "pm", "19")

final_pm19_dgo <- final_pm19_dgo  %>%
  mutate(clave_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                   nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                   nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla)),

         estado = "10",
         nombre_estado = "DURANGO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,clave_casilla))


# guardar rda

dgo_pm_19 <- final_pm19_dgo

dgo_pm_19 %>% write_rds("inst/electoral/dgo_pm_19.rda")

rm(pm19)


## gb 16 DURANGO  ------------------------------------------------------------------------------------------------------

gb16 <- bd_gb_16_dgo   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio),
         pan = 0)

# revisar nombres de varianles

colnames(gb16)

gb16 <- gb16 %>%
  rename("noreg"= no_reg,
         "municipio_16" = municipio,
         "nombre_municipio_16" = nombre_municipio,
         distritol_16 = distrito,
         nombre_distritol_16 = cabecera_distrital,
         pes = es
  )%>%
  mutate(across(pan_prd:pan, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_16 = formatC(municipio_16, width = 3, flag = "0"),
         distritol_16 = formatC(distritol_16, width = 2, flag = "0"))


gb16 <- gb16 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan_prd:pan)

# Identificar los partidos de la elecccion
detectar_partidos(gb16)

# sufijo para join

final_gb16_dgo <- insertar_sufijo(bd=gb16, "gb", "16")

final_gb16_dgo <- final_gb16_dgo  %>%
  mutate(casilla = formatC(casilla, width = 2,flag = "0"),
         tipo_casilla = "X",
         estado = "10",
         nombre_estado = "DURANGO",
         clave_casilla = paste0(estado,seccion,tipo_casilla,casilla,"XX"))


# guardar rda

dgo_gb_16 <- final_gb16_dgo

dgo_gb_16 %>% write_rds("inst/electoral/dgo_gb_16.rda")

rm(gb16)

## PM 16 DURANGO ------------------------------------------------------------------------------------------------------

pm16 <- bd_pm_16_dgo   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio),
         pan = 0)


# revisar nombres de varianles

colnames(pm16)

pm16 <- pm16 %>%
  rename("noreg"= no_reg,
         "municipio_16" = municipio,
         "nombre_municipio_16" = nombre_municipio,
         distritol_16 = distrito,
         nombre_distritol_16 = nombre_distrito,
  )%>%
  mutate(across(pan_prd:pan, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_16 = formatC(municipio_16, width = 3, flag = "0"),
         distritol_16 = formatC(distritol_16, width = 2, flag = "0"))


pm16 <- pm16 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan_prd:pan)

# Identificar los partidos de la elecccion
detectar_partidos(pm16)

# sufijo para join

final_pm16_dgo <- insertar_sufijo(bd=pm16, "pm", "16")

final_pm16_dgo <- final_pm16_dgo  %>%
  mutate(clave_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                   nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                   nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                   nchar(casilla) == 5 ~ paste0(gsub(pattern = "[MR]","",casilla),"00")),
         mr_rp = if_else(nchar(casilla) == 5, gsub(pattern = "[[:digit:]]","",casilla),""),
         mr_rp = gsub(pattern = "S","",mr_rp),
         mr_rp = if_else(mr_rp == "","MR",mr_rp),
         estado = "10",
         nombre_estado = "DURANGO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,clave_casilla))


# guardar rda

dgo_pm_16 <- final_pm16_dgo

dgo_pm_16 %>% write_rds("inst/electoral/dgo_pm_16.rda")

rm(pm16)
