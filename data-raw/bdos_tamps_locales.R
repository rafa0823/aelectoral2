# TAMPS LOCALES

# Cargar bases --------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

bd_pm_21_tamps <- list.files("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2021/Municipio/Tamaulipas",
                             full.names = T) %>%
map_df(~{
    print(.x)
    read_csv(.x,skip = 6) %>% janitor::clean_names() %>%
      mutate(pan = as.numeric(pan))
  }) %>% filter(!is.na(casilla))

bd_gb_16_tamps <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/tamaulipas_normal_casilla.csv")


## PM 21 TAMAULIPAS ------------------------------------------------------------------------------------------------------

pm21 <- bd_pm_21_tamps   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))

# revisar nombres de varianles

colnames(pm21)

pm21 <- pm21 %>%
  rename(noreg = candidatos_no_registrados,
         nulos = votos_nulos
  )%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio_21 = formatC(municipio_21, width = 3, flag = "0"))


pm21 <- pm21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm21)

# sufijo para join

final_pm21_tamps <- insertar_sufijo(bd=pm21, "pm", "21")

final_pm21_tamps <- final_pm21_tamps  %>%
  mutate(clave_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                   nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                   nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla)),

         estado = "10",
         nombre_estado = "DURANGO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,clave_casilla))


# guardar rda

tamps_pm_21 <- final_pm21_tamps

tamps_pm_21 %>% write_rds("inst/electoral/tamps_pm_21.rda")

rm(pm21)

