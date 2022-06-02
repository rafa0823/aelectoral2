# HIDALGO LOCALES

# Cargar bases --------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

#GB16
bd_gb_16_hgo <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2016/Gobernador/hidalgo_normal_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

#PM20
path <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local/2020/Municipio/hgo_normal_casilla.xlsx"

bd_pm_20_hgo <- path %>%
  excel_sheets() %>%
  set_names() %>%
  map_df(~ read_excel(path = path, sheet = .x)%>%
           janitor::clean_names(),
           .id = "Sheet") %>%
  select(!c(x28,x29,x30,x31,x24)) %>%
  select(Sheet:morena_pt_pvem,independiente_1,no_reg:participacion) %>%
  rename(pan_pri_prd_total = pan_pri_prd_cc,
         morena_pt_total = morena_pt_pvem)



bd_dl_21_dgo %>% filter(Sheet == "01. D. DURANGO") %>% summarise(sum(nominal))



## GB 21 HIDALGO ------------------------------------------------------------------------------------------------------

gb16 <- bd_gb_16_hgo   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))%>%
  select(!num_votos_validos)

# revisar nombres de varianles

colnames(gb16)

gb16 <- gb16 %>%
  rename(noreg = num_votos_can_nreg,
         distritol_16 = id_distrito,
         nombre_distritol_16 = cabecera_distrital,
         municipio_16 = id_municipio,
         nombre_municipio_16 = municipio,
         panal = nva_alianza,
         pri_pvem_panal = pri_pvem_nva_alianza,
         pri_panal = pri_nva_alianza,
         pvem_panal = pvem_nva_alianza,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal
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

final_gb16_hgo <- insertar_sufijo(bd=gb16, "gb", "16")

#agregar clave casillas

final_gb16_hgo <- final_gb16_hgo  %>%
  mutate(casilla = gsub(" ","",casilla),
         id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),

         estado = "13",
         nombre_estado = "HIDALGO",
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(estado,seccion,id_casilla))

#prueba

final_gb16_hgo %>% count(casilla)

final_gb16_hgo %>% count(nchar(clave_casilla))

# guardar rda

hgo_gb_16 <- final_gb16_hgo

hgo_gb_16 %>% write_rds("inst/electoral/hgo/gb_16.rda")

rm(gb16)
