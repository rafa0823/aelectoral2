
# FEDERALES

# CARGAR BASES ----------------------------------------------------------------------------------------
#renv::deactivate()

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# pr 18

bd_pr_18 <- pr_18 <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal/2018/Presidente_casilla.xlsx")%>%
  janitor::clean_names() %>%
  as_tibble()

# df_18

setwd("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal/2018/diputados_federales")


bd_df_18 <- list.files(full.names = T) %>%
  map_dfr(~{
    print(.x)
    read_excel(.x) %>%
      janitor::clean_names() %>%
      mutate(
        mr_rp = toupper(substr(gsub("./diputados_","",.x),1,2)))
  })

bd_df_18 %>% names()

setwd("~/Documents/Git/aelectoral2")

# df 21

bd_df_21 <- read_delim("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/diputaciones.csv",
                       delim = "|", escape_double = FALSE, trim_ws = TRUE,  locale = locale(encoding = "CP1252"),
                       skip = 5) %>%
  janitor::clean_names() %>%
  as_tibble()

bd_df_15 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal/2015/Diputado_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

bd_df_12 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal/2012/Diputado_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()


# FEDERALES -----------------------------------------------------------------------

## dipfed 21 ------------------------------------------------------------------

# Identificar nombres de variables y nombres de partidos

df21 <- bd_df_21 %>%
  clean_names()


colnames(df21)

# Homogenizar nombres de variables partidos

df21 <- df21  %>%
  rename("noreg" = candidato_a_no_registrado_a,
         "nulos" = votos_nulos,
         "total" = total_votos_calculados,
         "nominal" = lista_nominal_casilla,
         "distritof" = id_distrito,
         "nombre_distritof" = nombre_distrito,
         "estado" = id_estado) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         estado = formatC(estado, width = 2,flag = "0"),
         seccion = formatC(seccion, width = 4,flag = "0"),
         id_casilla = formatC(id_casilla, width = 2,flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2,flag = "0"),
         distritof = formatC(distritof, width = 2, flag = "0"),
         mr_rp = gsub(pattern = "[[:digit:]]","",num_acta_impreso),
         mr_rp = gsub(pattern = "E","",mr_rp),
         mr_rp = if_else(mr_rp == "","MR",mr_rp),
         seccion = if_else(tipo_casilla == "P","9999",seccion),
         nombre_distritof = if_else(tipo_casilla == "P","PREVENTIVA",nombre_distritof))


df21 <- df21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# revisar nombres de partidos

detectar_partidos(df21)



# sufijo para join

final_df21 <- insertar_sufijo(bd=df21, "df", "21") %>%
  mutate(clave_casilla = substr(clave_casilla,2,nchar(clave_casilla)-1))

# nchar casillas

final_df21 %>% count(nchar(clave_casilla))

# guardar rda

nac_df_21 <- final_df21

nac_df_21 %>% write_rds("inst/electoral/nacional/df_21.rda")

rm(df21)


## presidente 18 ------------------------------------------------------------------

# Identificar nombres de variables y nombres de partidos

pr18 <- bd_pr_18

colnames(pr18)

pr18 <- bd_pr_18   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))

# revisar nombres de varianles

colnames(pr18)

pr18 <- pr18 %>%
  rename(noreg = num_votos_can_nreg,
         distritof = id_distrito,
         nombre_distritof = cabecera_distrital,
         panal =  na,
         nombre_municipio = municipio,
         municipio = id_municipio,
         panal = na,
         pes = es,
         pri_pvem_panal = pri_pvem_na,
         pt_morena_pes = pt_morena_es,
         pvem_panal = pvem_na,
         pri_panal = pri_na,
         pt_pes = pt_es,
         morena_pes = morena_es,
         pt_pes = pt_es,
         ind1 = cand_ind1,
         ind2 = cand_ind2,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal
  ) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritof = formatC(distritof, width = 3, flag = "0"),
         id_estado = formatC(id_estado, width = 2, flag = "0"))


pr18 <- pr18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pr18)

# sufijo para join

final_pr18 <- insertar_sufijo(bd=pr18, "pr", "18")

#agregar clave casillas

final_pr18 <- final_pr18%>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(id_estado,seccion,id_casilla))

# se va voto en el extranjero volver a poner cuando se pueda

final_pr18 <- final_pr18 %>% filter(!grepl("MEC", casilla))

#prueba

final_pr18 %>% count(nchar(clave_casilla))


# guardar rda

pr_18 <- final_pr18

pr_18 %>% write_rds("inst/electoral/nacional/pr_18.rda")

rm(pr18)



## DIPFED 18 -------------------------------------------------------------

# Identificar nombres de variables y nombres de partidos

df18 <- bd_df_18

colnames(df18)

df18 <- bd_df_18   %>%
  mutate(municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))

# revisar nombres de varianles

colnames(df18)

df18 <- df18 %>%
  rename(noreg = num_votos_can_nreg,
         distritof = id_distrito,
         nombre_distritof = cabecera_distrital,
         panal =  na,
         nombre_municipio = municipio,
         municipio = id_municipio,
         panal = na,
         pes = es,
         pri_pvem_panal = pri_pvem_na,
         pt_morena_pes = pt_morena_es,
         pvem_panal = pvem_na,
         pri_panal = pri_na,
         pt_pes = pt_es,
         morena_pes = morena_es,
         pt_pes = pt_es,
         nulos = num_votos_nulos,
         total = total_votos,
         nominal = lista_nominal
  ) %>%
  rename_with( ~ gsub("cand_", "", .x, fixed = T)) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         municipio = formatC(municipio, width = 3, flag = "0"),
         distritof = formatC(distritof, width = 3, flag = "0"),
         id_estado = formatC(id_estado, width = 2, flag = "0"))


df18 <- df18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(df18)

# sufijo para join

final_df18 <- insertar_sufijo(bd=df18, "df", "18")

#agregar clave casillas

final_df18 <- final_df18%>%
  mutate(id_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                nchar(casilla) == 5 ~ paste0("S",substr(casilla,4,5),"00")),
         tipo_casilla = substr(casilla,1,1),
         clave_casilla = paste0(id_estado,seccion,id_casilla))

# se va voto en el extranjero volver a poner cuando se pueda

final_df18 <- final_df18 %>% filter(!grepl("MEC", casilla))

#prueba

final_df18 %>% count(nchar(clave_casilla))


# guardar rda

df_18 <- final_df18 %>% as_tibble()

df_18 %>% saveRDS("inst/electoral/nacional/df_18.rda")

rm(df18)


## DIPFED 15 --------------------------------------------------------------------------


# Identificar nombres de variables y nombres de partidos

df15 <- bd_df_15%>%
  mutate(clave_casilla = paste0(estado,seccion,tipo_casilla,id_casilla,ext_contigua))

df15[df15 == "-"] <- NA


colnames(df15)

# Homogenizar nombres de variables partidos

df15 <- df15  %>%
  rename("noreg" = no_reg,
         "distritof" = distrito,
         "pes" = ps) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         estado = formatC(estado, width = 2,flag = "0"),
         seccion = formatC(seccion, width = 4,flag = "0"),
         id_casilla = formatC(id_casilla, width = 2,flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2,flag = "0"),
         distritof = formatC(distritof, width = 2, flag = "0"),
         seccion = if_else(tipo_casilla == "P","9999",seccion),
         mr_rp = if_else(tipo_acta == 2, "MR", ""),
         mr_rp = if_else(tipo_acta == 3, "MR",mr_rp),
         mr_rp = if_else(tipo_casilla == "S" & tipo_acta == 4, "RP",mr_rp))


df15 <- df15 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# revisar nombres de partidos

detectar_partidos(df15)


# sufijo para join

final_df15 <- insertar_sufijo(bd=df15, "df", "15")

# guardar rda
nac_df_15 <- final_df15

nac_df_15 %>% write_rds("inst/electoral/nacional/df_15.rda")

rm(df15)


## DIPFED 12 --------------------------------------------------------------------------


# Identificar nombres de variables y nombres de partidos

df12 <- bd_df_12

df12[df12 == "NA"] <- NA


colnames(df12)

# Homogenizar nombres de variables partidos

df12 <- df12  %>%
  select(clave_casilla:orden,
         municipio,
         pan:pt_mc,
         no_reg,
         nulos,
         lista_nominal,
         votos_reservados,
         id_grupo,
         tipo_recuento) %>%
  rename("noreg" = no_reg,
         distritof = distrito,
         nominal = lista_nominal) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         estado = formatC(estado, width = 2,flag = "0"),
         seccion = formatC(seccion, width = 4,flag = "0"),
         id_casilla = formatC(id_casilla, width = 2,flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2,flag = "0"),
         distritof = formatC(distritof, width = 2, flag = "0"),
         seccion = if_else(tipo_casilla == "P","9999",seccion),
         mr_rp = if_else(tipo_candidatura == 2, "MR", "RP"))



df12 <- df12 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# revisar nombres de partidos

detectar_partidos(df12)

# Agregar municipios del año

# municipios_df_12 <- read_excel("data-raw/DatosAbiertos-derfe-pdln_edms_sexo_20120415.xlsx") %>%
#   janitor::clean_names() %>%
#   select("estado" = clave_entidad,
#          seccion,
#          "municipio_df_12" = clave_municipio,
#          "nombre_municipio_df_12" = nombre_municipio) %>%
#   mutate(seccion = formatC(seccion, width = 4,flag = "0")) %>%
#   unique()
#
# df12 <- df12 %>%
#   left_join(municipios_df_12)


# sufijo para join

final_df12 <- insertar_sufijo(bd=df12, "df", "12")

# guardar rda
nac_df_12 <- final_df12

nac_df_12 %>% write_rds("inst/electoral/nacional/df_12.rda")

rm(df12)





# Cambiar id_estado por estado --------------------------------------------
library(tidyverse)

df_18 <- read_rds("inst/electoral/nacional/df_18.rda")
df_18 %>% rename(estado = id_estado) %>% saveRDS("inst/electoral/nacional/df_18.rda")
