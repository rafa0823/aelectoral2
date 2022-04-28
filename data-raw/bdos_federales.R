
# FEDERALES

# CARGAR BASES ----------------------------------------------------------------------------------------
renv::deactivate()

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

bd_pr_18 <- pr_18 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal/2018/Presidente_casilla.csv")%>%
  janitor::clean_names() %>%
  as_tibble()

bd_df_21 <- read_delim("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/diputaciones.csv",
                       delim = "|", escape_double = FALSE, trim_ws = TRUE,  locale = locale(encoding = "CP1252"),
                       skip = 5) %>%
  janitor::clean_names() %>%
  as_tibble()

bd_df_18 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal/2018/Diputado_casilla.csv") %>%
  janitor::clean_names() %>%
  as_tibble()

bd_df_15 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal/2015/Diputado_casilla.csv") %>%
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
         "distritof_21" = id_distrito,
         "nombre_distritof_21" = nombre_distrito,
         "estado" = id_estado) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         estado = formatC(estado, width = 2,flag = "0"),
         seccion = formatC(seccion, width = 4,flag = "0"),
         id_casilla = formatC(id_casilla, width = 2,flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2,flag = "0"),
         distritof_21 = formatC(distritof_21, width = 2, flag = "0"),
         mr_rp = gsub(pattern = "[[:digit:]]","",num_acta_impreso),
         mr_rp = gsub(pattern = "E","",mr_rp),
         mr_rp = if_else(mr_rp == "","MR",mr_rp),
         seccion = if_else(tipo_casilla == "P","9999",seccion))


df21 <- df21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# revisar nombres de partidos

detectar_partidos(df21)

# Agregar municipios del año

# municipios_df_21 <- read_excel("data-raw/DatosAbiertos-derfe-pdln_edms_sexo_20210415.xlsx") %>%
#   janitor::clean_names() %>%
#   select("estado" = clave_entidad,
#          seccion,
#          "municipio_df_21" = clave_municipio,
#          "nombre_municipio_df_21" = nombre_municipio) %>%
#   mutate(seccion = formatC(seccion, width = 4,flag = "0"),
#          estado = formatC(estado, width = 4,flag = "0"),
#          municipio_df_21 = formatC(municipio_df_21, width = 4,flag = "0")) %>%
#   unique()
#
# df21 <- df21 %>%
#   left_join(municipios_df_21)



# sufijo para join

final_df21 <- insertar_sufijo(bd=df21, "df", "21") %>%
  mutate(clave_casilla = substr(clave_casilla,2,nchar(clave_casilla)-1))

# guardar rda
nac_df_21 <- final_df21

nac_df_21 %>% write_rds("inst/electoral/nac_df_21.rda")

rm(df21)


## presidente 18 ------------------------------------------------------------------

# Identificar nombres de variables y nombres de partidos

pr18 <- bd_pr_18 %>%
  clean_names() %>%
  mutate(clave_casilla = substr(clave_casilla,2,nchar(clave_casilla)-1))


colnames(pr18)


# Homogenizar nombres de variables partidos

pr18 <- pr18  %>%
  rename("noreg" = no_reg,
         "distritof_18" = distrito,
         "nombre_distritof_18" = nombre_distrito,
         "pes" = es,
         "pt_morena_pes" = pt_morena_es,
         "pt_pes" = pt_es,
         "morena_pes" = morena_es) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         estado = formatC(estado, width = 2,flag = "0"),
         seccion = formatC(seccion, width = 4,flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2,flag = "0"),
         distritof_18 = formatC(distritof_18, width = 2, flag = "0"),
         seccion = if_else(tipo_casilla == "P","9999",seccion))

pr18 <- pr18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# revisar nombres de partidos

detectar_partidos(pr18)

# Agregar municipios del año

# municipios_pr_18 <- read_excel("data_raw/DatosAbiertos-derfe-pdln_edms_sexo_20210415.xlsx") %>%
#   janitor::clean_names() %>%
#   select("estado" = clave_entidad,
#          seccion,
#          "municipio" = clave_municipio,
#          nombre_municipio) %>%
#   mutate(seccion = formatC(seccion, width = 4,flag = "0")) %>%
#   unique()
#
# pr18 <- pr18 %>%
#   left_join(municipios_pr_18)



# sufijo para join

final_pr18 <- insertar_sufijo(bd=pr18, "pr", "18")


# guardar rda

nac_pr_18 <- final_pr18

nac_pr_18 %>% write_rds("inst/electoral/nac_pr_18.rda")

rm(pr18)


## DIPFED 18 -------------------------------------------------------------


# Identificar nombres de variables y nombres de partidos

df18 <- bd_df_18 %>%
  mutate(clave_casilla = substr(clave_casilla,2,nchar(clave_casilla)-1))

df18[df18 == "-"] <- NA


colnames(df18)

# Homogenizar nombres de variables partidos

df18 <- df18  %>%
  rename("noreg" = no_reg,
         "distritof_18" = distrito,
         "pes" = es,
         "pt_morena_pes" = pt_morena_es,
         "pt_pes" = pt_es,
         "morena_pes" = morena_es) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         estado = formatC(estado, width = 2,flag = "0"),
         seccion = formatC(seccion, width = 4,flag = "0"),
         id_casilla = formatC(id_casilla, width = 2,flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2,flag = "0"),
         distritof_18 = formatC(distritof_18, width = 2, flag = "0"),
         seccion = if_else(tipo_casilla == "P","9999",seccion),
         mr_rp = gsub(pattern = "7","MR",num_acta_impreso),
         mr_rp = gsub(pattern = "8","PR",mr_rp),
         mr_rp = gsub(pattern = "6","MR",mr_rp))



df18 <- df18 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# revisar nombres de partidos

detectar_partidos(df18)

# Agregar municipios del año

# municipios_df_21 <- read_excel("data-raw/DatosAbiertos-derfe-pdln_edms_sexo_20210415.xlsx") %>%
#   janitor::clean_names() %>%
#   select("estado" = clave_entidad,
#          seccion,
#          "municipio_df_21" = clave_municipio,
#          "nombre_municipio_df_21" = nombre_municipio) %>%
#   mutate(seccion = formatC(seccion, width = 4,flag = "0")) %>%
#   unique()
#
# df21 <- df21 %>%
#   left_join(municipios_df_21)


# sufijo para join

final_df18 <- insertar_sufijo(bd=df18, "df", "18")

# guardar rda
nac_df_18 <- final_df18

nac_df_18 %>% write_rds("inst/electoral/nac_df_18.rda")

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
         "distritof_15" = distrito,
         "pes" = ps) %>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         estado = formatC(estado, width = 2,flag = "0"),
         seccion = formatC(seccion, width = 4,flag = "0"),
         id_casilla = formatC(id_casilla, width = 2,flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2,flag = "0"),
         distritof_15 = formatC(distritof_15, width = 2, flag = "0"),
         seccion = if_else(tipo_casilla == "P","9999",seccion),
         mr_rp = if_else(tipo_acta == 2, "MR", ""),
         mr_rp = if_else(tipo_acta == 3, "MR",mr_rp),
         mr_rp = if_else(tipo_casilla == "S" & tipo_acta == 4, "RP",mr_rp))


df15 <- df15 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# revisar nombres de partidos

detectar_partidos(df15)

# Agregar municipios del año

# municipios_df_21 <- read_excel("data-raw/DatosAbiertos-derfe-pdln_edms_sexo_20210415.xlsx") %>%
#   janitor::clean_names() %>%
#   select("estado" = clave_entidad,
#          seccion,
#          "municipio_df_21" = clave_municipio,
#          "nombre_municipio_df_21" = nombre_municipio) %>%
#   mutate(seccion = formatC(seccion, width = 4,flag = "0")) %>%
#   unique()
#
# df21 <- df21 %>%
#   left_join(municipios_df_21)


# sufijo para join

final_df15 <- insertar_sufijo(bd=df15, "df", "15")

# guardar rda
nac_df_15 <- final_df15

nac_df_15 %>% write_rds("inst/electoral/nac_df_15.rda")

rm(df15)

