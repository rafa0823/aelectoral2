
renv::deactivate()
library(pacman)
pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

bd_pr_18 <- pr_18 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal/2018/Presidente_casilla.csv")%>%
  janitor::clean_names() %>%
  as_tibble()

bd_df_21 <- read_delim("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/diputaciones.csv",
                       delim = "|", escape_double = FALSE, trim_ws = TRUE,  locale = locale(encoding = "CP1252"),
                       skip = 5) %>%
  janitor::clean_names() %>%
  as_tibble()

bd_pm_21_ch <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Chiapas/2021_AYUN_LOC_MR_CHIS_CAS.xlsx",
                          n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()

bd_pm_21_mex <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Estado de México/Casillas_computo_municipio_por_partido_politico_2021.xlsx",
                           n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()

bd_dl_21_mex <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Estado de México/Casillas_computo_distrito_por_partido_politico_2021.xlsx") %>%
  janitor::clean_names() %>%
  as_tibble()


# dipfed 21 ------------------------------------------------------------------

# Identificar nombres de variables y nombres de partidos

df21 <- bd_df_21%>%
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
         "estado" = id_estado)


df21 <- df21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# revisar nombres de partidos

detectar_partidos(df21)

# Agregar municipios del año

municipios_df_21 <- read_excel("data-raw/DatosAbiertos-derfe-pdln_edms_sexo_20210415.xlsx") %>%
  janitor::clean_names() %>%
  select("estado" = clave_entidad,
         seccion,
         "municipio_df_21" = clave_municipio,
         "nombre_municipio_df_21" = nombre_municipio) %>%
  mutate(seccion = formatC(seccion, width = 4,flag = "0")) %>%
  unique()

df21 <- df21 %>%
  left_join(municipios_df_21)



# sufijo para join

final_df21 <- insertar_sufijo(bd=df21, "df", "21") %>%
  mutate(clave_casilla = substr(clave_casilla,2,nchar(clave_casilla)-1))

# guardar rda
nac_df_21 <- final_df21
usethis::use_data(nac_df_21,overwrite = T)


rm(df21)


# presidente 18 ------------------------------------------------------------------

# Identificar nombres de variables y nombres de partidos

pr18 <- bd_pr_18%>%
  clean_names()

colnames(pr18)


# Homogenizar nombres de variables partidos

pr18 <- pr18  %>%
  rename("noreg" = no_reg,
         "distritof" = distrito,
         "nombre_distritof" = nombre_distrito,
         "pes" = es,
         "pt_morena_pes" = pt_morena_es,
         "pt_pes" = pt_es,
         "morena_pes" = morena_es)%>%
  mutate(seccion = formatC(seccion, width = 4,flag = "0"))


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

final_pr18 <- insertar_sufijo(bd=pr18, "pr", "18")  %>%
  mutate(clave_casilla = substr(clave_casilla,2,nchar(clave_casilla)-1))



# guardar rda
nac_pr_18 <- final_pr18
usethis::use_data(nac_pr_18,overwrite = T)

rm(pr18)



# pm 21 edomex------------------------------------------------------------------

pm21 <- bd_pm_21_mex   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = nombre_municipio))%>%
  mutate(seccion = formatC(seccion, width = 4,flag = "0"))

# revisar nombres de varianles

colnames(pm21)

pm21 %>%
  rename("noreg"="no_reg")


pm21 <- pm21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm21)

# sufijo para join

final_pm21_mex <- insertar_sufijo(bd=pm21, "pm", "21")

final_pm21_mex <- final_pm21_mex %>%
  mutate(casilla,
         clave_casilla = case_when(nchar(casilla) == 1 ~ paste0(casilla,"0100"),
                                   nchar(casilla) == 3 ~ paste0(casilla,"00"),
                                   nchar(casilla) == 6 ~ gsub(pattern = "C","",casilla),
                                   nchar(casilla) == 2 ~ paste0(gsub(pattern = "S", "S0",casilla), "00")))%>%
  mutate(estado = 15,
         nombre_estado = "MÉXICO",
         clave_casilla = paste0(estado,seccion,clave_casilla))


# guardar rda

mex_pm_21 <- final_pm21_mex
usethis::use_data(mex_pm_21,overwrite = T)




rm(pm21)

