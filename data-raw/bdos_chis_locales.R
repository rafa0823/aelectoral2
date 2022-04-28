
# CHIAPAS LOCALES

# CARGAR BASES -------------------------------------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

bd_pm_21_chis <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Chiapas/2021_AYUN_LOC_MR_CHIS_CAS.xlsx",
                            n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()

bd_pm_18_chis <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Chiapas/2021_AYUN_LOC_MR_CHIS_CAS.xlsx",
                            n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()

bd_pm_15_chis <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Chiapas/2021_AYUN_LOC_MR_CHIS_CAS.xlsx",
                            n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()

bd_gb_18_chis <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Chiapas/2021_AYUN_LOC_MR_CHIS_CAS.xlsx",
                            n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()

bd_dl_21_chis <- read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/Bases de datos/2021/Computos Distritales/Chiapas/2021_AYUN_LOC_MR_CHIS_CAS.xlsx",
                          n_max = 20036)%>%
  janitor::clean_names() %>%
  as_tibble()



# PM 21 CHIAPAS ------------------------------------------

pm21 <- bd_pm_21_chis   %>%
  mutate(nombre_municipio = gsub(pattern = "( |)[0-9]",replacement = "",x = municipio))

# revisar nombres de varianles

colnames(pm21)

pm21 <- pm21 %>%
  rename("id_estado" = id_estado,
         "estado" = estado,
         "noreg"= no_registrados,
         "id_municipio_pm_21" = id_municipio,
         "municipio_pm_21" = municipio,
         "nombre_municipio_pm_21" = nombre_municipio,
         "nominal" = lista_nominal,
         "total" = total_votos)%>%
  mutate(across(pan:nominal, ~as.numeric(.x)),
         seccion = formatC(seccion, width = 4,flag = "0"),
         seccion = if_else(casilla == "P","9999",seccion),
         id_municipio_pm_21 = formatC(municipio_pm_21, width = 2, flag = "0"),
         id_casilla = formatC(id_casilla, width = 2, flag = "0"),
         ext_contigua = formatC(ext_contigua, width = 2, flag = "0"))


pm21 <- pm21 %>%
  rename_with.(~paste0('ele_', .x),
               .cols = pan:nominal)

# Identificar los partidos de la elecccion
detectar_partidos(pm21)

# sufijo para join

final_pm21_chis <- insertar_sufijo(bd=pm21, "pm", "21")

final_pm21_chis <- final_pm21_mex %>%
  mutate(estado = "15",
         nombre_estado = "CHIAPAS",
         clave_casilla = paste0(id_estado,seccion,id_casilla,ext_contigua))


# guardar rda

chis_pm_21 <- final_pm21_chis

chis_pm_21 %>% write_rds("inst/electoral/chis_pm_21.rda")

rm(pm21)


## EXT PM 21 CHIAPAS -------------------------------------

## FINAL PM21 CHIAPAS -----------------------------------


# PM 18 CHIAPAS ------------------------------------------


# PM 15 CHIAPAS ------------------------------------------


# GB 18 CHIAPAS ------------------------------------------


# DL 21 CHIAPAS --------------------------------------------



############################################### COSAS PARA DOCUMENTO DE CHIAPAS EXTRA ------------------------------------------------------------------------

catalogo_utm <- read_csv("data_raw/CATALOGO_UNIDADES_TERRITORIALES_RM2022.csv",
                         skip = 1, locale = locale(encoding = "CP1252")) %>%
  janitor::clean_names()

ln <- readxl::read_excel("data_raw/DatosAbiertos-derfe-pdln_edms_re_sexo_20220218.xlsx") %>%
  rename_with(~str_replace(string = .x, pattern = "\r\n", replacement = "_")) %>%
  janitor::clean_names()

ln_mexico <- ln %>% filter(nombre_distrito!="0")

definitiva <- catalogo_utm %>%
  full_join(ln_mexico %>%
              select(seccion,
                     nombre_entidad,
                     clave_distrito,
                     lista_nominal),
            by = c("seccion",
                   "entidad" = "nombre_entidad",
                   "id_distrito_federal" = "clave_distrito"))


definitiva <- definitiva %>%
  mutate(utm=paste(id_entidad,
                   id_distrito_federal,
                   unidad_territorial, sep="-"))

definitiva %>% group_by(utm,sede) %>%
  summarise(lista_nominal=sum(lista_nominal),
            seccion=sum(seccion*(sede=="SI"))) %>%
  mutate(cobertura=lista_nominal/sum(lista_nominal)) %>%
  filter(sede=="SI") %>% arrange(cobertura)

######### COMPARAR BENEFICIARIOS CON VOTOS A FAVOR DE AMLO

beneficiarios <- read_csv("data_raw/consMpo7.csv") %>%
  janitor::clean_names() %>%
  select("id_estado" = clave_entidad,
         "id_municipio" = clave_municipio,
         "nombre_municipio" = nombe_municipio,
         beneficiarios_unicos) %>%
  mutate(id_estado = as.numeric(id_estado),
         id_municipio = as.numeric(id_municipio))


edomex_municipal %>%
  group_by(nombre_municipio) %>%
  summarise(cp_continua = sum(cp_continua)) %>%
  left_join(beneficiarios) %>%
  ggplot(aes(y = cp_continua,
             x = beneficiarios_unicos))+
  geom_point()+
  geom_smooth()

### JUNTAR BASE

informe_referentes <- read_excel("data_raw/informe_v2.xlsx") %>%
  janitor::clean_names()


## numero referentes por municipio

informe_referentes %>%
  mutate(referente = paste(referente,apellido_referente),
         operador = paste(nombre_operador,apellido_pat_operador,apellido_mat_operador, sep = " ")) %>%
  select(referente, nom_mun_casilla) %>%
  distinct() %>%
  count(nom_mun_casilla,sort = T) %>%
  count(n)

## municipios con referente

referente_mpos <- informe_referentes %>%
  mutate(referente = paste(referente,apellido_referente),
         operador = paste(nombre_operador,apellido_pat_operador,apellido_mat_operador, sep = " ")) %>%
  select(referente, nom_mun_casilla) %>%
  distinct()


final_referentes <- edomex_municipal %>%
  group_by(nombre_municipio) %>%
  summarise(across(.cols = starts_with("cp_"),.fns = ~sum(.x,na.rm=T))) %>%
  select(!c(cp_participacion,cp_cobertura)) %>%
  left_join(referente_mpos %>%
              rename("nombre_municipio" = "nom_mun_casilla")) %>%
  mutate(referente = if_else(is.na(referente),"Sin referente", referente))



final_referentes %>%
  ggplot() +
  aes(y = reorder(x = referente,cp_continua/cp_lista_nominal, FUN=median, na.rm=T ),
      x = cp_continua/cp_lista_nominal) +
  geom_boxplot() +
  theme_minimal()

informe_referentes %>%
  mutate(referente = paste(referente,apellido_referente,sep = " "),
         operador = paste(nombre_operador,apellido_pat_operador,apellido_mat_operador, sep = " "),
         lider = paste(nombre_lider,apellido_pat_lider,apellido_mat_lider, sep = " ")) %>%
  select(referente, nom_mun_casilla) %>%
  distinct() %>%
  count(referente,sort = T)

final_referentes %>%
  count(referente, sort = T)

informe_referentes %>% select(nom_mun_casilla) %>% unique()





