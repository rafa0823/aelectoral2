############################################### AUXILIARES CHIAPAS ------------------------------------------------------------------------


######### PADRON BENEFICIARIOS CHIAPAS

beneficiarios <- read_csv("inst/bdos_auxiliares_chis/consMpo7.csv") %>%
  janitor::clean_names() %>%
  select("id_estado" = clave_entidad,
         "id_municipio" = clave_municipio,
         "nombre_municipio" = nombe_municipio,
         beneficiarios_unicos)


### REFERENTES MOVILIZACION CHIAPAS

informe_referentes <- read_excel("inst/bdos_auxiliares_chis/informe_v2.xlsx") %>%
  janitor::clean_names()


## municipios con referente

referente_mpos <- informe_referentes %>%
  mutate(referente = paste(referente,apellido_referente),
         operador = paste(nombre_operador,apellido_pat_operador,apellido_mat_operador, sep = " "),
         referente = if_else(is.na(referente),"Sin referente", referente)) %>%
  rename("nombre_municipio_22" = "nom_mun_casilla") %>%
  mutate(referente = paste(referente,apellido_referente,sep = " "),
         operador = paste(nombre_operador,apellido_pat_operador,apellido_mat_operador, sep = " "),
         lider = paste(nombre_lider,apellido_pat_lider,apellido_mat_lider, sep = " ")) %>%
  select(!c(apellido_referente,apellido_pat_operador,apellido_mat_operador,nombre_operador))





