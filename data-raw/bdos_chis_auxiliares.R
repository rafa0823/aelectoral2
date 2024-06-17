############################################### AUXILIARES CHIAPAS ------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

######### PADRON BENEFICIARIOS CHIAPAS

beneficiarios <- read_csv("../revocacion/data_raw/consMpo7.csv") %>%
  janitor::clean_names() %>%
  select("id_estado" = clave_entidad,
         "id_municipio" = clave_municipio,
         "nombre_municipio" = nombe_municipio,
         beneficiarios_unicos)

beneficiarios %>% write_rds("inst/bdos_auxiliares_chis/beneficiarios.rda")


### REFERENTES MOVILIZACION CHIAPAS

informe_referentes <- read_excel("../revocacion/data_raw/informe_v2.xlsx") %>%
  janitor::clean_names()

referente_mpos <- informe_referentes %>%
  mutate(referente = paste(referente,apellido_referente),
         operador = paste(nombre_operador,apellido_pat_operador,apellido_mat_operador, sep = " "),
         referente = if_else(is.na(referente),"Sin referente", referente)) %>%
  rename("nombre_municipio_22" = "nom_mun_casilla") %>%
  mutate(referente = paste(referente,apellido_referente,sep = " "),
         operador = paste(nombre_operador,apellido_pat_operador,apellido_mat_operador, sep = " "),
         lider = paste(nombre_lider,apellido_pat_lider,apellido_mat_lider, sep = " ")) %>%
  select(!c(apellido_referente,apellido_pat_operador,apellido_mat_operador,nombre_operador))


referente_mpos %>% write_rds("inst/bdos_auxiliares_chis/referente_mpos.rda")


# ALIANZAS

alianzas_chis_pm_21 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_21_chiapas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "07",
         nombre_estado = "CHIAPAS",
         municipio_21 = formatC(municipio_21, width = 3, flag = "0"),
         candidatura_comun = NA,
         tipo_eleccion = "ORDINARIA",
         año = "2021")

# extraordinarias chiapas 22

alianzas_chis_pmextra_22 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_extra_22_chiapas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(eleccion = "pm_21",
         estado = "07",
         nombre_estado = "CHIAPAS",
         municipio_22 = formatC(municipio_22, width = 3, flag = "0"),
         candidatura_comun = NA,
         tipo_eleccion = "EXTRAORDINARIA",
         año = "2022") %>%
  rename(municipio_21 = municipio_22)

# nuntar alianzas pm 21 y pm extra 22

alianzas_chis_pm_21 %>% rbind(alianzas_chis_pmextra_22) %>% write_rds("inst/alianzas/chis/pm_21.rda")

alianzas_chis_dl_21 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/dl_21_chiapas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "07",
         nombre_estado = "CHIAPAS",
         distritol_21 = formatC(distritol_21, width = 2, flag = "0"))

alianzas_chis_dl_21 %>% write_rds("inst/alianzas/chis/dl_21.rda")

# PM 15

alianzas_chis_pm_15 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_15_chiapas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "07",
         nombre_estado = "CHIAPAS",
         municipio_15 = formatC(municipio_15, width = 3, flag = "0"),
         candidatura_comun = NA)

alianzas_chis_pm_15 %>% write_rds("inst/alianzas/chis/pm_15.rda")

# gb 18

alianzas_chis_gb_18 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_18_chiapas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "07",
         nombre_estado = "CHIAPAS")

alianzas_chis_gb_18 %>% write_rds("inst/alianzas/chis/gb_18.rda")

alianzas_chis_gb_18 %>% count(coaliciones)

alianzas_chis_pm_18 <- read.csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_18_chiapas.csv") %>%
  janitor::clean_names() %>%
  as_tibble() %>%
  mutate(estado = "07",
         municipio_21 = formatC(municipio_21, width = 3, flag = "0"),
         nombre_estado = "CHIAPAS") %>%
  rename(municipio_18 = municipio_21,
         coaliciones = coaliaciones)

alianzas_chis_pm_18 %>% write_rds("inst/alianzas/chis/pm_18.rda")

alianzas_chis_pm_18 %>% count(coaliciones)

# Alianzas 24 -------------------------------------------------------------
## code to prepare `bdos_chih_auxiliares` dataset goes here
# Alianza PM 24 -----------------------------------------------------------
library(tidyverse)
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/PREP/Locales"
files <- list.files(path, recursive = T, pattern = ".csv",full.names = T)
files_cand <- subset(files, grepl("AYUN_CAND|AYUN_Cand", files))

## Se debe de seguir la siguiente estructura: eleccion, estado, nombre_estado, municipio, coaliciones y un booleano de si es Candidatura Común
entidad <- "chis"

dicc <- aelectoral2::diccionario |>
  mutate(id_estado = sprintf("%02s", id_estado)) |>
  select(-abreviatura, nombre_estado = estado)

pm_24 <- readr::read_csv(files_cand[[5]]) |>
  janitor::clean_names() |>
  rename_with(~gsub("_local", "", .x), contains("_local")) |>
  select(-contains("suplente")) |>
  rename_with(~gsub("_propietaria", "", .x), contains("_propietaria")) |>
  rename_with(~gsub("id_", "", .x), contains("id")) |>
  arrange(as.numeric(municipio)) |>
  mutate(candidatura = if_else(candidatura == "SIN REGISTRO", NA, candidatura)) |>
  na.omit() |>
  filter(!grepl("IND", partido_ci)) |>
  filter((n() > 1 | grepl("-|_", partido_ci)), .by = candidatura) |>
  filter(nchar(partido_ci) == max(nchar(partido_ci)), .by = candidatura) |>
  reframe(partido_ci = paste(partido_ci, collapse = "_"), .by = c(municipio, candidatura)) |>
  transmute(eleccion = "pm_24",
            estado = sprintf("%02s", entidad),
            municipio = sprintf("%03s", municipio),
            coalicion = tolower(gsub("^CC_|^COA_|^C_", "", partido_ci)),
            coalicion = gsub("-", "_", coalicion),
            candidatura_comun = if_else(grepl("CC_", partido_ci), T, NA)
  ) |>
  left_join(dicc, join_by(estado == id_estado))

carpetas <- list.files("inst/alianzas/")

if(!entidad %in% carpetas){
  dir.create(glue::glue("inst/alianzas/{entidad}"))
  readr::write_rds(pm_24, glue::glue("inst/alianzas/{entidad}/pm_24.rda"))
} else{
  readr::write_rds(pm_24, glue::glue("inst/alianzas/{entidad}/pm_24.rda"))
}
