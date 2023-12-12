## code to prepare `modificacion_censo` dataset goes here
library(readr)
library(dplyr)

# Cargar bases ------------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INEGI/Censo 2020/Datos geografico/ITER_NALCSV20.csv"

# Edomex ------------------------------------------------------------------
## Censo_EDOMEX
censo_mex <- read_csv(path) |>
  janitor::clean_names() |>
  filter(entidad == 15, !is.na(longitud)) |>
  mutate(nom_mun = stringi::stri_trans_general(toupper(nom_mun), id = 'latin-ascii'),
         nom_mun = if_else(nom_mun == "ACAMBAY DE RUIZ CASTANEDA", "ACAMBAY DE RUIZ CASTAÑEDA", nom_mun))
## INE_EDOMEX
mex <- aelectoral2::Electoral$new(eleccion = "pm_21", entidad = "mex", llaves = "municipio_21")
mex_mun <- mex$bd |>
  distinct(municipio_21, nombre_municipio_21)

# Revisar que los nombres coincidan_EDOMEX ---------------------------------------

v1 <- unique(censo_mex$nom_mun)
v2 <- mex_mun$nombre_municipio_21

setdiff(v1, v2)
setdiff(v2, v1)
# Sobreescribir_EDOMEX -----------------------------------------------------------
censo_mex <- censo_mex |>
  left_join(mex_mun, join_by(nom_mun == nombre_municipio_21)) |>
  mutate(municipio_21 = gsub("15_", "", municipio_21)) |>
  distinct(entidad, mun, municipio_21)

# Jalisco -----------------------------------------------------------------
jal <- aelectoral2::Electoral$new(eleccion = "pm_21", entidad = "jal", llaves = "municipio_21")
jal_mun <- jal$todas[["pm_21"]] |>
  distinct(municipio_21, nombre_municipio)

##Censo_JAL
censo_jal <- read_csv(path) |>
  janitor::clean_names() |>
  filter(entidad == 14, !is.na(longitud)) |>
  mutate(nom_mun = stringi::stri_trans_general(toupper(nom_mun), id = 'latin-ascii'),
         nom_mun = if_else(nom_mun == "SAN MARTIN DE BOLANOS", "SAN MARTIN DE BOLAÑOS", nom_mun),
         nom_mun = if_else(nom_mun == "TLAJOMULCO DE ZUNIGA", "TLAJOMULCO DE ZUÑIGA", nom_mun),
         nom_mun = if_else(nom_mun == "BOLANOS", "BOLAÑOS", nom_mun),
         nom_mun = if_else(nom_mun == "CANADAS DE OBREGON", "CAÑADAS DE OBREGON", nom_mun))

# Revisar que los nombres coincidan_JAL ---------------------------------------
v3 <- unique(censo_jal$nom_mun)
v4 <- jal_mun$nombre_municipio

setdiff(v3, v4)
setdiff(v4, v3)
# Sobreescribir_jal -----------------------------------------------------------

censo_jal <- censo_jal |>
  left_join(jal_mun, join_by(nom_mun == nombre_municipio)) |>
  mutate(municipio_21 = gsub("14_", "", municipio_21)) |>
  distinct(entidad, mun, municipio_21)

# cdmx --------------------------------------------------------------------
cdmx <- aelectoral2::Electoral$new(eleccion = "pm_21", entidad = "cdmx", llaves = "municipio_21")
cdmx_mun <- cdmx$bd |>
  distinct(municipio_21, nombre_municipio_21)

#Censo CDMX
censo_cdmx <- read_csv(path) |>
  janitor::clean_names() |>
  filter(entidad == "09", !is.na(longitud)) %>%
  mutate(nom_mun = stringi::stri_trans_general(toupper(nom_mun), id = 'latin-ascii'))

# Revisar que los nombres coincidan_CDMX ---------------------------------------

v5 <- unique(censo_cdmx$nom_mun)
v6 <- cdmx_mun$nombre_municipio_21

setdiff(v5, v6)
setdiff(v6, v5)

# Sobreescribir_cdmx -----------------------------------------------------------
censo_cdmx <- censo_cdmx |>
  left_join(cdmx_mun, join_by(nom_mun == nombre_municipio_21)) |>
  mutate(municipio_21 = gsub("09_", "", municipio_21)) |>
  distinct(entidad, mun, municipio_21)

# Morelos -----------------------------------------------------------------
mor <- aelectoral2::Electoral$new(eleccion = "pm_21", entidad = "mor", llaves = "municipio_21")
mor_mun <- mor$todas[["pm_21"]] |>
  distinct(municipio_21, municipio)

##Censo_MOR
censo_mor <- read_csv(path) |>
  janitor::clean_names() |>
  filter(entidad =="17", !is.na(longitud)) |>
  mutate(nom_mun = stringi::stri_trans_general(toupper(nom_mun), id = 'latin-ascii'),
         nom_mun = if_else(nom_mun == "JONACATEPEC DE LEANDRO VALLE", "JONACATEPEC", nom_mun))

# Revisar que los nombres coincidan_CDMX ---------------------------------------

v7 <- unique(censo_mor$nom_mun)
v8 <- mor_mun$municipio

setdiff(v7, v8)
setdiff(v8, v7)

# Sobreescribir_mor -----------------------------------------------------------
censo_mor <- censo_mor |>
  left_join(mor_mun, join_by(nom_mun == municipio)) |>
  mutate(municipio_21 = gsub("17_", "", municipio_21)) |>
  distinct(entidad, mun, municipio_21)

relacion_ine_inegi <- bind_rows(censo_cdmx, censo_jal, censo_mex, censo_mor) |>
  mutate(municipio_21 = paste(entidad, municipio_21, sep = "_")) |>
  rename(municipio_22 = municipio_21)

# Modificar Censo Puebla --------------------------------------------------

pue <- aelectoral2::Electoral$new(eleccion = "pm_21", entidad = "pue", llaves = "municipio_21")
pue_mun <- pue$todas[["pm_21"]] |>
  distinct(municipio_21, nombre_municipio_21)


censo_pue <- read_csv(path) |>
  janitor::clean_names() |>
  filter(entidad =="21", !is.na(longitud)) |>
  mutate(nom_mun = stringi::stri_trans_general(toupper(nom_mun), id = 'latin-ascii')
  )

# Revisar que los nombres coincidan_CDMX ---------------------------------------

v7 <- unique(censo_pue$nom_mun)
v8 <- pue_mun$nombre_municipio_21

setdiff(v7, v8)
setdiff(v8, v7)

# Sobreescribir_mor -----------------------------------------------------------
censo_pue <- censo_pue |>
  mutate(nom_mun = case_when(nom_mun == "CANADA MORELOS" ~ "CAÑADA MORELOS",
                             nom_mun == "SAN ANTONIO CANADA" ~ "SAN ANTONIO CAÑADA",
                             T ~ nom_mun)) |>
  left_join(pue_mun, join_by(nom_mun == nombre_municipio_21)) |>
  mutate(municipio_22 = paste(entidad, gsub("21_", "", municipio_21), sep = "_")) |>
  distinct(entidad, mun, municipio_22)

relacion_ine_inegi <- bind_rows(relacion_ine_inegi, censo_pue)

# Modificar Censo Tabasco --------------------------------------------------

tab <- aelectoral2::Electoral$new(eleccion = "pm_21", entidad = "tab", llaves = "municipio_21")
tab_mun <- tab$todas[["pm_21"]] |>
  distinct(municipio_21, nombre_municipio_21)

censo_tab <- read_csv(path) |>
  janitor::clean_names() |>
  filter(entidad == "27", !is.na(longitud)) |>
  mutate(nom_mun = stringi::stri_trans_general(toupper(nom_mun), id = 'latin-ascii')
  )

# Revisar que los nombres coincidan_CDMX ---------------------------------------

v7 <- unique(censo_tab$nom_mun)
v8 <- tab_mun$nombre_municipio_21

setdiff(v7, v8)
setdiff(v8, v7)

# Sobreescribir_mor -----------------------------------------------------------
censo_tab <- censo_tab |>
  left_join(tab_mun, join_by(nom_mun == nombre_municipio_21)) |>
  mutate(municipio_22 = paste(entidad, gsub("27_", "", municipio_21), sep = "_")) |>
  distinct(entidad, mun, municipio_22)

relacion_ine_inegi <- bind_rows(relacion_ine_inegi, censo_tab)

# Censo Yucatán -----------------------------------------------------------
bd <- aelectoral2::Electoral$new(eleccion = "pm_21", entidad = "yuc", llaves = "municipio_21")
bd_mun <- bd$todas[["pm_21"]] |>
  distinct(municipio_21, nombre_municipio_21)

censo <- read_csv(path) |>
  janitor::clean_names() |>
  filter(entidad == "31", !is.na(longitud)) |>
  mutate(nom_mun = stringi::stri_trans_general(toupper(nom_mun), id = 'latin-ascii')
  )

# Revisar que los nombres coincidan_CDMX ---------------------------------------

v7 <- unique(censo$nom_mun)
v8 <- bd_mun$nombre_municipio_21

setdiff(v7, v8)
setdiff(v8, v7)

# Sobreescribir_mor -----------------------------------------------------------
censo <- censo |>
  left_join(bd_mun, join_by(nom_mun == nombre_municipio_21)) |>
  mutate(municipio_22 = paste(entidad, gsub("31_", "", municipio_21), sep = "_")) |>
  distinct(entidad, mun, municipio_22)

relacion_ine_inegi <- bind_rows(relacion_ine_inegi, censo)


usethis::use_data(relacion_ine_inegi, overwrite = T)
