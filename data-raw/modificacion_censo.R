## code to prepare `modificacion_censo` dataset goes here
library(readr)


# Cargar bases ------------------------------------------------------------
path <- "../telectoral.edomex/data-raw/mex_cen_2020.csv"
## Censo
censo_mex <- read_csv(path) |>
  janitor::clean_names() |>
  filter(entidad == 15, !is.na(longitud)) |>
  mutate(nom_mun = stringi::stri_trans_general(toupper(nom_mun), id = 'latin-ascii'),
         nom_mun = if_else(nom_mun == "ACAMBAY DE RUIZ CASTANEDA", "ACAMBAY DE RUIZ CASTAÑEDA", nom_mun))
## INE
mex <- aelectoral2::Electoral$new(eleccion = "pm_21", entidad = "mex", llaves = "municipio_21")
mex_mun <- mex$bd |>
  distinct(municipio_21, nombre_municipio_21)

# Revisar que los nombres coincidan ---------------------------------------

v1 <- unique(censo_mex$nom_mun)
v2 <- mex_mun$nombre_municipio_21

setdiff(v1, v2)
setdiff(v2, v1)

# Sobreescribir -----------------------------------------------------------

censo_mex <- censo_mex |>
  left_join(mex_mun, join_by(nom_mun == nombre_municipio_21)) |>
  mutate(mun = gsub("15_", "", municipio_21))

