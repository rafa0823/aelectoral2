## code to prepare `bdos_chih_auxiliares` dataset goes here
# Alianza PM 24 -----------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/PREP/Locales"
files <- list.files(path, recursive = T, pattern = ".csv",full.names = T)
files_cand <- subset(files, grepl("AYUN_CAND|AYUN_Cand", files))

## Se debe de seguir la siguiente estructura: eleccion, estado, nombre_estado, municipio, coaliciones y un booleano de si es Candidatura Común
entidad <- "chih"

dicc <- aelectoral2::diccionario |>
  mutate(id_estado = sprintf("%02s", id_estado)) |>
  select(-abreviatura, nombre_estado = estado)

pm_24 <- readr::read_csv(files_cand[[4]]) |>
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
  transmute(eleccion = "pm_24",
            estado = sprintf("%02s", entidad),
            municipio = sprintf("%03s", municipio),
            coalicion = tolower(gsub("CC_|COA_", "", partido_ci)),
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



