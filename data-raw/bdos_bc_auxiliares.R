## code to prepare `bdos_bc_auxiliares` dataset goes here
# Alianza PM 24 -----------------------------------------------------------

path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/PREP/Locales"
files <- list.files(path, recursive = T, pattern = ".csv",full.names = T)
files_cand <- subset(files, grepl("AYUN_CAND|AYUN_Cand", files))

## Se debe de seguir la siguiente estructura: eleccion, estado, nombre_estado, municipio, coaliciones y un booleano de si es Candidatura Común

dicc <- aelectoral2::diccionario |>
  mutate(id_estado = sprintf("%02s", id_estado)) |>
  select(-abreviatura)

pm_24 <- readr::read_csv(files_cand[[1]]) |>
  janitor::clean_names() |>
  rename_with(~gsub("_local", "", .x), contains("_local")) |>
  select(-contains("suplente")) |>
  rename_with(~gsub("_propietaria", "", .x), contains("_propietaria")) |>
  rename_with(~gsub("id_", "", .x), contains("id")) |>
  arrange(as.numeric(municipio)) |>
  mutate(candidatura = if_else(candidatura == "SIN REGISTRO", NA, candidatura)) |>
  na.omit() |>
  filter(!grepl("IND", partido_ci)) |>
  filter(n() > 1, .by = candidatura) |>
  filter(nchar(partido_ci) == max(nchar(partido_ci)), .by = candidatura) |>
  transmute(eleccion = "pm_24",
            estado = sprintf("%02s", entidad),
            municipio = sprintf("%03s", municipio),
            coalicion = tolower(gsub("C_", "", partido_ci))) |>
  distinct(municipio, .keep_all = T)

readr::write_rds(pm_24, "inst/alianzas/bc/pm_24.rda")
