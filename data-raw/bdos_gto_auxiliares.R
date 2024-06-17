## code to prepare `bdos_gto_auxiliares` dataset goes here

# Lista nominal 18 --------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Lista nominal/2018/guanajuato_18_nominal.xlsx"

ln_18 <- readxl::read_excel(path, skip = 6) |>
  janitor::clean_names()

# Formato alianzas --------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/alianzas/Sucias/guana_gob_18.csv"

gb_18 <- read_csv(path, skip = 8) |>
  janitor::clean_names() |>
  filter(grepl("_", alianza)) |>
  transmute(eleccion = "gb_18",
            estado = "11",
            nombre_estado = "GUANAJUATO",
            coaliciones = tolower(alianza),
            candidatura_comun = F
  ) |>
  distinct() |>
  na.omit()

write_rds(gb_18, file = "inst/alianzas/gto/gb_18.rda")
## DL 18
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/alianzas/Sucias/guana_mun_dl_18.csv"
read_csv(path, skip = 9) |>
  janitor::clean_names() |>
  filter(!is.na(distrito)) |>
  count(partido_politico) #No hay coaliciones


## PM 18
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/alianzas/Sucias/guana_pm_18.csv"

pm_18 <- read_csv(path) |>
  janitor::clean_names() |>
  mutate(candidatura_propietaria = iconv(candidatura_propietaria, from = "ISO-8859-1", to = "UTF-8"),
         partido_ci = case_when(partido_ci == "NA_Gto" ~ "panal",
                                grepl("CAND_IND", partido_ci) ~ gsub("CAND_IND_", "ind", partido_ci),
                                T ~ partido_ci)) |>
  filter(grepl("_", partido_ci)) |>
  transmute(eleccion = "pm_18",
            estado = id_estado,
            nombre_estado = "GUANAJUATO",
            municipio_18 = sprintf("%03d", id_municipio_local),
            coaliciones = partido_ci,
            candidatura_comun = F)

write_rds(pm_18, file = "inst/alianzas/gto/pm_18.rda")

## pm_21
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/alianzas/Sucias/guana_mun_21.csv"
pm_21 <- read_csv(path, skip = 6) |>
  janitor::clean_names() |>
  filter(!is.na(pri_prd)) |>
  distinct(id_municipio, .keep_all = T) |>
  transmute(eleccion = "pm_21",
            estado = id_estado,
            nombre_estado = "GUANAJUATO",
            municipio_18 = sprintf("%03d", id_municipio),
            coaliciones = "pri_prd",
            candidatura_comun = F)

write_rds(pm_21, file = "inst/alianzas/gto/pm_21.rda")


## dl_21
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/alianzas/Sucias/guana_dl_21.csv"
dl_21 <- read_csv(path) |>
  janitor::clean_names() |>
  filter(!is.na(pri_prd)) |>
  distinct(id_distrito_local, .keep_all = T) |>
  transmute(eleccion = "dl_21",
            estado = "11",
            nombre_estado = "GUANAJUATO",
            distritol_21 = sprintf("%02d", id_distrito_local),
            coaliciones = "pri_prd",
            candidatura_comun = F
  )

write_rds(dl_21, "inst/alianzas/gto/dl_21.rda")

# pm24 --------------------------------------------------------------------
entidad <- "gto"

dicc <- aelectoral2::diccionario |>
  mutate(id_estado = sprintf("%02s", id_estado)) |>
  select(-abreviatura, nombre_estado = estado)

pm_24 <- readr::read_csv(files_cand[[11]]) |>
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
  reframe(partido_ci = paste(partido_ci, collapse = "_"), .by = c(entidad, municipio, candidatura)) |>
  filter(nchar(partido_ci) == max(nchar(partido_ci)), .by = candidatura) |>
  transmute(eleccion = "pm_24",
            estado = sprintf("%02s", entidad),
            municipio = sprintf("%03s", municipio),
            coalicion = tolower(gsub("CC_|COA_|C_", "", partido_ci)),
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
