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
