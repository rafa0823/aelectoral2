############################################### AUXILIARES HIDALGO ------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# ALIANZAS ---------------------------

#PM 21

alianzas_hgo_pm_20 <- readxl::read_excel("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_20_hidalgo.xlsx") %>%
  janitor::clean_names() %>%
  rename(municipio = municipio_20) %>%
  as_tibble() %>%
  mutate(estado = "13",
         nombre_estado = "HIDALGO",
         municipio_16 = formatC(municipio, width = 2, flag = "0"),
         eleccion = "pm_20")

alianzas_hgo_pm_16 %>% count(coaliciones)

alianzas_hgo_pm_16 %>% write_rds("inst/alianzas/hgo/hgo_pm_20.rda")


# GB 16

alianzas_hgo_gb_16 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_16_hidalgo.csv") %>%
  janitor::clean_names() %>%
  rename(municipio = municipio_16) %>%
  as_tibble() %>%
  mutate(estado = "13",
         nombre_estado = "HIDALGO",
         municipio_16 = formatC(municipio, width = 2, flag = "0"),
         eleccion = "gb_16")

alianzas_hgo_gb_16 %>% count(coaliciones)

alianzas_hgo_gb_16 %>% write_rds("inst/alianzas/hgo/hgo_gb_16.rda")

#PM 16

alianzas_hgo_pm_16 <- read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_16_hidalgo.csv") %>%
  janitor::clean_names() %>%
  rename(municipio = municipio_16) %>%
  as_tibble() %>%
  mutate(estado = "13",
         nombre_estado = "HIDALGO",
         municipio_16 = formatC(municipio, width = 2, flag = "0"),
         eleccion = "pm_16")

alianzas_hgo_pm_16 %>% count(coaliciones)

alianzas_hgo_pm_16 %>% write_rds("inst/alianzas/hgo/hgo_pm_16.rda")

# pm24 --------------------------------------------------------------------
entidad <- "hgo"

dicc <- aelectoral2::diccionario |>
  mutate(id_estado = sprintf("%02s", id_estado)) |>
  select(-abreviatura, nombre_estado = estado)

pm_24 <- readr::read_csv(files_cand[[12]]) |>
  janitor::clean_names() |>
  rename_with(~gsub("_local", "", .x), contains("_local")) |>
  select(-contains("suplente")) |>
  rename_with(~gsub("_propietaria", "", .x), contains("_propietaria")) |>
  rename_with(~gsub("id_", "", .x), contains("id")) |>
  arrange(as.numeric(municipio)) |>
  mutate(candidatura = if_else(candidatura == "SIN REGISTRO", NA, candidatura)) |>
  na.omit() |>
  filter(!grepl("CI", partido_ci)) |>
  filter((n() > 1 | grepl("-|_", partido_ci)), .by = candidatura) |>
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
