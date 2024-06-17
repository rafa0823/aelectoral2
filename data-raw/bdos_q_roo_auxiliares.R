############################################### AUXILIARES QUINTANA ROO ------------------------------------------------------------------------

pacman::p_load(tidyverse,janitor, readxl, tidytable, here,edomex)

# ALIANZAS  ---------------------------------------------------------------------------------


# gb 16

q_roo_pm_21 <- read.csv('~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_21_quintanaroo.csv') %>%
  janitor::clean_names() %>%
  rename(municipio = municipio_21) %>%
  mutate(eleccion='pm_21',
         nombre_estado='QUINTANA ROO',
         municipio = formatC(municipio, width = 2, flag='0'))

q_roo_pm_21 %>% count(coaliciones)

q_roo_pm_21%>% write_rds("inst/alianzas/q_roo/q_roo_pm_21.rda")

# pm 18

q_roo_pm_18 <- read.csv('~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_18_quintanaroo.csv') %>%
  janitor::clean_names() %>%
  rename(municipio = municipio_18) %>%
  mutate(eleccion='pm_18',
         nombre_estado='QUINTANA ROO',
         municipio = formatC(municipio, width = 2, flag='0'))

q_roo_pm_18 %>% count(coaliciones)

q_roo_pm_18%>% write_rds("inst/alianzas/q_roo/q_roo_pm_18.rda")

# pm 16

q_roo_pm_16 <- read.csv('~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_16_quintanaroo.csv') %>%
  janitor::clean_names() %>%
  rename(municipio = municipio_16) %>%
  mutate(eleccion='pm_16',
         nombre_estado='QUINTANA ROO',
         municipio = formatC(municipio, width = 2, flag='0'))

q_roo_pm_16 %>% count(coaliciones)

q_roo_pm_16 %>% write_rds("inst/alianzas/q_roo/q_roo_pm_16.rda")



# gb 16

q_roo_gb_16 <- read.csv('~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/gob_16_quintanaroo.csv') %>%
  janitor::clean_names() %>%
  rename(distritol = distrito_16) %>%
  mutate(eleccion='gb_16',
         nombre_estado='QUINTANA ROO',
         distritol = formatC(distritol, width = 2, flag='0'))
q_roo_gb_16 %>% count(coaliciones)

q_roo_gb_16 %>% write_rds("inst/alianzas/q_roo/q_roo_gb_16.rda")

# pm24 --------------------------------------------------------------------
entidad <- "q_roo"

dicc <- aelectoral2::diccionario |>
  mutate(id_estado = sprintf("%02s", id_estado)) |>
  select(-abreviatura, nombre_estado = estado)

pm_24 <- readr::read_csv(files_cand[[19]]) |>
  janitor::clean_names() |>
  rename_with(~gsub("_local", "", .x), contains("_local")) |>
  select(-contains("suplente")) |>
  rename_with(~gsub("_propietaria", "", .x), contains("_propietaria")) |>
  rename_with(~gsub("id_", "", .x), contains("id")) |>
  arrange(as.numeric(municipio)) |>
  mutate(candidatura = if_else(candidatura %in% c("SIN REGISTRO", "Registro cancelado"), NA, candidatura)) |>
  na.omit() |>
  filter(!grepl("CI|IND", partido_ci)) |>
  filter((n() > 1 | grepl("-|_", partido_ci)), .by = candidatura) |>
  filter(nchar(partido_ci) == max(nchar(partido_ci)), .by = candidatura) |>
  transmute(eleccion = "pm_24",
            estado = sprintf("%02s", entidad),
            municipio = sprintf("%03s", municipio),
            coalicion = tolower(gsub("CC_|COA_|C_", "", partido_ci)),
            coalicion = gsub("-", "_", coalicion),
            coalicion = if_else(coalicion == "mprogresa", "mc_progresa", coalicion),
            candidatura_comun = if_else(grepl("CC_", partido_ci), T, NA)
  ) |>
  left_join(dicc, join_by(estado == id_estado))

glimpse(pm_24)

carpetas <- list.files("inst/alianzas/")

if(!entidad %in% carpetas){
  dir.create(glue::glue("inst/alianzas/{entidad}"))
  readr::write_rds(pm_24, glue::glue("inst/alianzas/{entidad}/pm_24.rda"))
} else{
  readr::write_rds(pm_24, glue::glue("inst/alianzas/{entidad}/pm_24.rda"))
}
