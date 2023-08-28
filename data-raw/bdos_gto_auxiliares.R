## code to prepare `bdos_gto_auxiliares` dataset goes here

# Lista nominal 18 --------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Lista nominal/2018/guanajuato_18_nominal.xlsx"

ln_18 <- readxl::read_excel(path, skip = 6) |>
  janitor::clean_names()













# Formato alianzas --------------------------------------------------------
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/alianzas/locales/"

## GB 18
gb_18_guanajuato <- ln_18 |>
  transmute(eleccion = "gb_18",
            estado = "11",
            nombre_estado = toupper(entidad),
            coaliciones = T,
            candidatura_comun = F
  ) |>
  distinct() |>
  na.omit()

write_excel_csv(gb_18_guanajuato, file = paste0(path, "gb_18_guanajuato.csv"))

## DL 18

dl_18_guanajuato <- ln_18 |>
  arrange(distritacion_local_2016) |>
  transmute(eleccion = "dl_18",
            estado = "11",
            nombre_estado = toupper(entidad),
            distritol_18 = sprintf("%02d", distritacion_local_2016),
            coaliciones = T,
            candidatura_comun = T
  ) |>
  distinct() |>
  na.omit()

write_excel_csv(dl_18_guanajuato, file = paste0(path, "dl_18_guanajuato.csv"))

## DF 18
df_18_guanajuato <- ln_18 |>
  arrange(distrito_federal) |>
  transmute(eleccion = "df_18",
            estado = "11",
            nombre_estado = toupper(entidad),
            distritof_18 = sprintf("%02d", distrito_federal),
            coaliciones = T,
            candidatura_comun = F
  ) |>
  distinct() |>
  na.omit()

write_excel_csv(df_18_guanajuato, file = paste0(path, "df_18_guanajuato.csv"))

## PM 18

pm_18_guanajuato <- ln_18 |>
  arrange(municipio) |>
  transmute(eleccion = "pm_18",
            estado = "11",
            nombre_estado = toupper(entidad),
            municipio_18 = sprintf("%02d", municipio),
            coaliciones = T,
            candidatura_comun = F
  ) |>
  distinct() |>
  na.omit()

write_excel_csv(pm_18_guanajuato, file = paste0(path, "pm_18_guanajuato.csv"))

## pm_21

path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Municipio/guanajuato_normal_casilla.csv"
pm_21_guanajuato <- read_csv(path, skip = 6) |>
  arrange(ID_MUNICIPIO) |>
  transmute(eleccion = "pm_21",
            estado = "11",
            nombre_estado = ESTADO,
            municipio_21 = sprintf("%02d", ID_MUNICIPIO),
            coaliciones = T,
            candidatura_comun = T
  ) |>
  distinct() |>
  na.omit()

path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/alianzas/locales/"
write_excel_csv(pm_21_guanajuato, file = paste0(path, "pm_21_guanajuato.csv"))

## df_21
path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Local/2021/Distrito local/guanajuato_normal_casilla.csv"
dl_21_guanajuato <- read_csv(path, skip = 6) |>
  janitor::clean_names() |>
  transmute(eleccion = "dl_21",
            estado = "11",
            nombre_estado = "GUANAJUATO",
            distritol_21 = sprintf("%02d",
                                   as.numeric(as.roman(
                                     gsub("DISTRITO ", "", distrito_local)))
            ),
            coaliciones = T,
            candidatura_comun = F
  ) |>
  distinct() |>
  na.omit()

path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/alianzas/locales/"
write_excel_csv(dl_21_guanajuato, file = paste0(path, "dl_21_guanajuato.csv"))

## DF 21

path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/Resultados definitivos/Federal/2021/Diputado_normal_casilla.csv"
df_21_guanajuato <- read_delim(path, "|",skip = 6) |>
  janitor::clean_names() |>
  filter(id_estado == 11) |>
  transmute(eleccion = "df_21",
            estado = "11",
            nombre_estado = "GUANAJUATO",
            distritol_21 = sprintf("%02d", id_distrito),
            coaliciones = T,
            candidatura_comun = F
  ) |>
  distinct() |>
  na.omit()

path <- "~/Google Drive/Unidades compartidas/2_Recursos/Externas/Limpieza/alianzas/locales/"
write_excel_csv(df_21_guanajuato, file = paste0(path, "df_21_guanajuato.csv"))
