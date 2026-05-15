# Bases -------------------------------------------------------------------
estado <- "mor"
entidad <- "17"
partidos <- c("morena", "pan", "pri")

cp_22  <- Electoral$new("cp_22", estado, partidos = partidos, llaves = "municipio")
shp <- ElectoralSHP$new(unidad = "secc_22", entidad = estado)
shp$agregar_shp(unidad = "dl_22", entidad = estado)
shp$agregar_shp(unidad = "df_22", entidad = estado)
shp$agregar_shp(unidad = "mun_22", entidad = estado)

# Funciones ---------------------------------------------------------------
calcular_cp_nivel <- function(grupo, entidad){
  shp$shp[[1]] |>
    as_tibble() |>
    left_join(cp_22$bd, join_by(seccion)) |>
    group_by(.data[[grupo]], .data[[glue::glue("nombre_{grupo}")]]) |>
    summarise(continua = sum(cp_continua, na.rm = T),
              total = sum(cp_total, na.rm = T),
              nominal = sum(cp_nominal, na.rm = T)) |>
    mutate(participacion = total/nominal,
           pct_continua = continua/nominal) |>
    arrange(desc(participacion)) |>
    filter(!is.na(pct_continua)) |>
    ungroup()
}


df <- calcular_cp_nivel("distritof_22", entidad = entidad)
dl <- calcular_cp_nivel("distritol_22", entidad = entidad)
mun <- calcular_cp_nivel("municipio_22", entidad = entidad)

# Resultados de revocación - TABLA------------------------------------------------
readr::write_excel_csv(df, glue::glue("~/Desktop/bon_ces/df_{estado}.csv"))

# Mapas - RESULTADOS REVOCACION -------------------------------------------------------------------
## Distrito Federal - labels
shp$shp[[3]] |>
  left_join(df, join_by(distritof_22)) |>
  ggplot() +
  geom_sf(aes(fill = pct_continua), color = "gray66", linewidth = 0.6) +
  scale_fill_gradient2(low = "#335c67" , high = "#9e2a2b", midpoint = mean(df$pct_continua), name = "Continúa",
                       labels = scales::percent) +
  ggsflabel::geom_sf_label_repel(aes(label = gsub(paste0(entidad, "_"), "", distritof_22))) +
  theme_void()

##Distrito local - labels

shp$shp[[2]] |>
  left_join(dl) |>
  ggplot() +
  geom_sf() +
  geom_sf(aes(fill = pct_continua), color = "gray66", linewidth = 0.6) +
  scale_fill_gradient2(low = "#335c67" , high = "#9e2a2b", midpoint = mean(dl$pct_continua), name = "Continúa",
                       labels = scales::percent) +
  ggsflabel::geom_sf_label_repel(aes(label = gsub(paste0(entidad, "_"), "", distritol_22)), check_overlap = T, max.overlaps = 50) +
  theme_void()

## Municipios

shp$shp[[4]] |>
  left_join(mun, join_by("municipio_22")) |>
  ggplot() +
  geom_sf(aes(fill = pct_continua), color = "gray66", linewidth = 0.6) +
  scale_fill_gradient2(low = "#335c67" , high = "#9e2a2b", midpoint = median(mun$pct_continua), name = "Continúa",
                       labels = scales::percent) +
  # ggsflabel::geom_sf_label_repel(aes(label = gsub(paste0(entidad, "_"), "", distritol_22)), check_overlap = T, max.overlaps = 50) +
  theme_void()

# Morenismo ---------------------------------------------------------------

tab <- readr::read_rds(glue::glue("../tableroGenerico/inst/{estado}.rda"))
## Distrito federal
tab$filtrar(nivel = "distritof_22")
tab$graficas$mapa(seccion = F, fill = "morena", labels = F) +
  ggsflabel::geom_sf_label_repel(aes(label = gsub(paste0(entidad, "_"), "", distritof_22)))

## Distrito local

tab$filtrar(nivel = "distritol_22")
tab$graficas$mapa(seccion = F, fill = "morena", labels = F) +
  ggsflabel::geom_sf_label_repel(aes(label = gsub(paste0(entidad, "_"), "", distritol_22)), check_overlap = T, max.overlaps = 50)


# Participación  ----------------------------------------------------------
tab$filtrar(nivel = "distritof_22")
tab$graficas$mapa(seccion = F, fill = "participacion", labels = F) +
  ggsflabel::geom_sf_label_repel(aes(label = gsub(paste0(entidad, "_"), "", distritof_22)))

## Distrito local

tab$filtrar(nivel = "distritol_22")
tab$graficas$mapa(seccion = F, fill = "participacion", labels = F) +
  ggsflabel::geom_sf_label_repel(aes(label = gsub(paste0(entidad, "_"), "", distritol_22)), check_overlap = T, max.overlaps = 50)

# Extras ------------------------------------------------------------------
zoom <- shp$shp[[2]] |>
  filter(distritol_22 %in% paste("31", c("01", "02", "03", "04", "05", "07", "08", "09", "10"), sep = "_")) |>
  inner_join(dl, join_by(distritol_22))

zoom2 <- tab$info$shp$distritol_22 |>
  filter(distritol_22 %in% paste("31", c("01", "02", "03", "04", "05", "07", "08", "09", "10"), sep = "_"))

gen <- shp$shp[[2]] |>
  left_join(dl, join_by(distritol_22)) |>
  ggplot() +
  geom_sf(aes(fill = pct_continua), color = "gray66", linewidth = 0.6) +
  geom_rect(xmin = -89.8012, xmax = -89.47159, ymin = 20.89488, ymax = 21.18592,
            color = "gray33", fill = "transparent", linewidth = 1.2) +
  scale_fill_gradient2(low = "#335c67" , high = "#9e2a2b", midpoint = mean(dl$pct_continua), name = "Continúa",
                       labels = scales::percent) +
  ggsflabel::geom_sf_label_repel(aes(label = gsub(paste0(entidad, "_"), "", distritol_22))) +
  theme_void()

gen2 <- tab$info$shp$distritol_22 |>
  ggplot() +
  geom_sf(aes(fill = col_participacion), color = "gray66", linewidth = 0.6) +
  geom_rect(xmin = -89.8012, xmax = -89.47159, ymin = 20.89488, ymax = 21.18592,
            color = "gray33", fill = "transparent", linewidth = 1.2) +
  scale_fill_identity() +
  ggsflabel::geom_sf_label_repel(aes(label = gsub(paste0(entidad, "_"), "", distritol_22))) +
  theme_void()


z <- zoom |>
  ggplot() +
  geom_sf(aes(fill = pct_continua), color = "gray66", linewidth = 0.6) +
  scale_fill_gradient2(low = "#335c67" , high = "#9e2a2b", midpoint = mean(dl$pct_continua), name = "Continúa",
                       labels = scales::percent, guide = 'none') +
  ggsflabel::geom_sf_label_repel(aes(label = gsub(paste0(entidad, "_"), "", distritol_22))) +
  theme_void()

z2 <- zoom2 |>
  ggplot() +
  geom_sf(aes(fill = col_participacion), color = "gray66", linewidth = 0.6) +
  geom_rect(xmin = -89.8012, xmax = -89.47159, ymin = 20.89488, ymax = 21.18592,
            color = "gray33", fill = "transparent", linewidth = 1.2) +
  scale_fill_identity() +
  ggsflabel::geom_sf_label_repel(aes(label = gsub(paste0(entidad, "_"), "", distritol_22))) +
  theme_void()

gen + z

gen2 + z2

# Sin zoom
shp$shp[[2]] |>
  ggplot() +
  geom_sf() +
  ggsflabel::geom_sf_label(aes(label = gsub(paste0(entidad, "_"), "", distritol_22)), check_overlap = T, max.overlaps = 50) +
  theme_void()

# Con zoom
gen3 <- shp$shp[[2]] |>
  ggplot() +
  geom_sf() +
  geom_rect(xmin = -89.8012, xmax = -89.47159, ymin = 20.89488, ymax = 21.18592,
            color = "gray33", fill = "transparent", linewidth = 1.2) +
  ggsflabel::geom_sf_label_repel(aes(label = gsub(paste0(entidad, "_"), "", distritol_22))) +
  theme_void()

z3 <- zoom |>
  ggplot() +
  geom_sf() +
  ggsflabel::geom_sf_label_repel(aes(label = gsub(paste0(entidad, "_"), "", distritol_22))) +
  theme_void()

gen3 + z3

# Mapa morenismo GDL ------------------------------------------------------

jal <- readr::read_rds("../tableroGenerico/inst/jal.rda")

# GDL
jal$filtrar(unidad = "14_041")
# Morenismo
jal$graficas$mapa(seccion = T, fill = "morena", linewidth = 0.2)
# Participación
jal$graficas$mapa(seccion = T, fill = "participacion", linewidth = 0.2)
# Presidencia municipal pm-21
jal$graficas$mapa(seccion = T, fill = "pm_21", linewidth = 0.2)
# Datos pm_21
jal$aux$shp_secc |>
  as_tibble() |>
  select(entidad:nombre_municipio_22, contains("_pm_21")) |>
  readr::write_excel_csv("~/Desktop/datos_gdl/gdl_pm_21.csv")

#ZAPOPAN
# GDL
jal$filtrar(unidad = "14_120")
# Morenismo
jal$graficas$mapa(seccion = T, fill = "morena", linewidth = 0.2)
# Participación
jal$graficas$mapa(seccion = T, fill = "participacion", linewidth = 0.2)
# Presidencia municipal pm-21
jal$graficas$mapa(seccion = T, fill = "pm_21", linewidth = 0.2)
# Datos pm_21
jal$aux$shp_secc |>
  as_tibble() |>
  select(entidad:nombre_municipio_22, contains("_pm_21")) |>
  readr::write_excel_csv("~/Desktop/datos_gdl/zap_pm_21.csv")

# Revocación de mandato
estado <- "jal"
entidad <- "14"
partidos <- c("morena", "pan", "pri")

cp_22  <- Electoral$new("cp_22", estado, partidos = partidos, llaves = "municipio")
shp <- ElectoralSHP$new(unidad = "secc_22", entidad = estado)
shp <- ElectoralSHP$new(unidad = "mun_22", entidad = estado)

cp <- shp$shp[[1]] |>
  as_tibble() |>
  group_by(municipio_22, nombre_municipio_22, seccion) |>
  left_join(cp_22$bd, join_by(seccion)) |>
  summarise(continua = sum(cp_continua, na.rm = T),
            total = sum(cp_total, na.rm = T),
            nominal = sum(cp_nominal, na.rm = T)) |>
  mutate(participacion = total/nominal,
         pct_continua = continua/nominal) |>
  arrange(desc(participacion)) |>
  filter(!is.na(pct_continua), nominal != 0) |>
  ungroup()

cp_mun <- calcular_cp_nivel("municipio_22", entidad = entidad)

shp_mun <- shp$shp$mun_22_jal

ja <- shp_mun |>
  filter(municipio_22 %in% c("14_095", "14_005", "14_009", "14_084", "14_002", "14_119",
                             "14_052", "14_031", "14_046", "14_068", "14_065", "14_049",
                             "14_106", "14_094", "14_112", "14_117", "14_062", "14_118",
                             "14_030", "14_047", "14_073", "14_120", "14_098", "14_053",
                             "14_123", "14_124", "14_001", "14_102", "14_041", "14_099",
                             "14_072", "14_094")) |>
  left_join(cp_mun)

ja |>
  leaflet::leaflet() |>
  leaflet::addPolygons(label = ~glue::glue("{municipio_22} {nombre_municipio_22}")) |>
  leaflet::addProviderTiles(provider = "CartoDB.Positron")

ja |>
  ggplot() +
  geom_sf(aes(fill = pct_continua)) +
  geom_sf_label(aes(label = as.numeric(gsub("14_", "",  municipio_22)))) +
  scale_fill_gradient2(low = "#335c67" , high = "#9e2a2b", midpoint = mean(ja$pct_continua), name = "Continúa",
                       labels = scales::percent) +
  theme_void()

# Top 15, bottom 15
partidos <- c("delfina", "adm", "total")

bd <- Electoral$new("gb_23", entidad = "mex", partidos = partidos)
bd$partido("gb_23")
bd$voto_relativo("bd_partido", "gb_23")

shp <- ElectoralSHP$new(unidad = "secc_22", entidad = "mex")
shp$agregar_shp(unidad = "mun_22", entidad = "mex")

bd$colapsar_base("bd_partido", filtro = shp$shp$secc_22_mex |>
                   select(seccion))

bd$fusionar_shp(shp = shp$shp$secc_22_mex, base = "bd_partido")

# Creación de clase tablero -----------------------------------------------
tablero <- Tablero$new(info_seccion = bd)

tablero$agregar_eleccion(elecciones = "gb_23",
                         nivel = "municipio_22",
                         bd_relacion = shp$shp$secc_22_mex |>
                           as_tibble() |>
                           select(seccion, municipio_22),
                         shp = shp$shp$mun_22_mex)

tablero$info$shp$municipio_22 |>
  as_tibble() |>
  arrange(desc(ele_delfina_gb_23)) |>
  head(15) |>
  transmute(ranking = dplyr::row_number(),
            municipio_22, nombre_municipio_22, Delfina = ele_delfina_gb_23) |>
  readr::write_excel_csv("~/Desktop/bon_ces/vot_tot_delf.csv")

tablero$info$shp$municipio_22 |>
  as_tibble() |>
  arrange(desc(pct_delfina_gb_23)) |>
  head(15) |>
  transmute(ranking = dplyr::row_number(),
            municipio_22, nombre_municipio_22, Delfina = scales::percent(pct_delfina_gb_23, 0.1)) |>
  readr::write_excel_csv("~/Desktop/bon_ces/vot_pct_delf.csv")

# Álvaro Obregón ----------------------------------------------------------

## code to prepare `cdmx_dl18` dataset goes here

elecciones <- c("pm_18", "dl_18", "df_18", "pm_21", "dl_21", "df_21")
partidos <- c("morena", "pan", "pri", "mc", "prd", "total")

bd <- Electoral$new("gb_18", entidad = "cdmx", partidos = partidos)
bd$partido("gb_18")
bd$voto_relativo("bd_partido", "gb_18")
bd$calcular_ganador("bd_partido", "gb_18")
bd$obtener_degradado_ganador(base = "bd_partido", eleccion = "gb_18")

walk(elecciones, ~{
  bd$agregar_bd(.x)
  bd$partido(.x)
  bd$voto_relativo("bd_partido", .x)
  bd$calcular_ganador("bd_partido", .x)
  bd$obtener_degradado_ganador(base = "bd_partido", eleccion = .x)
})

shp <- ElectoralSHP$new(unidad = "secc_22", entidad = "cdmx")

bd$colapsar_base("bd_partido", filtro = shp$shp$secc_22_cdmx |>
                   as_tibble() |>
                   filter(nombre_municipio_22 == "ALVARO OBREGON") |>
                   select(seccion))

bd$obtener_indice_completo("bd_partido")

shp <- shp$shp[[1]] |>
  inner_join(bd$bd_partido, join_by(seccion))

#Mapa morenismo

shp |>
  ggplot() +
  geom_sf(aes(fill = col_morena), color = "gray66", linewidth = 0) +
  scale_fill_identity() +
  theme_void()

shp |>
  ggplot() +
  geom_sf(aes(fill = col_morena), color = "gray66", linewidth = 0) +
  scale_fill_identity() +
  theme_void()

shp |>
  ggplot() +
  geom_sf(aes(fill = col_total), color = "gray66", linewidth = 0) +
  scale_fill_identity() +
  theme_void()

shp |>
  ggplot() +
  geom_sf(aes(fill = col_pm_21), color = "gray66", linewidth = 0) +
  scale_fill_identity() +
  theme_void()

cp <- Electoral$new("cp_22", "cdmx", partidos = "morena")
cp$bd

shp <- ElectoralSHP$new(unidad = "secc_22", entidad = "cdmx")
shp$agregar_shp("mun_22", entidad = "cdmx")

bd <- shp$shp[[1]] |>
  as_tibble() |>
  left_join(cp$bd, join_by(seccion)) |>
  group_by(municipio_22, nombre_municipio_22) |>
  summarise(continua = sum(cp_continua, na.rm = T),
            total = sum(cp_total, na.rm = T),
            nominal = sum(cp_nominal, na.rm = T)) |>
  mutate(participacion = total/nominal,
         pct_continua = continua/nominal) |>
  arrange(desc(participacion)) |>
  filter(!is.na(pct_continua), nominal != 0) |>
  ungroup()

shp$shp[[2]] |>
  left_join(bd) |>
  ggplot() +
  geom_sf(aes(fill = pct_continua)) +
  geom_sf_label(aes(label = as.numeric(gsub("09_", "",  municipio_22)))) +
  scale_fill_gradient2(low = "#335c67" , high = "#9e2a2b", midpoint = mean(bd$pct_continua), name = "Continúa",
                       labels = scales::percent) +
  theme_void()

shp$shp[[2]] |>
  as_tibble() |>
  select(contains("mun")) |>
  readr::write_excel_csv("~/Desktop/bon_ces/mun_cdmx.csv")

bd |>
  readr::write_excel_csv("~/Desktop/bon_ces/revocacion_cdmx.csv")

# Listado Diálogo Social --------------------------------------------------

pm_21 <- Electoral$new(eleccion = "pm_21", entidad = "mor")
ja <- pm_21$bd |>
  select(seccion, ele_nominal_pm_21)

bd <- readxl::read_excel("~/Downloads/DB DT versión 13012024.xlsx") |>
  janitor::clean_names() |>
  mutate(seccion = sprintf("17_%04s", seccion))

censo <- readr::read_rds("inst/censo/seccion_2020.rda") |>
  filter(entidad == "17")

bd |>
  glimpse()

censo |>
  glimpse()

bd <- bd |>
  left_join(select(censo, seccion, vivpar_hab, vivtot), join_by(seccion)) |>
  left_join(ja, join_by(seccion)) |>
  mutate(coinciden = if_else(listado_nominal_2021 == ele_nominal_pm_21, T, F)) |>
  relocate(ele_nominal_pm_21,.after = listado_nominal_2021)

#Hay 54 secciones que no tienen datos de vivienda

bd |>
  filter(coinciden == F)

bd |>
  readr::write_excel_csv("~/Desktop/bon_ces/morelos_vivienda.csv")

# Subsecretarías ----------------------------------------------------------
library(ggsankey)
bd1 <- readxl::read_excel("~/Downloads/SUBSECRETARÍAS VS COORDINACIONES (1).xlsx", sheet = 2) |>
  janitor::clean_names()
bd2 <- readxl::read_excel("~/Downloads/SUBSECRETARÍAS VS COORDINACIONES (1).xlsx", sheet = 1) |>
  janitor::clean_names()

bd <- bd1 |>
  left_join(bd2, join_by(municipios))


# bd <- readxl::read_excel("~/Downloads/SUBSECRETARÍAS VS COORDINACIONES.xlsx", sheet = 1) |>
#   janitor::clean_names()

bd <- bd |>
  make_long(subsecretaria, coordinacion)

conteo <- bd |>
  group_by(node) |>
  tally()

bd2 <- bd |>
  merge(conteo, by.x = 'node', by.y = 'node', all.x = T)

bd2 |>
  ggplot(aes(x = x
             , next_x = next_x
             , node = node
             , next_node = next_node
             , fill = factor(node)
             , label = paste0(node, " n = ", n))
  ) +
  geom_sankey(flow.alpha = 0.5
              , node.color = "black"
              ,show.legend = FALSE) +
  geom_sankey_label(size = 2, color = "black", fill= "white", hjust = 0.5) +
  theme_bw() +
  theme(axis.title = element_blank()
        , axis.text.y = element_blank()
        , axis.ticks = element_blank()
        , panel.grid = element_blank(),
        panel.border = element_blank()) +
  scale_fill_viridis_d(option = "H")

ggsave("~/Desktop/sankey.png", device = "png", scale = 2, dpi = "retina", width = 900, height = 450, units = "px")

# Barras por coordinación -------------------------------------------------

bd1 <- readxl::read_excel("~/Downloads/SUBSECRETARÍAS VS COORDINACIONES (1).xlsx", sheet = 2) |>
  janitor::clean_names()
bd2 <- readxl::read_excel("~/Downloads/SUBSECRETARÍAS VS COORDINACIONES (1).xlsx", sheet = 1) |>
  janitor::clean_names()

bd <- bd1 |>
  left_join(bd2, join_by(municipios)) |>
  mutate(municipios = case_when(municipios == "VALLE DE CHALCO" ~ "VALLE DE CHALCO SOLIDARIDAD",
                                municipios == "CUAUTITLÁN MÉXICO" ~ "CUAUTITLAN",
                                municipios == "ACAMBAY" ~ "ACAMBAY DE RUIZ CASTANEDA",
                                T ~ municipios))

censo <- readr::read_rds("inst/censo/municipio_22_2020.rda") |>
  filter(entidad == "15") |>
  mutate(nom_mun = toupper(stringi::stri_trans_general(nom_mun, id = "latin-ascii"))) |>
  select(nom_mun, pobtot)

bd |>
  mutate(municipios = stringi::stri_trans_general(municipios, id = "latin-ascii")) |>
  left_join(censo, join_by(municipios == nom_mun)) |>
  count(coordinacion, wt = pobtot) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = reorder(coordinacion, pct), y = pct, label = glue::glue("{scales::percent(pct)} ({scales::comma(n)})"))) +
  geom_col(fill = "#81b29a", width = 0.6) +
  coord_flip() +
  theme_bw(base_size = 12, base_family = "Poppins") +
  ggfittext::geom_bar_text() +
  labs(x = "Coordinaciones", y = "Población",
       title = "Distribución de población por coordinación",
       caption = "Fuente: Censo Población y Vivienda INEGI 2020") +
  scale_y_continuous(labels = scales::percent) +
  theme(plot.title.position = "plot", panel.border = element_blank())

ggsave("~/Desktop/barras_pob.png", device = "png", scale = 2, dpi = "retina", width = 900, height = 450, units = "px")


# Análisis secciones AO ---------------------------------------------------

ao <- readr::read_rds("../tableroGenerico/inst/ao.rda")

censo_ao <- readr::read_rds("inst/censo/seccion_2020.rda") |>
  select(seccion, vivpar_hab)

base <- ao$bd_partido |>
  transmute(seccion, quant_morena, quant_total, quant_rezago,
            clasif = if_else(quant_morena %in% c("Mucho", "Algo") &
                              quant_total %in% c("Poco", "Nada") &
                               quant_rezago %in% c("Alto", "Muy alto")
                             , "1", "0"
                             )
            ) |>
  left_join(censo_ao) |>
  arrange(desc(as.numeric(clasif)), desc(vivpar_hab)) |>
  readr::write_excel_csv("~/Desktop/bon_ces/ao_secciones.csv")

base2 <- ao$bd_partido |>
  transmute(seccion, quant_morena, quant_total, quant_rezago,
            clasif = if_else(quant_morena %in% c("Poco", "Nada") &
                               quant_total %in% c("Mucho", "Algo") &
                               quant_rezago %in% c("Bajo", "Muy bajo")
                             , "1", "0"
            )
  ) |>
  left_join(censo_ao) |>
  arrange(desc(as.numeric(clasif)), desc(vivpar_hab))

titulo <- "**Análisis profundo:** 80 secciones <span style = 'color:#ae2012'>prioritarias</span>"

ao$shp$seccion |>
  left_join(base) |>
  ggplot() +
  geom_sf(aes(fill = clasif)) +
  scale_fill_manual(values = c("1" = "#ae2012", "0" = "gray77"), guide = "none") +
  theme_void() +
  theme(plot.title = ggtext::element_markdown(size = 20, margin = margin(b = 5, unit = 'mm')),
        plot.title.position = "plot") +
  labs(title = titulo)

# Revisión datos Morelos --------------------------------------------------
## Librerías
library(readxl)
library(dplyr)
library(sf)
## Insumos
path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Clientes/MargaritaGonzalez_Morelos/Diálogo Social/Bases secciones, metas/Base final DT_seccion.xlsx"
bd <- read_excel(path) |>
  janitor::clean_names() |>
  filter(!is.na(dt)) #|>
  mutate(seccion_origen = if_else(is.na(seccion_origen), secc, seccion_origen))

path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/SHP/2023/17 MORELOS/SECCION.shp"

shp_23 <- read_sf(path, as_tibble = T) |>
  as_tibble() |>
  janitor::clean_names()

bd |>
  anti_join(shp_23, join_by(secc == seccion)) |>
  filter(!is.na(seccion_origen), is.na())


censo <- readRDS("inst/censo/seccion_2020.rda") |>
  select(seccion,  vivpar_hab)

path <-  "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/Lista Nominal/DatosAbiertos-derfe-pdln_edms_re_20230921.xlsx"
ln <- read_excel(path) |>
  janitor::clean_names() |>
  filter(clave_entidad == 17) |>
  select(nombre_entidad, nombre_municipio, seccion, lista_nominal)


shp <- ElectoralSHP$new(unidad = "secc_22", entidad = "mor")
mor_22 <- shp$shp[[1]] |>
  as_tibble() |>
  select(seccion, contains("municipio"))

mor <- readr::read_rds("../tableroGenerico/inst/mor.rda")
mor <- mor$info$shp$seccion |>
  as_tibble() |>
  mutate(vot_morena = ele_morena_pm_21/ele_participacion_pm_21) |>
  select(seccion, quant_morena, quant_participacion, pct_morena_pm_21, ganador_pm_21, vot_morena, quant_rezago)
## Análisis

### Hay 30 secciones que no existen en nuestro shp, todas salvo 1 tienen una sección origen
### Nuestro shp coincide con las secciones origen, no tenemos cómo corroborar sus secciones nuevas con los insumos que tenemos.
no_shp <- bd |>
  select(secc, municipio) |>
  mutate(secc = sprintf("17_%04s", secc)) |>
  left_join(mor_22, join_by(secc == seccion)) |>
  mutate(coincide = if_else(municipio == nombre_municipio_22, T, F)) |>
  filter(is.na(coincide)) |>
  pull(secc)

bd |>
  filter(secc %in% as.numeric(gsub("17_", "", no_shp))) |>
  select(seccion_origen, municipio) |>
  mutate(secc = sprintf("17_%04s", seccion_origen)) |>
  left_join(mor_22, join_by(secc == seccion)) |>
  mutate(coincide = if_else(municipio == nombre_municipio_22, T, F)) |>
  filter(coincide == F)

## Comparación lista nominal
# Las diferencias entre las listas nominales son chicas con excepción de la sección 943 donde hay un claro error

dif_nom <- bd |>
  select(contains("secc"), municipio, lista_nominal) |>
  left_join(ln, join_by(seccion_origen == seccion)) |>
  mutate(dif = lista_nominal.x - lista_nominal.y)

dif_nom |>
  filter(abs(dif) > 50)

### Diferencia promedio de 6.5 personas
dif_nom |>
  filter(secc != "943") |>
  summarise(mean(dif, na.rm = T))

## Intervalos

secciones_interes <- bd |>
  filter(seccion_origen != "943") |>
  select(seccion_origen, municipio, dt, promovidos_final, lista_nominal, cots_final) |>
  mutate(razon = promovidos_final/lista_nominal,
         estimacion_alta = if_else(razon >= .8,  T, F),
         estimacion_baja = if_else(razon <= .2,  T, F),
         seccion = sprintf("17_%04s", seccion_origen)) |>
  left_join(mor, join_by(seccion)) |>
  left_join(censo)

secciones_interes |>
  select(seccion_origen, municipio, razon, contains("quant"), pct_morena_pm_21, ganador_pm_21, everything()) |>
  filter(razon > 1) |>
  arrange(desc(razon))

secciones_interes |>
  filter(promovidos_final == 0) |>
  count(quant_participacion)

secciones_interes |>
  filter(promovidos_final == 0) |>
  janitor::tabyl(quant_morena, quant_participacion)


# Análisis morenismo vs rezago --------------------------------------------

path <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/SHP/2023/14 JALISCO/COLONIA.shp"

colonias <- sf::read_sf(path) |>
  sf::st_transform(crs = st_crs(4326))

shp_col <- colonias |>
  janitor::clean_names() |>
  filter(municipio == 120)

bd <- readr::read_rds("../tableroGenerico/inst/jal.rda")
shp_zap <- bd$info$shp$seccion |>
  filter(nombre_municipio_22 == "ZAPOPAN")

bd2 <- shp_zap |>
  sf::st_join(shp_col) |>
  as_tibble()

bd <- bd$info$shp$seccion |>
  as_tibble() |>
  filter(nombre_municipio_22 == "ZAPOPAN")

tile <- bd |>
  filter(!is.na(quant_rezago), !is.na(quant_morena)) |>
  count(quant_rezago, quant_morena)

tile |>
  ggplot(aes(x = quant_rezago, y = quant_morena, fill = n)) +
  geom_tile() +
  scale_fill_gradient2(low = "#5b8e7d", mid = "white", high = "#bc4b51",
                       midpoint = mean(tile$n), name = "Coincidencias") +
  theme_minimal(base_size = 12, base_family = "Poppins") +
  labs(x = "Indice de rezago", y = "Indice de Morenismo",
       title = titulo) +
  theme(plot.title = ggtext::element_markdown(size = 16, margin = margin(b = 5, unit = 'mm')),
        plot.title.position = "plot",
        plot.background = element_rect(color = "white"))



ggsave("~/Desktop/rezago_tile.png", device = "png", scale = 2, dpi = "retina", width = 900, height = 800, units = "px")

bd |>
  ggplot(aes(x = quant_rezago, y = rezago)) +
  geom_point() +
  geom_smooth()

titulo <- "Comparativa <span style = 'color:#bc4b51'>alto morenismo</span> y <span style = 'color:#5b8e7d'>bajo morenismo</span>"

bd |>
  filter(!is.na(quant_rezago), !is.na(quant_morena)) |>
  ggplot(aes(x = rezago, y = morena)) +
  geom_point(aes(color = if_else(morena > 0, "#bc4b51", "#5b8e7d")), alpha = 0.8) +
  geom_smooth(color = "gray33") +
  theme_minimal(base_size = 12, base_family = "Poppins") +
  scale_color_identity() +
  labs(x = "Índice de rezago", y = "Índice de morenismo", title = titulo) +
  annotate("text", x = 61, y = -4, label = "Más alto") +
  annotate("text", x = 72, y = -4, label = "Más Bajo") +
  theme(plot.title = ggtext::element_markdown(size = 16, margin = margin(b = 5, unit = 'mm')),
        plot.title.position = "plot",
        plot.background = element_rect(color = "white"))

ggsave("~/Desktop/rezago.png", device = "png", scale = 2, dpi = "retina", width = 900, height = 450, units = "px")

bd2 |>
  arrange(rezago) |>
  filter(quant_rezago %in% c("Muy alto", "Alto", "Medio")) |>
  select(seccion, quant_rezago, rezago, nombre, cp) |>
  filter(is.na(cp)) |>
  readr::write_excel_csv("~/Desktop/colonias_rezago.csv")
