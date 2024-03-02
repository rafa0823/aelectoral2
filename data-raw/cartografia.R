library(dplyr)
library(purrr)
library(sf)
wd <- "/Users/emiliomorones/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/SHP/2022"


# Distrito Federal --------------------------------------------------------

wd %>% list.files(full.names = T) %>% map(~{
  list.files(.x,pattern = "DISTRITO_FEDERAL.shp", full.names = T)
})%>% do.call(c,.) %>% map(~{

  aux <- substr(.x,112,113)
  terminacion <- substr(.x, nchar(.x)-2, nchar(.x))
  sf <- st_read(.x)
  if("DISTRITO" %in% names(sf)) sf <- sf |> rename(DISTRITO_F = DISTRITO)
  temp <- sf %>% transmute(distritof_22 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                formatC(DISTRITO_F, width = 2, flag = 0), sep = "_"))
  readr::write_rds(temp,glue::glue("inst/shp/df_22/{aux}.rda"))
})


# Distrito Local ----------------------------------------------------------

wd %>% list.files(full.names = T) %>% map(~{
  list.files(.x,pattern = "DISTRITO_LOCAL.shp", full.names = T)
}) %>% flatten() %>% do.call(c,.) %>% map(~{

  aux <- substr(.x,112,113)
  terminacion <- substr(.x, nchar(.x)-2, nchar(.x))

  temp <- st_read(.x) %>% transmute(distritol_22 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                         formatC(DISTRITO_L, width = 2, flag = 0), sep = "_"))
  readr::write_rds(temp,glue::glue("inst/shp/dl_22/{aux}.rda"))

})

# Municipio ---------------------------------------------------------------

wd %>% list.files(full.names = T) %>% map(~{
  list.files(.x,pattern = "MUNICIPIO.shp", full.names = T)
}) %>% flatten() %>% do.call(c,.) %>% map(~{

  aux <- substr(.x,112,113)
  terminacion <- substr(.x, nchar(.x)-2, nchar(.x))

  temp <- st_read(.x) %>% transmute(municipio_22 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                         formatC(MUNICIPIO, width = 3, flag = 0), sep = "_"),
                                    nombre_municipio_22 = NOMBRE)
  readr::write_rds(temp,glue::glue("inst/shp/mun_22/{aux}.rda"))

})


# Estados -----------------------------------------------------------------

wd %>% list.files(full.names = T) %>% map(~{
  list.files(.x,pattern = "ENTIDAD.shp", full.names = T)
}) %>% flatten() %>% do.call(c,.) %>% map(~{

  aux <- substr(.x,112,113)
  terminacion <- substr(.x, nchar(.x)-2, nchar(.x))

  temp <- st_read(.x) %>% transmute(entidad = formatC(ENTIDAD, width = 2, flag = 0),
                                    nombre_entidad = NOMBRE,
                                    circunscripcion = CIRCUNSCRI)

  readr::write_rds(temp,glue::glue("inst/shp/ent_22/{aux}.rda"))

})


# Sección -----------------------------------------------------------------


wd %>% list.files(full.names = T) %>% map(~{
  list.files(.x,pattern = "SECCION.shp", full.names = T)
}) %>% flatten() %>% do.call(c,.) %>% map(~{

  aux <- substr(.x,112,113)
  terminacion <- substr(.x, nchar(.x)-2, nchar(.x))

  temp <- st_read(.x) %>% transmute(entidad = formatC(ENTIDAD, width = 2, flag = 0),
                                    distritof_22 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                         formatC(DISTRITO, width = 2, flag = 0), sep = "_"),
                                    distritol_22 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                         formatC(DISTRITO_L, width = 2, flag = 0), sep = "_"),
                                    municipio_22 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                         formatC(MUNICIPIO, width = 3, flag = 0), sep = "_"),
                                    seccion = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                    formatC(SECCION, width = 4, flag = 0), sep = "_")


  )
  readr::write_rds(temp,glue::glue("inst/shp/secc_22/{aux}.rda"))
})

# SHPS 2023 ---------------------------------------------------------------

wd <- "~/Google Drive/Unidades compartidas/Morant Consultores/Insumos/INE/SHP/2023"

# Distrito Federal --------------------------------------------------------

wd %>%
  list.files(full.names = T) %>%
  map(~{
    list.files(.x,pattern = "DISTRITO_FEDERAL.shp", full.names = T)
  })%>%
  do.call(c,.) %>%
  map(~{
    aux <-  stringr::str_extract(.x, "(?<=2023/)\\d+(?= )")
    terminacion <- substr(.x, nchar(.x)-2, nchar(.x))
    sf <- sf::st_read(.x)
    if("DISTRITO" %in% names(sf)) sf <- sf |> rename(DISTRITO_F = DISTRITO)
    temp <- sf %>%
      transmute(distritof_23 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                     formatC(DISTRITO_F, width = 2, flag = 0), sep = "_"))
    readr::write_rds(temp,glue::glue("inst/shp/df_23/{aux}.rda"))
  })


# Distrito Local ----------------------------------------------------------

wd %>%
  list.files(full.names = T) %>%
  map(~{
    list.files(.x,pattern = "DISTRITO_LOCAL.shp", full.names = T)
  }) %>%
  flatten() %>%
  do.call(c,.) %>%
  map(~{
    aux <-  stringr::str_extract(.x, "(?<=2023/)\\d+(?= )")
    terminacion <- substr(.x, nchar(.x)-2, nchar(.x))

    temp <- st_read(.x) %>%
      transmute(distritol_23 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                     formatC(DISTRITO_L, width = 2, flag = 0), sep = "_"))
    readr::write_rds(temp,glue::glue("inst/shp/dl_23/{aux}.rda"))
  })

# Municipio ---------------------------------------------------------------

wd %>%
  list.files(full.names = T) %>%
  map(~{
    list.files(.x,pattern = "MUNICIPIO.shp", full.names = T)
  }) %>%
  flatten() %>%
  do.call(c,.) %>%
  map(~{
    aux <-  stringr::str_extract(.x, "(?<=2023/)\\d+(?= )")
    terminacion <- substr(.x, nchar(.x)-2, nchar(.x))

    temp <- st_read(.x) %>%
      transmute(municipio_23 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                     formatC(MUNICIPIO, width = 3, flag = 0), sep = "_"),
                nombre_municipio_23 = NOMBRE)
    readr::write_rds(temp,glue::glue("inst/shp/mun_23/{aux}.rda"))
  })


# Estados -----------------------------------------------------------------

wd %>%
  list.files(full.names = T) %>%
  map(~{
    list.files(.x,pattern = "ENTIDAD.shp", full.names = T)
  }) %>%
  flatten() %>%
  do.call(c,.) %>%
  map(~{
    aux <-  stringr::str_extract(.x, "(?<=2023/)\\d+(?= )")
    terminacion <- substr(.x, nchar(.x)-2, nchar(.x))

    temp <- st_read(.x) %>%
      transmute(entidad = formatC(ENTIDAD, width = 2, flag = 0),
                nombre_entidad = NOMBRE,
                circunscripcion = CIRCUNSCRI)

    readr::write_rds(temp,glue::glue("inst/shp/ent_23/{aux}.rda"))
  })


# Sección -----------------------------------------------------------------


wd %>%
  list.files(full.names = T) %>%
  map(~{
    list.files(.x,pattern = "SECCION.shp", full.names = T)
  }) %>%
  flatten() %>%
  do.call(c,.) %>%
  map(~{
    aux <-  stringr::str_extract(.x, "(?<=2023/)\\d+(?= )")
    terminacion <- substr(.x, nchar(.x)-2, nchar(.x))
    temp <- st_read(.x) %>%
      transmute(entidad = formatC(ENTIDAD, width = 2, flag = 0),
                distritof_23 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                     formatC(DISTRITO, width = 2, flag = 0), sep = "_"),
                distritol_23 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                     formatC(DISTRITO_L, width = 2, flag = 0), sep = "_"),
                municipio_23 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                     formatC(MUNICIPIO, width = 3, flag = 0), sep = "_"),
                seccion = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                formatC(SECCION, width = 4, flag = 0), sep = "_")
      )
    readr::write_rds(temp,glue::glue("inst/shp/secc_23/{aux}.rda"))
  })
