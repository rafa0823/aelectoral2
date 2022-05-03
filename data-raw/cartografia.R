library(dplyr)
library(purrr)
wd <- "/Users/emiliomorones/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/SHP/2021"


# Distrito Federal --------------------------------------------------------

wd %>% list.files(full.names = T) %>% map(~{
  list.files(.x,pattern = "DISTRITO_FEDERAL.shp", full.names = T)
})%>% do.call(c,.) %>% map(~{

  aux <- substr(.x,112,113)
  terminacion <- substr(.x, nchar(.x)-2, nchar(.x))

  temp <- st_read(.x) %>% transmute(distritof_21 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                         formatC(DISTRITO_F, width = 2, flag = 0), sep = "_"))
  readr::write_rds(temp,glue::glue("inst/shp/df_21/{aux}.rda"))


})


# Distrito Local ----------------------------------------------------------

wd %>% list.files(full.names = T) %>% map(~{
  list.files(.x,pattern = "DISTRITO_LOCAL.shp", full.names = T)
}) %>% flatten() %>% do.call(c,.) %>% map(~{

  aux <- substr(.x,112,113)
  terminacion <- substr(.x, nchar(.x)-2, nchar(.x))

  temp <- st_read(.x) %>% transmute(distritol_21 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                         formatC(DISTRITO_L, width = 2, flag = 0), sep = "_"))
  readr::write_rds(temp,glue::glue("inst/shp/dl_21/{aux}.rda"))

})

# Municipio ---------------------------------------------------------------

wd %>% list.files(full.names = T) %>% map(~{
  list.files(.x,pattern = "MUNICIPIO.shp", full.names = T)
}) %>% flatten() %>% do.call(c,.) %>% map(~{

  aux <- substr(.x,112,113)
  terminacion <- substr(.x, nchar(.x)-2, nchar(.x))

  temp <- st_read(.x) %>% transmute(municipio_21 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                         formatC(MUNICIPIO, width = 3, flag = 0), sep = "_"),
                                    nombre_municipio_21 = NOMBRE)
  readr::write_rds(temp,glue::glue("inst/shp/mun_21/{aux}.rda"))

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

  readr::write_rds(temp,glue::glue("inst/shp/ent_21/{aux}.rda"))

})


# Sección -----------------------------------------------------------------

# Estados -----------------------------------------------------------------

wd %>% list.files(full.names = T) %>% map(~{
  list.files(.x,pattern = "SECCION.shp", full.names = T)
}) %>% flatten() %>% do.call(c,.) %>% map(~{

  aux <- substr(.x,112,113)
  terminacion <- substr(.x, nchar(.x)-2, nchar(.x))

  temp <- st_read(.x) %>% transmute(entidad = formatC(ENTIDAD, width = 2, flag = 0),
                                    distritof_21 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                         formatC(DISTRITO, width = 2, flag = 0), sep = "_"),
                                    distritol_21 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                         formatC(DISTRITO_L, width = 2, flag = 0), sep = "_"),
                                    municipio_21 = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                         formatC(MUNICIPIO, width = 3, flag = 0), sep = "_"),
                                    seccion = paste(formatC(ENTIDAD, width = 2, flag = 0),
                                                         formatC(SECCION, width = 4, flag = 0), sep = "_")


                                    )

  readr::write_rds(temp,glue::glue("inst/shp/secc_21/{aux}.rda"))

})
