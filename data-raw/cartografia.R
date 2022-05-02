library(dplyr)
library(purrr)
wd <- "/Users/emiliomorones/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/INE/SHP/2021"


# Distrito Federal --------------------------------------------------------

wd %>% list.files(full.names = T) %>% map(~{
  list.files(.x,pattern = "DISTRITO_FEDERAL", full.names = T)
}) %>% flatten() %>% do.call(c,.) %>% map(~{

  aux <- substr(.x,112,113)
  terminacion <- substr(.x, nchar(.x)-2, nchar(.x))

    file.copy(.x,
            glue::glue("~/Documents/Git/aelectoral2/inst/shp/df_21/{aux}.{terminacion}")
  )

})


# Distrito Local ----------------------------------------------------------

wd %>% list.files(full.names = T) %>% map(~{
  list.files(.x,pattern = "DISTRITO_LOCAL", full.names = T)
}) %>% flatten() %>% do.call(c,.) %>% map(~{

  aux <- substr(.x,112,113)
  terminacion <- substr(.x, nchar(.x)-2, nchar(.x))

  file.copy(.x,
            glue::glue("~/Documents/Git/aelectoral2/inst/shp/dl_21/{aux}.{terminacion}")
  )

})


