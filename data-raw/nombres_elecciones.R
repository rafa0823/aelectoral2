## code to prepare `nombres` dataset goes here

nombres_elecciones <- readxl::read_excel("data-raw/nombres_elecciones.xlsx")
usethis::use_data(nombres_elecciones, overwrite = TRUE)
