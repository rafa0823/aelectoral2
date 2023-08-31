## code to prepare `catalogo` dataset goes here

catalogo <- readr::read_csv("inst/Documentacion/catalogo2.csv")

usethis::use_data(catalogo, overwrite = TRUE)
