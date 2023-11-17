## code to prepare `paleta` dataset goes here
paleta <- readr::read_csv("data-raw/paleta.csv")

usethis::use_data(paleta, overwrite = TRUE)
