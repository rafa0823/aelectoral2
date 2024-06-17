## code to prepare `paleta` dataset goes here
bd_2006 <- tibble(partidos = c("pbt", "apm", "asdc"),
                  colores = c("#fcbf49", "#588157", "#ffb3c6"),
                  alcance = rep("nacional", 3))

jal_2021 <- tibble(partidos = c("futuro", "hagamos", "somos"),
                   colores = c("#390099", "#7209b7", "#fbf8cc"),
                   alcance = rep("jal", 3))


paleta <- readr::read_csv("data-raw/paleta.csv") |>
  bind_rows(bd_2006) |>
  bind_rows(jal_2021)

usethis::use_data(paleta, overwrite = TRUE)
