## code to prepare `paleta` dataset goes here
partidos <- c("morena", "pri", "pan", "prd", "mc", "panal", "pt", "total")
colores <- c("#A6032F", "#038C33", "#0339A6", "#F2B705","#f27405","#03A6A6", "#D91136", "#FF8FAB")
alcance <- rep("nacional", times = length(colores))

paleta <- tibble(partidos, colores, alcance)
usethis::use_data(paleta, overwrite = TRUE)
