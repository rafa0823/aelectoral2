library(readr)
library(tidyr)
load("data/nac_pr_18.rda")
pr18 <- nac_pr_18 %>% as_tibble %>% mutate(across(starts_with("ele_"), ~as.numeric(.x)))


# Nacional ----------------------------------------------------------------

#candidato
t_nac(pr18, "candidato", "morena_|pt_|pes_")
#partido
t_nac(pr18, "partido", "total_")
t_nac(pr18, "partido", "morena_")

# Niveles -----------------------------------------------------------------

#estado
#candidato
t_nivel(pr18, "candidato", "morena_|pt_|pes_", estado = 1, nivel = "nombre_estado",seleccion = "AGUASCALIENTES")
t_nivel(pr18, "candidato", "morena_|pt_|pes_", estado = 1, nivel = "distritof",seleccion = "1")
#partido
t_nivel(pr18, "partido", "morena_", estado = 1, nivel = "nombre_estado",seleccion = "AGUASCALIENTES")

#distritof
#partido
t_nivel(pr18, "candidato", "morena_|pt_|pes_", estado = 1, nivel = "distritof", seleccion = "1")

#casilla
cas <- pr18 %>% sample_n(1) %>% pull(clave_casilla)
t_nivel(pr18, "candidato", "ele_morena_pr_18", estado = 15, nivel = "clave_casilla", seleccion = cas)




