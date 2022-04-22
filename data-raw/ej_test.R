library(readr)
library(tidyr)
load("data/nac_pr_18.rda")
pr18 <- nac_pr_18 %>% as_tibble %>% mutate(across(starts_with("ele_"), ~as.numeric(.x)))

t_nac(pr18, "candidato", "morena_|pt_|pes_")

t_nivel(pr18, "candidato", "morena_|pt_|pes_", estado = 1, nivel = "nombre_estado",seleccion = "AGUASCALIENTES")
t_nivel(pr18, "candidato", "morena_|pt_|pes_", estado = 1, nivel = "distritof",seleccion = "1")

t_nac(pr18, "partido", "total_")
t_nac(pr18, "partido", "morena_")

pr18 %>%
  group_by(estado) %>%
  summarise(across(starts_with("ele_"), ~sum(.x,na.rm = T)))

