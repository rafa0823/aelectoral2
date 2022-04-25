renv::deactivate()

library(tabulizer)
library(dplyr)
diccionario <- extract_tables("~/Downloads/diccionario.pdf",) %>% purrr::pluck(1) %>% as_tibble %>%
  purrr::set_names(.[1,]) %>% slice(-1) %>%
  janitor::clean_names() %>% mutate(abreviatura = tolower(abreviatura),
                                    estado = toupper(estado))

usethis::use_data(diccionario, overwrite = T)

renv::activate()
