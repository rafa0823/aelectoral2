# Federales ---------------------------------------------------------------

library(readr)
library(purrr)
library(tibble)
wd <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal"
partidos <- c("pan", "pri", "prd", "pvem", "pt", "mc", "panal", "morena", "es")
ja <- list.files(wd, full.names = T) %>% map(~{
  list.files(.x, full.names = T) %>% map(~{
    aux <- read_csv(.x)

    rep <- aux %>% revisar_nombres()

    part_falt <- NULL
    for(i in seq_along(partidos)){
      no_esta <- sum(grepl(partidos[i], names(aux))) == 0
      if(no_esta) part_falt <- part_falt %>% append(partidos[i])
    }

    list(vars = names(aux),
         repetidas = paste(rep, collapse = ", "),
         tipo_casilla = "TIPO_CASILLA" %in% names(aux),
         archivo = .x,
         partidos_faltantes = paste(part_falt, collapse = ", ")
    )
  })
})
ja2 <- ja %>% flatten

#columnas repetidas
col_rep <- ja2 %>% discard(~.x$repetidas == "") %>% map(~{
  tibble(columnas = .x$repetidas, archivo = .x$archivo)
}) %>% bind_rows()
col_rep %>% write_excel_csv("data-raw/col_rep_federal.csv")
#todas las federales están por casilla
ja2 %>% discard(~.x$tipo_casilla)

#consistencia de variables

aux <- ja2 %>% map(~{
  tibble(variables = .x %>% pluck("vars"),
         archivo = .x %>% pluck("archivo")
         )
}) %>% bind_rows()

no_todo <- aux %>% count(variables,sort = T) %>% mutate(n_base = length(ja2)) %>% filter(n != n_base)
aux %>% semi_join(no_todo) %>% write_excel_csv("data-raw/columnas_inconsistentes_federal.csv")
aux %>% anti_join(no_todo) %>% write_excel_csv("data-raw/columnas_consistentes_federal.csv")
# Locales -----------------------------------------------------------------
wd2 <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local"
local <- list.files(wd2, full.names = T) %>% map(~{

  list.files(.x, full.names = T) %>% map(~{
    list.files(.x, full.names = T) %>% map(~{
      aux <- read_csv(.x) %>% janitor::clean_names()

      rep <- aux %>% revisar_nombres()

      part_falt <- NULL
      for(i in seq_along(partidos)){
        no_esta <- sum(grepl(partidos[i], names(aux))) == 0
        if(no_esta) part_falt <- part_falt %>% append(partidos[i])
      }

      list(vars = names(aux),
           repetidas = paste(rep, collapse = ", "),
           tipo_casilla = "TIPO_CASILLA" %in% names(aux),
           archivo = .x,
           partidos_faltantes = paste(part_falt, collapse = ", ")
      )
    })

  })
})

local2 <- local %>% flatten() %>% flatten()

#columnas numéricas
col_rep_local <- local2 %>% discard(~.x$repetidas == "") %>% map(~{
  tibble(columnas = .x$repetidas, archivo = .x$archivo)
}) %>% bind_rows()

col_rep_local %>% write_excel_csv("data-raw/col_numerica_local.csv")
# sin variable tipo_casilla
s_tipo_casilla <- local2 %>% discard(~.x$tipo_casilla) %>% map(~{
  tibble(casilla = grepl("casilla",.x$archivo),
         archivo = .x$archivo
  )
}) %>% bind_rows()

s_tipo_casilla %>% select(archivo) %>% write_excel_csv("data-raw/tipo_casilla_local.csv")

#consistencia de variables

aux_loc <- local2 %>% map(~{
  tibble(variables = .x %>% pluck("vars"),
         archivo = .x %>% pluck("archivo")
  )
}) %>% bind_rows()

no_todo_local <- aux_loc %>% count(variables,sort = T) %>% mutate(n_base = length(local2)) %>% filter(n != n_base)
aux_loc %>% semi_join(no_todo_local) %>% write_excel_csv("data-raw/columnas_inconsistentes_local.csv")
