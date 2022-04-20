# Federales ---------------------------------------------------------------

library(readr)
library(purrr)
library(tibble)
wd <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal"
ja <- list.files(wd, full.names = T) %>% map(~{
  list.files(.x, full.names = T) %>% map(~{
    aux <- read_csv(.x)

    rep <- aux %>% revisar_nombres()
    list(vars = names(aux),
         repetidas = paste(rep, collapse = ", "),
         tipo_casilla = "TIPO_CASILLA" %in% names(aux),
         archivo = .x
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

# Locales -----------------------------------------------------------------
wd2 <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Local"
local <- list.files(wd2, full.names = T) %>% map(~{
  list.files(.x, full.names = T) %>% map(~{
    list.files(.x, full.names = T) %>% map(~{
      aux <- read_csv(.x)

      rep <- aux %>% revisar_nombres()
      list(vars = names(aux),
           repetidas = paste(rep, collapse = ", "),
           tipo_casilla = "TIPO_CASILLA" %in% names(aux),
           archivo = .x
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

