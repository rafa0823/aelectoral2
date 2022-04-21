
# Clave casilla federal ---------------------------------------------------

wd <- "~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/Resultados definitivos/Federal/2012"

list.files(wd, full.names = T) %>% walk(~{
  read_csv(.x) %>%
    mutate(CLAVE_CASILLA =
             glue::glue("{formatC(ESTADO,width = 2, flag = '0')}{formatC(SECCION,width = 4, flag = '0')}{TIPO_CASILLA}{formatC(ID_CASILLA,width = 2, flag = '0')}{formatC(EXT_CONTIGUA,width = 2, flag = '0')}")
    ) %>%
    relocate(CLAVE_CASILLA, .before = 1) %>% write_excel_csv(.x)
})

