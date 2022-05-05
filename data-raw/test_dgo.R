library(purrr)
library(tidyr)

bd <- Electoral$new("df_21", entidad = "dgo",
                    llaves = c("seccion", "distritof", "distritol", "municipio"),
                    extranjero = T, especial = "repartir")

c("df_18", "pr_18","gb_16", "pm_16", "pm_19") %>% walk(~{
  print(.x)
  bd$agregar_bd(.x, "dgo")
  })

bd$partido(nivel = "distritof_21", eleccion = "df_21")
bd$partido(nivel = "distritof_18", eleccion = "df_18")
bd$partido(nivel = "distritof_18", eleccion = "pr_18")

bd$partido(nivel = "estado", eleccion = "gb_16")
bd$partido(nivel = "nombre_municipio_19", eleccion = "pm_19")
bd$partido(nivel = "nombre_municipio_16", eleccion = "pm_16")


# Test --------------------------------------------------------------
test <- function(bd, eleccion){
  total <- bd %>% summarise(across(ends_with(eleccion),~sum(.x,na.rm = T))) %>%
    pivot_longer(everything())

  return(list(bd, total))
}

bd$bd_partido %>% map2(names(bd$bd_partido), ~{
  test(.x, .y)
})

test
