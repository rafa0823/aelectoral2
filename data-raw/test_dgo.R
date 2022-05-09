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



# Segundo intento de test -------------------------------------------------

devtools::load_all()

shp <- ElectoralSHP$new("secc_21","dgo")

shp$juntar_bd(nivel = "secc_21_dgo",bd = bd$bd)
shp$shp$secc_21_dgo
# df_21 -------------------------------------------------------------------

bd <- Electoral$new(eleccion = "df_21", entidad = "dgo",llaves = c("distritof", "municipio"))
bd$partido("distritof_21",eleccion = "df_21")
bd$candidato(al_df_21, nivel = "distritof_21",eleccion = "df_21")

bd$bd %>% summarise(sum(ele_total_df_21,na.rm = T))
bd$bd_partido$df_21 %>% summarise(sum(ele_total_df_21,na.rm = T))
bd$todas$df_21 %>% summarise(sum(ele_total_df_21,na.rm = T))

bd$bd_candidato$df_21 %>% summarise(sum(cand_pvem_pt_morena_df_21))
bd$bd_candidato$df_21 %>% summarise(sum(cand_pan_pri_prd_df_21))
bd$bd_candidato$df_21 %>% summarise(sum(cand_mc_df_21))
bd$bd_candidato$df_21 %>% summarise(sum(cand_mc_df_21))
bd$bd_candidato$df_21 %>% summarise(sum(cand_nominal_df_21))

bd$bd_candidato$df_21 %>%
  ganador(nivel = "distritof_21",eleccion =  "df_21") %>% count(ganador_df_21)


# df_18 -------------------------------------------------------------------

bd$agregar_bd("df_18","dgo")
bd$partido("distritof_18",eleccion = "df_18")
al_df_18 <- readr::read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/federales/df_18.csv")
al_df_18 <- al_df_18 %>% transmute(distritof_18 = paste(stringr::str_pad(estado, width = 2, pad = "0"),
                                                        stringr::str_pad(distritof_18, width = 2, pad = "0"),sep = "_"
),
coalicion = tolower(coaliciones),
coalicion = stringr::str_replace(string = coalicion,pattern = "_na", replacement = "_panal"))

bd$candidato(al_df_18, "distritof_18", "df_18")
bd$bd_candidato$df_18 %>% summarise(sum(cand_total_df_18))
bd$bd_candidato$df_18 %>% summarise(sum(cand_pan_prd_mc_df_18))

bd$bd_candidato$df_18 %>% ganador("distritof_18", "df_18") %>% count(ganador_df_18)


# pm_19 -------------------------------------------------------------------
bd$agregar_bd("pm_19","dgo")
bd$partido("municipio_19",eleccion = "pm_19")
al_pm_19 <- readr::read_csv("~/Dropbox (Selva)/Ciencia de datos/Consultoría Estadística/Recursos/Externos/Limpieza/alianzas/locales/pm_19_durango.csv")
al_pm_19 <- al_pm_19 %>% transmute(municipio_19 = paste("10", stringr::str_pad(string = municipio_19, width = 3, pad = "0"), sep = "_"),
                                   coalicion = coaliciones)

bd$candidato(al_pm_19, "municipio_19",eleccion = "pm_19")

bd$bd_partido$pm_19 %>% summarise(sum(ele_pan_pm_19))
# pm_16 -------------------------------------------------------------------
# no tiene prd
# solo tiene un municipio
bd$agregar_bd("pm_16","dgo")

debug(repartir_coalicion)
bd$partido("municipio_16", eleccion = "pm_16")
al_pm_16 <- readr::read_rds("inst/alianzas/alianzas_durango/alianzas_dgo_pm_16.rda") %>%
  transmute(municipio_16 = paste(estado, stringr::str_pad(municipio_16,width = 3,pad = "0"),sep = "_"),
            coalicion = gsub(replacement = "_",pattern = "__",x = coaliciones),
            coalicion = gsub(replacement = "pri_pvem_pd_panal",pattern = "pvem_pri_pd_panal",x = coalicion,fixed = T))

bd$bd_candidato <- bd$bd_candidato[1:3]
bd
bd$candidato(al_pm_16,nivel = "municipio_16","pm_16")
bd$bd_partido$pm_16
bd$todas$pm_16 %>% count(municipio_16)
bd$bd_partido$pm_16 %>% View

debug(repartir_coalicion)
bd$bd %>% filter(municipio_16 == "10_015") %>% repartir_coalicion("municipio_16", eleccion = "pm_16") %>%
  tidyr::pivot_longer(-municipio_16)
# gb_16 -------------------------------------------------------------------
# no esta el prd
# bd$agregar_bd("gb_16","dgo")
# bd$partido("estado",eleccion = "gb_16")
# al_gb_16 <- readr::read_rds("inst/alianzas/alianzas_durango/alianzas_dgo_gb_16.rda") %>%
#   count(estado,coalicion = coaliciones) %>% select(-n)
#
# bd$candidato(al_gb_16,nivel = "estado", eleccion = "gb_16")
#
# bd$bd_partido$gb_16 %>% tidyr::pivot_longer(-estado)
