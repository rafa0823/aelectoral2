library(aelectoral2)
library(dplyr)
library(purrr)
mex <- Electoral$new("df_21", "mex",extranjero = T,especiales = T)

mex$agregar_bd(eleccion = "pm_21", entidad = "mex", llaves = c( "seccion", "distritol","distritof", "municipio"), extraordinaria = c(eleccion = "pmext_21", entidad = "mex"))

c("df_18", "df_15", "pr_18", "dl_21", "pm_18", "gb_17", "dl_18", "cp_22") %>%
  walk(~{
    mex$agregar_bd(eleccion = .x, entidad = "mex")
    print(.x)
  })

mex$agregar_manual(bd = regiones %>% select(1:2), by = c("nombre_municipio_pm_21" = "municipio"))
mex$agregar_manual(bd = presidentes_mpos_mex %>% select(1:3), by = c("nombre_municipio_pm_21" = "nombre_municipio"))
mex$agregar_manual(bd = cat_utm_22 %>% filter(estado == "15") %>% distinct(seccion, unidad_territorial, sede, distritof_22, nombre_distritof_22), by = "seccion")

mex$bd
resultados_final <- mex$bd %>%
  mutate(id_utm = paste(15,distritof_22.y,unidad_territorial, sep = "-")) %>%
  group_by(id_distrito_federal = distritof_22.y, distrito_federal = nombre_distritof_22.y, id_utm) %>%
  summarise(across(.cols = c(starts_with("ele_"), starts_with("cp_")),.fns = ~sum(.x,na.rm=T)),
            seccion=sum(as.numeric(seccion)*(sede=="SI"),na.rm=T),
            info_municipal = list(tibble(id_municipio = municipio_pm_21,
                                         nombre_municipio = nombre_municipio_pm_21,
                                         id_distrito_local = distritol_21.y,
                                         distrito_local = nombre_distritol_21.y,
                                         region,
                                         # alianza,
                                         presidente_municipal,
                                         partido) %>%
                                    distinct())) %>%
  ungroup()



#agregar cobertura
ln <- readxl::read_excel("data_raw/DatosAbiertos-derfe-pdln_edms_re_sexo_20220218.xlsx") %>%
  rename_with(~str_replace(string = .x, pattern = "\r\n", replacement = "_")) %>%
  janitor::clean_names()
ln_mexico <- ln %>% filter(clave_distrito!="0")

definitiva <- cat_utm_22 %>% mutate(seccion = as.numeric(seccion),
                                    distritof_22 = as.numeric(distritof_22),
                                    estado = as.numeric(estado)) %>%
  filter(estado == "15") %>%
  full_join(ln_mexico %>%
              select(seccion,
                     clave_entidad,
                     clave_distrito,
                     lista_nominal),
            by = c("seccion",
                   "estado" = "clave_entidad",
                   "distritof_22" = "clave_distrito"))

definitiva <- definitiva %>%
  mutate(id_utm=paste(estado,
                      formatC(distritof_22, width = 2, flag = 0),
                      unidad_territorial, sep="-"))

cobertura_definitiva <- definitiva %>% group_by(id_utm,sede) %>%
  summarise(lista_nominal=sum(lista_nominal),
            seccion=sum(seccion*(sede=="SI"))) %>%
  mutate(cobertura=lista_nominal/sum(lista_nominal)) %>%
  filter(sede=="SI") %>% arrange(cobertura) %>%
  select(id_utm,
         seccion,
         cobertura)

# Pegar cobertura

resultados_final <- resultados_final %>% full_join(cobertura_definitiva)

resultados_final %>% write_rds("markdown/resultados_final.rda")
