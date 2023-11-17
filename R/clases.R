
#' Clase R6 que construye un conjunto de bases electorales comparables
#'
#' @description
#' Se inicia con la base de una elección en específico y a ella se le pueden ir agregando elecciones que se juntan por seccion
#'
#' @details
#' Este conjunto de bases electorales pueden ser de distintas elecciones, candidatos, años, tipos de eleccion; se puede decidir la forma en la se reparten los votos en el extranjero y las casillas especiales; así como agregar distintas variables geográficas como el municipio o el estado.


Electoral <- R6::R6Class("Electoral",
                         public = list(bd = NA,
                                       todas = NULL,
                                       censo = NULL,
                                       bd_partido = list(),
                                       bd_candidato = list(),
                                       shp = list(),
                                       eleccion = NA_character_,
                                       nivel = NA_character_,
                                       entidad = NA_character_,
                                       tipo_eleccion = NA_character_,
                                       extranjero = NA,
                                       especiales = NA,
                                       partidos = NA_character_,
                                       colores = NA,
                                       llaves = NULL,
                                       elecciones_agregadas = NULL,
                                       analisis = tibble::tribble(~eleccion, ~nivel, ~analisis, ~parametros),
                                       #' #' Initialize: Obtener base de datos electoral
                                       #' @description
                                       #' Lo que hace es leer una base de datos electoral, darle formato el formato documentado para unirla o compararla con otras elecciones y la resume por sección.
                                       #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                                       #' @param entidad Cuando es nacional es "nac", cuando es local se pone la abreviatura oficial, por ejemplo "chis", "dgo", "mex".
                                       #' @param llaves Son las claves cartográficas de los niveles. Por default la unidad mínima es sección y está acompañada de estado.
                                       #' @param tipo_eleccion Por default es "MR" refiriéndose a mayoría relativa.
                                       #' @param extranjero Se refiere a si se desea incluir los votos en el extrajero, entendidos como sección 0000. El default es TRUE
                                       #' @param especiales Las casillas especiales se pueden "eliminar", "repartir" o dejar como están es el parámetro default.
                                       #' @param partidos Aquellos partidos para los cuales se van a realizar todas las operaciones de aelectoral en las que acote el número de partidos.
                                       #' @return Un data frame con la elección seleccionada
                                       #' @export
                                       #' @examples
                                       #' bd <- Electoral$new("df_21", entidad = "dgo",
                                       #' llaves = c("seccion", "distritof", "distritol", "municipio"),
                                       #' extranjero = T, especial = "repartir")

                                       initialize = function(eleccion,
                                                             entidad,
                                                             nivel = "seccion",
                                                             llaves = "seccion",
                                                             tipo_eleccion = "MR",
                                                             partidos = NULL,
                                                             extranjero = T, especiales = NULL){
                                         self$eleccion <- eleccion
                                         self$elecciones_agregadas <- eleccion
                                         self$entidad <- entidad
                                         self$nivel <- nivel
                                         self$extranjero <- extranjero
                                         self$especiales <- especiales
                                         self$llaves <- c("estado", llaves)
                                         self$tipo_eleccion <- tipo_eleccion
                                         self$partidos <- partidos
                                         self$obtener_bd()
                                         self$todas <- list(self$bd) %>% purrr::set_names(eleccion)
                                         self$colores <- asociar_colores(partidos = self$partidos)

                                         if(!self$extranjero){
                                           self$eliminar_votoExtranjero()
                                         }

                                         self$bd <- self$bd %>% self$accion_especiales(self$especiales)

                                         self$bd <- self$bd %>% reducir(NULL, self$llaves)

                                       },
                                       print = function(){

                                         mensaje <- cat(
                                           glue::glue("Entidad: {self$entidad} \nElecciones agregadas: {paste(self$todas %>% names, collapse = ', ')}

Variables cartográficas agregadas en bd: {paste(self$llaves, collapse = ', ')}

Tipo de elección: {self$tipo_eleccion}
{if(self$extranjero) 'Se mantiene el voto en el extranjero' else 'Se elimina el voto en el extranjero'}
Criterio de casillas especiales: {if(is.null(self$especiales)) 'ninguna acción especial realizada' else self$especiales}

{if(length(self$bd_partido)> 0)  paste('Elecciones repartidas por partido:', paste(names(self$bd_partido), collapse = ', ')) else ''}
{if(length(self$bd_candidato)> 0) paste('Elecciones repartidas por candidato:', paste(names(self$bd_candidato), collapse = ', ')) else ''}
                                                      ")
                                         )

                                         return(mensaje)
                                       },
                                       #' @description
                                       #'Función basada en leer_base()
                                       #' @return tibble de la bd

                                       obtener_bd = function(){
                                         self$bd <- leer_base(eleccion = self$eleccion,
                                                              entidad = self$entidad,
                                                              tipo_eleccion = self$tipo_eleccion)
                                       },

                                       #' @description
                                       #' Esta función te indica cuales fueron las alianzas de la eleccion indicada
                                       #' @param nivel Nivel en el que se determinan las alianzas dependiendo de la unidad en la que se realiza la elección.
                                       #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                                       #'
                                       #' @return La lista de coaliciones que hubieron en la elección señalada.
                                       #' @examples
                                       coalicion = function(eleccion){
                                         if(!eleccion %in% names(self$todas)) stop("Favor de agregar la elección primero con el método agregar_bd")

                                         self$partido(self$nivel[length(self$nivel)], eleccion)

                                         al <- leer_alianza(self$nivel[length(self$nivel)], eleccion, self$entidad, self$bd)

                                         self$candidato(al, self$nivel[length(self$nivel)], eleccion)
                                       },

                                       #' @description
                                       #' Reparte los voto de acuerdo con las coaliciones
                                       #' @param  nivel En el que se determinan las alianzas
                                       #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                                       #'
                                       #' @return Elección con los votos repartidos por alianza
                                       #' @examples
                                       partido = function(eleccion){
                                         aux_c <- self$bd %>% repartir_coalicion(nivel = self$nivel[length(self$nivel)], eleccion = eleccion)

                                         self$bd_partido <- self$bd_partido %>%
                                           append(
                                             list(aux_c) |>
                                               purrr::set_names(eleccion)
                                           )

                                         self$analisis <- self$analisis |>
                                           tibble::add_row(eleccion = eleccion, nivel = self$nivel[length(self$nivel)], analisis = "partido",
                                                           parametros = list(eleccion = eleccion))
                                       },
                                       #' @description
                                       #' Reparte los votos por candidato de la elección, para ello necesita saber qué candidatos fueron en alianza y cual fue.
                                       #' @param alianzas  Proviene de coalicion() en la que se indican las alianzas de la elección
                                       #' @param nivel En el que se determinan las alianzas
                                       #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                                       #'
                                       #' @return Una base datos con los votos repartidos por candidato.
                                       #' @examples
                                       candidato = function(alianzas, eleccion){
                                         aux_c <- repartir_candidato(bd = self$bd_partido[[eleccion]],
                                                                     alianzas, self$nivel[length(self$nivel)], eleccion)

                                         self$bd_candidato <- self$bd_candidato %>%
                                           append(list(aux_c) %>% purrr::set_names(eleccion))

                                       },

                                       #'@description
                                       #'Agrega una base de datos de la elección señalada y se la pega a la elección que se haya leído con obtener_bd.
                                       #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                                       #'
                                       #' @return Tibble de la base de datos con la nueva elección resumidas por sección
                                       #' @examples
                                       #'  bd$agregar_bd(eleccion = "pm_21", extraordinaria = c(eleccion = "pmext_21", entidad = "mex"))

                                       agregar_bd = function(eleccion){

                                         add <- leer_base(eleccion = eleccion,
                                                          entidad = self$entidad, tipo_eleccion = self$tipo_eleccion)

                                         self$todas <- self$todas %>% append(list(add) %>% purrr::set_names(eleccion))

                                         add <- add %>% self$accion_especiales(self$especiales)

                                         if(!self$extranjero){
                                           add <- add %>% eliminar_votoExtranjero()
                                         }

                                         add <- add %>% reducir(self$bd, self$llaves)

                                         self$bd <- self$bd %>% full_join(
                                           add, by = c("estado", "seccion")
                                         )

                                         self$elecciones_agregadas <- self$elecciones_agregadas |>
                                           append(eleccion) |>
                                           unique()

                                       },

                                       #' @description
                                       #'Basada en la función full_join, se juntan bases de datos.
                                       #'
                                       #' @param bd base de datos que que se quiere juntar
                                       #' @param by variable por la que se une las bds
                                       #'
                                       #' @return
                                       #' @examples
                                       #' bd %>%  agregar_manual(df_21, "seccion")
                                       agregar_manual = function(bd, by){
                                         self$bd <- self$bd %>% full_join(
                                           bd, by = by
                                         )
                                       },

                                       #' @description
                                       #' Para dterminar lo que se va a hacer con las casillas especiales
                                       #' @param bd Base de datos electoral
                                       #' @param accion se puede dejar como están, repartir o eliminar
                                       #'
                                       #' @return Tibble de la bd
                                       #' @examples
                                       accion_especiales = function(bd, accion){
                                         if(!is.null(accion)){
                                           if(accion == "eliminar"){
                                             bd <- eliminar_especiales(bd)
                                           }

                                           if(accion == "repartir"){
                                             bd <- repartir_especiales(bd)
                                           }
                                         }
                                         return(bd)
                                       },

                                       #' @description
                                       #' Elimina el voto en el extanjero. Esta función filtra las secciones que son 0000.
                                       #'
                                       #' @return Devuelve el tibble de la elección sin los votos en el extanjero
                                       #' @examples
                                       eliminar_votoExtranjero = function(){
                                         self$bd <- eliminar_votoExtranjero(self$bd)
                                       },
                                       fusionar_shp = function(shp, base){

                                         if("list" %in% class(self[[base]])){
                                           stop("No se ha ejecutado la función self$colapsar_base")
                                         }
                                         self$shp <- self$shp |>
                                           append(
                                             list(shp |>
                                                    inner_join(self[[base]],
                                                               by = self$nivel[length(self$nivel)])) |>
                                               purrr::set_names(self$nivel[length(self$nivel)])
                                           )

                                         self[[base]] = list()

                                       },
                                       #' @description Calcula los votos relativos para los partidos seleccionados
                                       #' @param base Es la base de datos que será modificada
                                       #' @param eleccion Es el tipo de elección y su año separado por "_".
                                       #' @param partidos Es un vector con los partidos que se quieren calcular los votos relativos
                                       #' @return Regresa columnas con el prefijo 'pct' en la misma base entregada
                                       voto_relativo = function(base, eleccion, partidos = self$partidos){
                                         self[[base]][[eleccion]] <-
                                           self[[base]][[eleccion]] |>
                                           left_join(
                                             aelectoral2::calcular_votos_relativos(self[[base]][[eleccion]],
                                                                                   eleccion = eleccion,
                                                                                   grupo = !!rlang::sym(self$nivel[length(self$nivel)]),
                                                                                   partido = partidos),
                                             by = self$nivel[length(self$nivel)]
                                           )

                                         self$analisis <- self$analisis |>
                                           tibble::add_row(eleccion = eleccion,
                                                           nivel = self$nivel[length(self$nivel)],
                                                           analisis = "voto_relativo",
                                                           parametros = list(list(base = base,
                                                                                  eleccion = eleccion,
                                                                                  partidos = partidos)))

                                       },
                                       #' @description Calcula el partido ganador por nivel entre los partidos disponibles
                                       #' @param base Es la base de datos que será modificada
                                       #' @param eleccion Es el tipo de elección y su año separado por "_".
                                       #' @param tipo Es el tipo de datos que trae la base de datos, puede ser 'absoluto' o 'relativo'.
                                       #' Los absolutos tienen como prefijo 'ele', mientras que los relativos tienen como prefijo 'pct'.
                                       #' @return Regresa columnas con el prefijo 'ganador' en la misma base entregada
                                       calcular_ganador = function(base, eleccion, tipo = "absoluto", partidos = self$partidos){
                                         self[[base]][[eleccion]] <- self[[base]][[eleccion]] |>
                                           ganador_eleccion(eleccion = eleccion, tipo = tipo, nivel = self$nivel[length(self$nivel)], partido = partidos)

                                         self$analisis <- self$analisis |>
                                           tibble::add_row(eleccion = eleccion,
                                                           nivel = self$nivel[length(self$nivel)], analisis = "calcular_ganador",
                                                           parametros = list(list(base = base, eleccion = eleccion,
                                                                                  tipo = tipo, partidos = partidos)))
                                       },
                                       #' @description Une todas las bases de datos que conformen la lista de la 'base'
                                       #' @param nivel es el nivel de agregación por el cual se van a unir las bases. El valor tiene que ser un símbolo (sin comillas).
                                       #' @return Regresa una única tibble con todas las bases de datos unidas como columnas
                                       colapsar_base = function(base, filtro = NULL){
                                         aux <- self[[base]] |>
                                           reduce(left_join, self$nivel[length(self$nivel)])

                                         if(!is.null(filtro)){
                                           aux <- aux |>
                                             inner_join(filtro, by = self$nivel[length(self$nivel)])

                                           self$bd <- self$bd |>
                                             inner_join(filtro, by = self$nivel[length(self$nivel)])
                                         }

                                         self[[base]] <- aux |>
                                           rename_with(~gsub("total", "participacion", .x), contains("total"))

                                         self$partidos <- gsub("total", "participacion", self$partidos)

                                         names(self$colores)[names(self$colores) == "total"] <- "participacion"
                                       },
                                       #' @description Especifica un color degradado según el número de votos obtenidos por el partido ganador.
                                       #' Se recomienda ampliamente usar la función con el parámetro tipo = "relativo" y con partidos específicos.
                                       #' @param base Es la base de datos que será modificada
                                       #' @param eleccion Es el tipo de elección y su año separado por "_".
                                       #' @param tipo Es el tipo de datos que trae la base de datos, puede ser 'absoluto' o 'relativo'.
                                       #' Los absolutos tienen como prefijo 'ele', mientras que los relativos tienen como prefijo 'pct'.
                                       #' @param nivel Es el nivel de agregación que se quiere calcular los votos relativos, puede ser 'seccion', 'municipio', 'distritol', 'distritof' y el año de elección.
                                       #' @param colores_nombrados Es un vector nombrado con los colores que se quieren asignar a los partidos.
                                       #' Los colores nombrados tienen que estar ligados a todos los partidos ganadores de la elección.
                                       obtener_degradado_ganador = function(base, eleccion, tipo = "relativo",
                                                                            colores_nombrados = self$colores,
                                                                            partidos = self$partidos){
                                         #Acá se debe incluir un objeto ya creado de colores
                                         nombres <- names(self[[base]][[eleccion]])
                                         if(tipo == "relativo"){
                                           if(sum(grepl("pct_", nombres)) == 0){
                                             self$voto_relativo(base = base, eleccion = eleccion, partido = partidos)
                                           }
                                           if(sum(grepl("ganador_", nombres)) == 0) {
                                             self$calcular_ganador(base = base, eleccion = eleccion,tipo = tipo, partido = partidos)
                                           }
                                           self[[base]][[eleccion]] <- self[[base]][[eleccion]] |>
                                             left_join(colorear_ganador_degradado(self[[base]][[eleccion]], eleccion = eleccion, colores_nombrados = colores_nombrados,
                                                                                  grupo = self$nivel[length(self$nivel)], tipo = tipo),
                                                       by = self$nivel[length(self$nivel)])
                                         } else if(tipo == "absoluto"){
                                           if(sum(grepl("ganador_", nombres)) == 0) {
                                             self$calcular_ganador(base = base, eleccion = eleccion,tipo = tipo, nivel = self$nivel[length(self$nivel)], partido = partidos)

                                           }
                                           self[[base]][[eleccion]] <- self[[base]][[eleccion]] |>
                                             left_join(colorear_ganador_degradado(self[[base]][[eleccion]], eleccion = eleccion, colores_nombrados = colores_nombrados,
                                                                                  grupo = self$nivel[length(self$nivel)], tipo = tipo),
                                                       by = self$nivel[length(self$nivel)])
                                         }

                                         self$analisis <- self$analisis |>
                                           tibble::add_row(eleccion = eleccion,
                                                           nivel = self$nivel[length(self$nivel)],
                                                           analisis = "colorear_ganador_degradado",
                                                           parametros = list(list(base = base, eleccion = eleccion,
                                                                                  tipo = tipo,
                                                                                  colores_nombrados = colores_nombrados,
                                                                                  partidos = partidos)))
                                       },
                                       obtener_indice_completo = function(base){
                                         ind <- names(self$colores) |>
                                           purrr::map2(self$colores, ~{
                                             aux <- crear_indice(self[[base]], .x, nivel = self$nivel)
                                             aux <- colorear_indice(aux, c_principal = .y, var = .x)
                                             aux <- crear_quantiles(aux, .x)
                                           })

                                         self[[base]] <- self[[base]] |>
                                           left_join(reduce(ind, left_join, by = self$nivel), by = self$nivel)
                                       },
                                       añadir_leyenda = function(base){
                                         self[[base]] <- self[[base]] |>
                                           left_join(crear_label(self[[base]], nivel = self$nivel), by = self$nivel)
                                       },
                                       calcular_irs = function(ano, base = NULL, c_principal = "#140a8c"){

                                         if("list" %in% class(self[[base]])){
                                           stop("No se ha ejecutado la función self$colapsar_base")
                                         }

                                         self$censo <- leer_censo(ano = ano,
                                                                  entidad = self$entidad,
                                                                  nivel = self$nivel[length(self$nivel)])

                                         self[[base]] <-
                                           self[[base]] |>
                                           left_join(calcular_irs(bd = self$censo,
                                                                  electoral = self[[base]],
                                                                  nivel = self$nivel[length(self$nivel)],
                                                                  c_principal = c_principal),
                                                     self$nivel[length(self$nivel)])

                                         self$colores <- append(self$colores, purrr::set_names(c_principal, "rezago"))

                                         self$analisis <- self$analisis |>
                                           tibble::add_row(eleccion = NULL,
                                                           nivel = self$nivel[length(self$nivel)],
                                                           analisis = "calcular_irs",
                                                           parametros = list(list(ano = ano, base = base, c_principal = c_principal)))
                                       }
                         ))


#' Clase R6 para leer y unir shapefiles
#'
#' @description
#' Se inicia leyendo un shp
#'
#' @details
#' Al shp leído se le pude agregar otrabase de datos

ElectoralSHP <- R6::R6Class("ElectoralSHP",
                            public = list(
                              shp = list(),
                              entidades = NULL,
                              #'@description
                              #'Lee un shapefile
                              #' @param unidad si es de municipio, estado, distrito, seccion, etc.
                              #' @param entidad el estado de donde es
                              #'
                              #' @return Una lista con shapefiles
                              #' @export
                              #' @examples
                              initialize = function(unidad, entidad){
                                self$entidades <- entidad
                                aux <- leer_shp(unidad, entidad)
                                self$shp <- self$shp %>% append(list(aux) %>% purrr::set_names(paste(unidad, entidad, sep = "_")))
                              },
                              print = function(){
                                cat(glue::glue("Entidad(es): {paste(self$entidades, collapse = ', ')} \n\n Shps agregados: {paste(names(self$shp), collapse = ', ')}"))
                              },
                              agregar_shp = function(unidad, entidad = NULL){
                                if(!entidad %in% self$entidades) self$entidades <- self$entidades %>% append(entidad)
                                aux <- leer_shp(unidad, entidad)
                                self$shp <- self$shp %>% append(list(aux) %>% purrr::set_names(paste(unidad, entidad, sep = "_")))
                              },
                              #'@description
                              #' Junta shapefiles
                              #' @param nivel Si la base que se va a juntar es por seccion, municipio, distrito, etc
                              #' @param bd base de datos que se le quiere pegar al shp
                              #'
                              #' @examples
                              juntar_bd = function(nivel, bd){
                                self$shp[[nivel]] <- join_shp_bd(self$shp[[nivel]], bd)
                              }
                            ))


#' Clase R6 para leer y unir shapefiles
#'
#' @description
#' Se inicia leyendo un shp
#'
#' @details
#' Al shp leído se le pude agregar otrabase de datos
#'

Tablero <- R6::R6Class("Tablero",
                       public = list(
                         info = NULL,
                         nombres_elecciones = NA,
                         initialize = function(info_seccion){
                           self$info <- info_seccion$clone()
                         },
                         agregar_eleccion = function(elecciones, nivel, bd_relacion, shp){

                           self$info$bd <- self$info$bd |>
                             dplyr::left_join(bd_relacion,
                                              by = self$info$nivel[[1]])

                           self$info$nivel <- self$info$nivel |>
                             append(nivel)

                           elecciones |>
                             purrr::walk(~{
                               self$info$partido(eleccion = .x)
                               self$info$obtener_degradado_ganador(base = "bd_partido",
                                                                   eleccion = .x)
                             })

                           self$info$colapsar_base("bd_partido")

                           # self$info$calcular_irs(ano = "2020", base = "bd_partido")

                           self$info$fusionar_shp(shp = shp,
                                                  base = "bd_partido")
                         },
                         obtener_nombres_elecciones = function(){
                           self$nombres_elecciones <- nombres_elecciones |>
                             filter(eleccion %in% na.omit(unique(self$info$analisis$eleccion)))
                         }
                       )
)
