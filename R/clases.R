
#' Clase R6 que construye un conjunto de bases electorales comparables
#'
#' @description
#' Se inicia con la base de una elección en específico y a ella se le pueden ir agregando elecciones que se juntan por seccion
#'
#' @details
#' Este conjunto de bases electorales pueden ser de distintas elecciones, candidatos, años, tipos de eleccion; se puede decidir la forma en la se reparten los votos en el extranjero y las casillas especiales; así como agregar distintas variables geográficas como el municipio o el estado.

Electoral <- R6::R6Class("Electoral",
                         public = list(
                           #' @field bd base agrupada por sección con todos los datos
                           bd = NA,
                           #' @field todas base a nivel casilla procesada para ser utilizada por la clase
                           todas = NULL,
                           #' @field censo censo a nivel seccional
                           censo = NULL,
                           #' @field bd_partido base por partido; los votos de las coaliciones fueron repartidos
                           bd_partido = list(),
                           #' @field bd_candidato base agrupada por candidato
                           bd_candidato = list(),
                           #' @field shp archivo geospacial de nivel seccional que recibe los datos electorales
                           shp = list(),
                           #' @field eleccion elección para la cual se quieren obtener los datos
                           eleccion = NA_character_,
                           #' @field nivel esta clase solo recibe datos a nivel sección, este parámetro no se tiene que especificar.
                           nivel = NA_character_,
                           #' @field entidad abreviatura del estado para el cual se quieren los datos.
                           entidad = NA_character_,
                           #' @field tipo_eleccion parámetro para escoger si los datos se quieren de mayoría relativa o representación proporcional
                           tipo_eleccion = NA_character_,
                           #' @field extranjero booleano (T, F) para especificar si se quieren preservar o remover las casillas de voto extranjero
                           extranjero = NA,
                           #' @field especiales booleano (T, F) para especificar si se quieren preservar o remover las casilllas especiales
                           especiales = NA,
                           #' @field partidos vector con los partidos para los cuales se ejecutarán los métodos de las clase
                           partidos = NA_character_,
                           #' @field colores los colores asociados a los partidos seleccionados
                           colores = NA,
                           #' @field llaves vector que permite mantener un nivel además del seccional
                           llaves = NULL,
                           #' @field elecciones_agregadas vector que guarda las elecciones que se han agregado a la clase mediando `agregar_bd()`
                           elecciones_agregadas = NULL,
                           #' @field analisis base auxiliar que registra los métodos, elecciones, niveles y parámetros que se han utilizado en la clase
                           analisis = tibble::tribble(~eleccion, ~nivel, ~analisis, ~parametros),
                           #' #' Initialize: Obtener base de datos electoral
                           #' @description
                           #' Lo que hace es leer una base de datos electoral, darle formato el formato documentado para unirla o compararla con otras elecciones y la resume por sección.
                           #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                           #' @param entidad Cuando es nacional es "nac", cuando es local se pone la abreviatura oficial, por ejemplo "chis", "dgo", "mex".
                           #' @param nivel nivel para el cual se calculan los datos. En esta nueva versión solo se utiliza el nivel seccional.
                           #' @param llaves Son las claves cartográficas de los niveles. Solo se utiliza el nivel seccional.
                           #' @param tipo_eleccion Por default es "MR" refiriéndose a mayoría relativa.
                           #' @param extranjero Se refiere a si se desea incluir los votos en el extrajero, entendidos como sección 0000. El default es TRUE
                           #' @param especiales Las casillas especiales se pueden "eliminar", "repartir" o dejar como están es el parámetro default.
                           #' @param partidos Aquellos partidos para los cuales se van a realizar todas las operaciones de aelectoral en las que acote el número de partidos.
                           #' @return Un nuevo objeto 'Electoral'
                           #' @export
                           #' @examples
                           #'  Electoral$new(eleccion = "pm_21", entidad = "mex", partidos = c("morena", "pan", "pri"), extranjero = FALSE)
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
                           #' @description describe lo que se ha incluído en la clase
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
                           #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                           #'
                           #' @return La lista de coaliciones que hubieron en la elección señalada.
                           coalicion = function(eleccion){
                             if(!eleccion %in% names(self$todas)) stop("Favor de agregar la elección primero con el método agregar_bd")

                             self$partido(self$nivel[length(self$nivel)], eleccion)

                             al <- leer_alianza(self$nivel[length(self$nivel)], eleccion, self$entidad, self$bd)

                             self$candidato(al, self$nivel[length(self$nivel)], eleccion)
                           },

                           #' @description
                           #' Reparte los voto de acuerdo con las coaliciones
                           #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                           #'
                           #' @return tibble con las votos obtenidos por las coaliciones divididos por partido político
                           partido = function(eleccion){
                             aux_c <- self$bd |>
                               repartir_coalicion(nivel = self$nivel[length(self$nivel)], eleccion = eleccion)

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
                           #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                           #'
                           #' @return Una base datos con los votos repartidos por candidato.
                           candidato = function(alianzas, eleccion){
                             aux_c <- repartir_candidato(bd = self$bd_partido[[eleccion]],
                                                         alianzas, self$nivel[length(self$nivel)], eleccion)

                             self$bd_candidato <- self$bd_candidato %>%
                               append(list(aux_c) %>%
                                        purrr::set_names(eleccion))

                           },
                           #'@description
                           #' Añade una base electoral a la lista que corresponde al objeto 'bd'
                           #' @param eleccion Es el tipo de elección y su año separado por "_". Opciones posibles para 2021: pm_21, dl_21, df_21.
                           #'
                           #' @return Tibble de la base de datos con la nueva elección resumidas por sección
                           agregar_bd = function(eleccion){

                             add <- leer_base(eleccion = eleccion,
                                              entidad = self$entidad, tipo_eleccion = self$tipo_eleccion)

                             self$todas <- self$todas %>%
                               append(list(add) %>%
                                        purrr::set_names(eleccion))

                             add <- add %>%
                               self$accion_especiales(self$especiales)

                             if(!self$extranjero){
                               add <- add %>%
                                 eliminar_votoExtranjero()
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
                           #' @param bd base de datos que que se quiere juntar
                           #' @param by variable por la que se une las bds
                           #' @return lista con base de datos
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
                           eliminar_votoExtranjero = function(){
                             self$bd <- eliminar_votoExtranjero(self$bd)
                           },
                           #' @description
                           #' Añade al objeto tipo lista shp el shp definido como parámetro con los datos electorales del parámetro base.
                           #' @param shp archivo geográfico que puede ser de nivel sección, municipio, distrital federal o local.
                           #' @param base base a añadir al shp, suele ser bd_partido
                           #'
                           #' @return lista con shapefiles
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
                           },
                           #' @description Calcula los votos relativos para los partidos seleccionados
                           #' @param base Es la base de datos que será modificada
                           #' @param eleccion Es el tipo de elección y su año separado por "_".
                           #' @return base con nuevas columnas con el prefijo 'pct' en la misma base entregada
                           voto_relativo = function(base, eleccion){
                             self[[base]][[eleccion]] <-
                               self[[base]][[eleccion]] |>
                               left_join(
                                 calcular_votos_relativos(self[[base]][[eleccion]],
                                                          eleccion = eleccion,
                                                          grupo = !!rlang::sym(self$nivel[length(self$nivel)]),
                                                          partido = self$partidos),
                                 by = self$nivel[length(self$nivel)]
                               )

                             self$analisis <- self$analisis |>
                               tibble::add_row(eleccion = eleccion,
                                               nivel = self$nivel[length(self$nivel)],
                                               analisis = "voto_relativo",
                                               parametros = list(list(base = base,
                                                                      eleccion = eleccion)))
                           },
                           #' @description Calcula el partido ganador por nivel entre los partidos disponibles
                           #' @param base Es la base de datos que será modificada
                           #' @param eleccion Es el tipo de elección y su año separado por "_".
                           #' @param tipo Es el tipo de datos que trae la base de datos, puede ser 'absoluto' o 'relativo'.
                           #' Los absolutos tienen como prefijo 'ele', mientras que los relativos tienen como prefijo 'pct'.
                           #' @return Regresa columnas con el prefijo 'ganador' en la misma base entregada
                           calcular_ganador = function(base, eleccion, tipo = "absoluto"){
                             self[[base]][[eleccion]] <- self[[base]][[eleccion]] |>
                               ganador_eleccion(eleccion = eleccion, tipo = tipo,
                                                nivel = self$nivel[length(self$nivel)],
                                                partido = self$partidos)

                             self$analisis <- self$analisis |>
                               tibble::add_row(eleccion = eleccion,
                                               nivel = self$nivel[length(self$nivel)], analisis = "calcular_ganador",
                                               parametros = list(list(base = base, eleccion = eleccion,
                                                                      tipo = tipo)))
                           },
                           #' @description Une todas las bases de datos que conformen la lista del objeto base
                           #' @param base base a colapsar, suele ser bd_partido.
                           #' @param filtro subconjunto de secciones a preservar
                           #' @return Regresa una única tibble con todas las bases de datos unidas como columnas
                           colapsar_base = function(base, filtro = NULL){
                             aux <- self[[base]] |>
                               reduce(full_join, self$nivel[length(self$nivel)])

                             if(!is.null(filtro)){
                               aux <- select(as_tibble(filtro), contains(self$nivel)) |>
                                 left_join(aux, by = self$nivel[length(self$nivel)])

                               self$bd <- select(as_tibble(filtro), contains(self$nivel)) |>
                                 left_join(self$bd, by = self$nivel[length(self$nivel)])
                             }
                             self[[base]] <- aux
                           },
                           #' @description Especifica un color degradado según el número de votos obtenidos por el partido ganador.
                           #' Se recomienda ampliamente usar la función con el parámetro tipo = "relativo" y con partidos específicos.
                           #' @param base Es la base de datos que será modificada
                           #' @param eleccion Es el tipo de elección y su año separado por "_".
                           #' @param tipo Es el tipo de datos que trae la base de datos, puede ser 'absoluto' o 'relativo'.
                           #' Los absolutos tienen como prefijo 'ele', mientras que los relativos tienen como prefijo 'pct'.
                           obtener_degradado_ganador = function(base, eleccion, tipo = "relativo"){
                             if(!tipo %in% c("relativo", "absoluto")){
                               stop("Error: príncipe, 'tipo' solo puede tomar los valores 'relativo' o 'absoluto'")
                             }
                             #Acá se debe incluir un objeto ya creado de colores
                             nombres <- names(self[[base]][[eleccion]])
                             if(tipo == "relativo"){
                               if(sum(grepl("pct_", nombres)) == 0){
                                 self$voto_relativo(base = base, eleccion = eleccion)
                               }
                               if(sum(grepl("ganador_", nombres)) == 0) {
                                 self$calcular_ganador(base = base, eleccion = eleccion,tipo = tipo)
                               }
                               self[[base]][[eleccion]] <- self[[base]][[eleccion]] |>
                                 left_join(colorear_ganador_degradado(self[[base]][[eleccion]], eleccion = eleccion, colores_nombrados = self$colores,
                                                                      grupo = self$nivel[length(self$nivel)], tipo = tipo),
                                           by = self$nivel[length(self$nivel)])
                             } else if(tipo == "absoluto"){
                               if(sum(grepl("ganador_", nombres)) == 0) {
                                 self$calcular_ganador(base = base, eleccion = eleccion,tipo = tipo, nivel = self$nivel[length(self$nivel)])

                               }
                               self[[base]][[eleccion]] <- self[[base]][[eleccion]] |>
                                 left_join(colorear_ganador_degradado(self[[base]][[eleccion]], eleccion = eleccion, colores_nombrados = self$colores,
                                                                      grupo = self$nivel[length(self$nivel)], tipo = tipo),
                                           by = self$nivel[length(self$nivel)])
                             }

                             self$analisis <- self$analisis |>
                               tibble::add_row(eleccion = eleccion,
                                               nivel = self$nivel[length(self$nivel)],
                                               analisis = "obtener_degradado_ganador",
                                               parametros = list(list(base = base, eleccion = eleccion,
                                                                      tipo = tipo)))
                           },
                           #' @description función que obteiene todas las columnas relevantes para el índice de los partidos definidos en la clase.
                           #' @param base base para la cual se calcula el índice, se utiliza "bd_partido"
                           #'
                           #' @return base con columnas de índice para cada partido
                           obtener_indice_completo = function(base){
                             #Esto está parchado hasta que el vector de colores no tenga rezago
                             ind <- setdiff(names(self$colores), "rezago") |>
                               purrr::map2(setdiff(self$colores, "#140a8c"), ~{
                                 tryCatch(
                                   {
                                     aux <- crear_indice(self[[base]], .x, nivel = self$nivel[length(self$nivel)])
                                     aux <- colorear_indice(aux, c_principal = .y, var = .x)
                                     aux <- crear_quantiles(aux, .x)

                                     aux  # return the successfully processed result
                                   },
                                   error = function(e) {
                                     warning(sprintf("Error in processing %s with color %s: %s", .x, .y, e$message))
                                     NULL  # return NULL or some other indication of the error
                                   }
                                 )
                               })

                             ind <- Filter(function(x) !is.null(x), ind)

                             self[[base]] <- self[[base]] |>
                               left_join(reduce(ind, left_join, by = self$nivel[length(self$nivel)]),
                                         by = self$nivel[length(self$nivel)])

                             self$analisis <- self$analisis |>
                               tibble::add_row(eleccion = "todas",
                                               nivel = self$nivel[length(self$nivel)],
                                               analisis = "obtener_indice_completo",
                                               parametros = list(list(base = base)))
                           },
                           #' @description añade una columna con un string por unidad geográfica (sección, municipio, etc.) con una label que se usa en un leaflet.
                           #' @param base base para la cuál se añadirá la columna
                           #'
                           #' @return base con columna adicional
                           anadir_leyenda = function(base){
                             self[[base]] <- self[[base]] |>
                               left_join(crear_label(self[[base]], nivel = self$nivel), by = self$nivel)
                           },
                           #' @description calcula el índice de rezago y todas sus variables relevantes y las añade a la base seleccionada
                           #' @param ano Año del censo que se quiere utilizar
                           #' @param base base a la cual se le añaden las columnas del índice, suele ser "bd_partido"
                           #' @param c_principal color que representará bajo rezago
                           #'
                           #' @return base con columnas adicionales
                           calcular_irs = function(ano, base = NULL, c_principal = "#140a8c"){
                             tryCatch(
                               {
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

                                 if(!"rezago" %in% names(self$colores)){
                                   self$colores <- append(self$colores, purrr::set_names(c_principal, "rezago"))
                                 }

                                 self$analisis <- self$analisis |>
                                   tibble::add_row(eleccion = NULL,
                                                   nivel = self$nivel[length(self$nivel)],
                                                   analisis = "calcular_irs",
                                                   parametros = list(list(ano = ano, base = base, c_principal = c_principal)))
                               },
                               error = function(e) {
                                 warning(e$message)
                                 NULL  # return NULL or some other indication of the error
                               }
                             )
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
                              #' @field shp lista con los shps cargados
                              shp = list(),
                              #' @field entidades entidad para la cual se quiere cargar el shp
                              entidades = NULL,
                              #'@description
                              #'Lee un shapefile
                              #' @param unidad si es de municipio, estado, distrito, seccion, etc.
                              #' @param entidad el estado de donde es
                              #'
                              #' @return Una lista con shapefiles
                              #' @export
                              initialize = function(unidad, entidad){
                                self$entidades <- entidad
                                aux <- leer_shp(unidad, self$entidades)
                                if(unidad == "secc_22") {
                                  aux <- aux |>
                                    left_join(claves |>
                                                distinct(distritol_22, nombre_distritol_22) |>
                                                na.omit(),
                                              join_by(distritol_22)) |>
                                    left_join(claves |>
                                                distinct(distritof_22, nombre_distritof_22) |>
                                                na.omit(),
                                              join_by(distritof_22)) |>
                                    left_join(claves_mun, join_by(municipio_22))
                                }
                                self$shp <- self$shp %>% append(list(aux) %>% purrr::set_names(paste(unidad, entidad, sep = "_")))
                              },
                              #' @description muestra qué se ha incluído en la clase
                              print = function(){
                                cat(glue::glue("Entidad(es): {paste(self$entidades, collapse = ', ')} \n\n Shps agregados: {paste(names(self$shp), collapse = ', ')}"))
                              },
                              #' @description añade otro shp al objeto shp
                              #' @param unidad nivel de shp que se quiere cargar
                              #' @param entidad entidad para la que se quiere datos, se hereda del método `Electoral$new`
                              #' @return lista con un nuevo shp
                              agregar_shp = function(unidad, entidad = NULL){
                                if(!entidad %in% self$entidades) self$entidades <- self$entidades %>% append(entidad)
                                aux <- leer_shp(unidad, entidad)
                                if (unidad == "dl_22"){
                                  aux <- aux |>
                                    left_join(claves |>
                                                distinct(distritol_22, nombre_distritol_22) |>
                                                na.omit(),
                                              join_by("distritol_22"))
                                } else if(unidad == "df_22"){
                                  aux <- aux |>
                                    left_join(claves |>
                                                distinct(distritof_22, nombre_distritof_22) |>
                                                na.omit(),
                                              join_by("distritof_22"))
                                }
                                self$shp <- self$shp %>% append(list(aux) %>% purrr::set_names(paste(unidad, entidad, sep = "_")))
                              },
                              #'@description
                              #' Junta shapefiles
                              #' @param nivel Si la base que se va a juntar es por seccion, municipio, distrito, etc
                              #' @param bd base de datos que se le quiere pegar al shp
                              juntar_bd = function(nivel, bd){
                                self$shp[[nivel]] <- join_shp_bd(self$shp[[nivel]], bd)
                              }
                            ))


#' Clase R6 para replicar las operaciones de la clase Electoral para otros niveles: municipio, distrito local, distrito federal
#'
#' @description
#' Esta clase requiere una clase Electoral. Dicha clase tiene un registro de qué métodos fueron utilizados, mismo que son replicados para el nuevo nivel de interés
#'
#'
Tablero <- R6::R6Class("Tablero",
                       public = list(
                         #' @field info objeto que recoge todos los elementos de la clase Electoral
                         info = NULL,
                         #' @field nombres_elecciones objeto que guarda la relación entre la elección seleccionada 'pm_21' y su nombre 'Presidente municipal 21'
                         nombres_elecciones = NA,
                         #' @field graficas objeto que incorpora las funciones de la clase `Graficar`
                         graficas = NA,
                         #' @field aux objeto que recoge las bases transformadas por la función `Tablero$filtrar`
                         aux = NA,
                         #' @description
                         #' esta clase replica todo lo hecho en la clase Electoral para otros niveles de agregación
                         #' @param info_seccion es un único parámetro que es la clase Electoral
                         initialize = function(info_seccion){
                           self$info <- info_seccion$clone()
                           self$reiniciar_info()
                           self$graficas <- Graficas$new(self)
                         },
                         #' @description replica todas las operaciones realizadas para el nivel seccional a los niveles seleccionados
                         #' @param elecciones elecciones para las que hay que obtener el análisis
                         #' @param nivel nivel de agregación para el que se quiere el análisis
                         #' @param bd_relacion base que sirve de puente entre las secciones y el nivel que se desea.
                         #' @param shp archivos al cual se le añadirán los datos colapsados. Debe ser del mismo nivel que el parámetro 'nivel'
                         agregar_eleccion = function(elecciones, nivel, bd_relacion, shp){
                           self$info$bd <- self$info$bd |>
                             dplyr::left_join(bd_relacion,
                                              by = self$info$nivel[[1]])

                           self$info$nivel <- self$info$nivel |>
                             append(nivel)

                           analisis <- self$info$analisis |>
                             filter(nivel == !!self$info$nivel[1])

                           elecciones |>
                             purrr::walk(~{
                               ana_aux <- analisis |>
                                 filter(eleccion == !!.x)

                               ana_aux |>
                                 nrow() |> seq_len() |>
                                 purrr::walk(~{
                                   aux <- ana_aux |>
                                     slice(.x)
                                   ana <- aux$analisis
                                   params <- aux$parametros |>
                                     purrr::flatten()

                                   if(ana == "partido") do.call(self$info$partido, params)
                                   if(ana == "voto_relativo") do.call(self$info$voto_relativo, params)
                                   if(ana == "calcular_ganador") do.call(self$info$calcular_ganador, params)
                                   if(ana == "obtener_degradado_ganador") do.call(self$info$obtener_degradado_ganador, params)
                                 })
                             })

                           #luego pensamos cómo hacerle para quitar bd_partido y ponerlo como parametro por si se requiere bd o bd_candidato

                           self$info$colapsar_base("bd_partido")

                           if("obtener_indice_completo" %in% analisis$analisis) {
                             params <- analisis |>
                               filter(analisis == "obtener_indice_completo") |>
                               pull(parametros) |>
                               purrr::flatten()

                             do.call(self$info$obtener_indice_completo, params)
                           }

                           if("calcular_irs" %in% analisis$analisis) {
                             params <- analisis |>
                               filter(analisis == "calcular_irs") |>
                               pull(parametros) |>
                               purrr::flatten()

                             do.call(self$info$calcular_irs, params)
                           }

                           self$info$fusionar_shp(shp = shp,
                                                  base = "bd_partido")
                           self$reiniciar_info()
                         },
                         #' @description reinicia la bd_partido
                         reiniciar_info = function(){
                           #luego pensamos cómo hacerle para quitar bd_partido y ponerlo como parametro por si se requiere bd o bd_candidato
                           self$info$bd_partido <- list()
                         },
                         #' @description Genera un objeto auxiliar para mejorar el nombre de los niveles que se han incluido en la clase
                         obtener_nombres_elecciones = function(){
                           nombres <- tibble(nombres = c("Sección", "Municipio", "Distrito local", "Distrito federal"),
                                             niveles = c("seccion", "municipio_22", "distritol_22", "distritof_22"))

                           self$nombres_elecciones <- nombres_elecciones |>
                             filter(eleccion %in% na.omit(unique(self$info$analisis$eleccion)))

                           self$info$nivel <- set_names(self$info$nivel, filter(nombres, niveles %in% self$info$nivel)$nombres)
                         },
                         #' @description cambia todos los lugar donde aparezca la columna "total" para que se llame "participación"
                         cambiar_nombre_participacion = function(){
                           self$info$nivel |>
                             purrr::walk(~{
                               self$info$shp[[.x]] <- self$info$shp[[.x]] |>
                                 rename_with(~gsub("total", "participacion", .x), contains("total"))

                               self$info$partidos <- gsub("total", "participacion", self$info$partidos)

                               names(self$info$colores)[names(self$info$colores) == "total"] <- "participacion"
                             })
                         },
                         #' @description crea una lista con los objetos filtrados
                         #' @param nivel del conjunto de niveles incluidos en la clase, cuál se quiere filtrar
                         #' @param unidad por si se quiere especificar una unidad del nivel
                         filtrar = function(nivel = "municipio_22", unidad = NULL){
                           shp <- self$info$shp[[nivel]]
                           shp_secc <- self$info$shp[["seccion"]]
                           general <- self$info$bd

                           if(!is.null(unidad)){
                             shp <- shp |>
                               filter(.data[[nivel]] == unidad)
                             shp_secc <- shp_secc |>
                               filter(.data[[nivel]] == unidad)
                             general <- general |>
                               filter(seccion %in% shp_secc$seccion)
                           }

                           self$aux <- list(shp_secc = shp_secc, shp = shp, general = general)
                         }
                       )
)

#' Clase R6 para procesar y graficar los datos electorales
#'
#' @description
#' Se inicia definiendo la unidad geográfica de trabajo mediante el método $filtrar.
#' Se puede definir un nivel y una unidad geográfica.
#'
#' @details
#' Algunas funciones tienen parámetros espécificos, pero otras solo tienes que llamarlas y usan los datos ya incluidos en la clase tablero.
#'
Graficas <-  R6::R6Class("Graficas",
                         public = list(
                           #' @field tab Recibe como un único insumo la clase tablero
                           tab = NULL,
                           #' Initialize: Obtener los datos incorporados en clase
                           #' @description
                           #' Recupera los datos de la clase tablero
                           #' @param tablero Clase tablero
                           #' @export
                           initialize = function(tablero){
                             self$tab = tablero
                           },
                           #' @description crea un mapa con el coloreo que se desee
                           #' @param seccion variable booleana para especificar que el mapa que se quiere ese de secciones o no
                           #' @param fill variable con la cual se va a colorear el mapa
                           #' @param linewidth ancho de la línea, parámetro heredado de `geom_sf`
                           #' @param labels booleano para definir si se quieren leyendas en el mapa
                           #'
                           #' @return objeto tipo ggplot
                           mapa = function(seccion, fill, linewidth = 0.6, labels = F){
                             nivel = if_else(seccion == T, "shp_secc", "shp")
                             mapa <- crear_mapa(self$tab$aux[[nivel]], glue::glue("col_{fill}"), linewidth = linewidth)
                             if(labels == T){
                               var <- names(self$tab$aux[[nivel]])[grepl("nombre", names(self$tab$aux[[nivel]]))]
                               mapa <- mapa +
                                 ggsflabel::geom_sf_label_repel(data = self$tab$aux[[nivel]], aes(label = .data[[var]]))
                             }
                             return(mapa)
                           },
                           #' @description grafica de barras de secciones ganadas
                           #' @param bd es el shape de secciones luego de usar la función  `Tablero$filtrar`
                           #' @param eleccion elección de interés
                           #' @param eje_x leyenda del eje x
                           #' @param eje_y leyenda del eje y
                           #'
                           #' @return objeto tipo ggplot
                           secciones_ganadas = function(bd = self$tab$aux$shp_secc, eleccion, eje_x = "", eje_y = ""){
                             procesar_secciones_ganadas(bd, eleccion) |>
                               graficar_barras(x = "ganador", y = "pct", fill = "ganador", label = "label", colores = self$tab$info$colores,
                                               eje_x = eje_x, eje_y = eje_y)
                           },
                           #' @description gráfica de barras con el total de votos obtenidos en términos absolutos y relativos
                           #' @param partidos heredado de la clase Tablero, partidos definidos desde la clase Electoral
                           #' @param eleccion elección de interés, debe estar en la clase Electoral
                           #' @param eje_x leyenda del eje x
                           #' @param eje_y leyenda del eje y
                           #'
                           #' @return objeto de ggplot
                           voto_relativo = function(partidos = self$tab$info$partidos, eleccion, eje_x = "", eje_y = ""){
                             obtener_absolutos(self$tab$aux$shp, eleccion) |>
                               tidyr::pivot_longer(cols = everything()) |>
                               mutate(name = gsub(glue::glue('ele|_|{eleccion}'), "", name)) |>
                               calcular_relativos(partidos = partidos, nominal = nominal(bd = self$tab$aux$general, eleccion)) |>
                               graficar_barras(x = "name", y = "pct", fill = "name",
                                               label = "label", colores = self$tab$info$colores,
                                               eje_x = eje_x, eje_y = eje_y)
                           },
                           #' @description gráfica de tipo violín con la distribución de la participación
                           #' @param eleccion elección de interés, debe estar en la clase Electoral
                           #' @param eje_x leyenda del eje x
                           #' @param eje_y leyenda del eje y
                           distribucion_participacion = function(eleccion, eje_x = "", eje_y = ""){
                             self$tab$aux$shp_secc |>
                               as_tibble() |>
                               filter(!is.na(.data[[glue::glue("ganador_{eleccion}")]])) |>
                               graficar_violin(x = glue::glue("ganador_{eleccion}"),
                                               y = glue::glue("pct_participacion_{eleccion}"), fill = glue::glue("ganador_{eleccion}"),
                                               colores = self$tab$info$colores, eje_x = eje_x, eje_y = eje_y)
                           },
                           #' Gráfica de sankey
                           #' @return objeto tipo ggplot
                           sankey = function() {
                             procesar_sankey(bd = self$tab$aux$shp_secc, elecciones = self$tab$nombres_elecciones$eleccion) |>
                               ejecutar_sankey(colores = self$tab$info$colores)
                           },
                           #' Gráfica tipo pointrange
                           #' @param indice ínndice de interés
                           #' @param variables determina si la gráfica va a ser de partidos o de elecciones, solo puede tomar "partidos" o "elecciones" como varlor.
                           #' @return objeto de ggplot
                           pointrange = function(indice, variables){
                             if(!variables %in% c("partidos", "elecciones")) {
                               stop("Error: N'mbre mi bro, chécate la documentación, 'variables' no puede tomar ese valor")
                             }

                             if(variables == "partidos"){
                               procesar_pointrange(bd = self$tab$aux$shp, indice = indice,
                                                   partidos = self$tab$info$partidos) |>
                                 graficar_pointrange(eje_x = indice,grupo = variables,
                                                     indice = indice, colores = self$tab$info$colores)

                             } else if (variables == "elecciones"){
                               procesar_pointrange(bd = self$tab$aux$shp, indice = indice,
                                                   partidos = self$tab$info$partidos,
                                                   elecciones = self$tab$nombres_elecciones$eleccion
                               ) |>
                                 graficar_pointrange(eje_x = indice, grupo = variables,
                                                     indice = indice, colores = self$tab$nombres_elecciones$color)

                             }
                           },
                           #' gráfica de mosaico
                           #' @param indice indice a comparar con el de participación
                           #' @param low color mínimo de la cloropleta
                           #' @param high color máximo de la cloropleta
                           #'
                           #' @return objeto de ggplot
                           tiles = function(indice, low = "#118ab2", high = "#ef476f"){
                             graficar_tiles(bd = self$tab$aux$shp_secc,
                                            x = "quant_participacion",
                                            y = glue::glue("quant_{indice}"),
                                            low = low,
                                            high = high,
                                            name = "Coincidencias",
                                            eje_x = "Índice de participación",
                                            eje_y = glue::glue("Índice {y}")
                             )
                           }
                         ))
