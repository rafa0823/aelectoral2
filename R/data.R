#' Diccionario del censo
#'
#' Diccionario del censo con información para le calculo relativo de variables
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{indicador}{descripcion de la variable}
#'   \item{definicion}{Rango que abarca la variable}
#'   \item{variable}{abreviatura de la variable tal como aparece en el censo}
#'   \item{unidad_de_medida}{unidad de medida}
#'   \item{unidad_de_observacion}{unidad de observacion}
#'   \item{poblacion_de_referencia}{población de referencia}
#'   \item{denominador}{denominador que relativiza la variable}
#'   \item{tema}{tema}
#' }
"diccionario_censo20"

#' Catálogo de bases de datos remotas
#'
#' Relación de carpetas y bases de datos disponibles en el almacenamiento remoto.
#'
#' @format A data frame with 275 rows and 3 variables:
#' \describe{
#'   \item{carpeta1}{Carpeta de primer nivel.}
#'   \item{capeta}{Subcarpeta.}
#'   \item{bd}{Nombre de la base de datos.}
#' }
"catalogo"

#' Claves de distritos electorales 2022
#'
#' Claves y nombres de distritos federales y locales por entidad.
#'
#' @format A data frame with 242 rows and 5 variables:
#' \describe{
#'   \item{entidad}{Clave de la entidad.}
#'   \item{distritof_22}{Clave del distrito federal (2022).}
#'   \item{nombre_distritof_22}{Nombre del distrito federal.}
#'   \item{distritol_22}{Clave del distrito local (2022).}
#'   \item{nombre_distritol_22}{Nombre del distrito local.}
#' }
"claves"

#' Claves de municipios 2022
#'
#' Claves y nombres de municipios.
#'
#' @format A data frame with 2477 rows and 2 variables:
#' \describe{
#'   \item{municipio_22}{Clave del municipio (2022).}
#'   \item{nombre_municipio_22}{Nombre del municipio.}
#' }
"claves_mun"

#' Diccionario de entidades
#'
#' Catálogo de entidades federativas con id, nombre y abreviatura.
#'
#' @format A data frame with 32 rows and 3 variables:
#' \describe{
#'   \item{id_estado}{Clave numérica del estado.}
#'   \item{estado}{Nombre del estado.}
#'   \item{abreviatura}{Abreviatura del estado.}
#' }
"diccionario"

#' Nombres de elecciones
#'
#' Etiquetas y colores asociados a cada tipo de elección.
#'
#' @format A data frame with 19 rows and 3 variables:
#' \describe{
#'   \item{Nombre}{Nombre legible de la elección.}
#'   \item{eleccion}{Clave de la elección (e.g. \code{"pm_21"}).}
#'   \item{color}{Color asociado.}
#' }
"nombres_elecciones"

#' Paleta de colores de partidos
#'
#' Colores por partido y su alcance (nacional o estatal).
#'
#' @format A data frame with 27 rows and 3 variables:
#' \describe{
#'   \item{partidos}{Clave del partido.}
#'   \item{colores}{Color hexadecimal.}
#'   \item{alcance}{Alcance del partido (\code{"nacional"} o entidad).}
#' }
"paleta"

#' Relación INE - INEGI
#'
#' Equivalencia entre claves municipales del INE y del INEGI.
#'
#' @format A data frame with 302 rows and 3 variables:
#' \describe{
#'   \item{entidad}{Clave de la entidad.}
#'   \item{mun}{Clave municipal (INE).}
#'   \item{municipio_22}{Clave municipal (INEGI, 2022).}
#' }
"relacion_ine_inegi"
