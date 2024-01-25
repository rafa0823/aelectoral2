
# Preprocesamiento --------------------------------------------------------
#' Extrae los datos absolutos de una base de datos electoral
#'
#' @param bd Base de datos electoral
#' @param eleccion elecciones de interés ('gb_17')
#'
#' @return subconjunto de valores en términos absolutos para la base entrante
#' @export
#'
obtener_absolutos <- function(bd, eleccion){
  bd |>
    as_tibble() |>
    select(contains("ele_") & contains(eleccion)) |>
    summarise(across(where(is.numeric), ~sum(.x, na.rm = T)))
}

#' Obtienes la lista nominal total de la base y la elección seleccionada
#'
#' @param bd base de datos con columna nominal
#' @param eleccion elección de interés
#'
#' @return vector numérico
#' @export
#'
nominal <- function(bd, eleccion){
  sum(na.omit(bd[[glue::glue("ele_nominal_{eleccion}")]]))
}

# Procesamiento -----------------------------------------------------------

#' Calcula el número de secciones ganadas por ganador
#'
#' @param bd base con columna de ganadores
#' @param eleccion elección de interes
#'
#' @return base procesada con estadísticos de secciones ganadas por partido
#' @export
procesar_secciones_ganadas <- function(bd, eleccion){
  bd |>
    as_tibble() |>
    filter(!is.na(.data[[glue::glue("ganador_{eleccion}")]])) |>
    select(contains("ganador_") & contains(eleccion)) |>
    group_by(across(starts_with("ganador"))) |>
    summarise(n = n()) |>
    transmute(pct = n/sum(n),
              label = glue::glue("{scales::comma(n, 1)} ({scales::percent(pct, 1)})"),
              ganador = !!rlang::sym(glue::glue("ganador_{eleccion}"))) |>
    ungroup()
}

#' Calcula los votos relativos para los partidos requeridos
#'
#' @param bd base larga con los partidos en una sola columna
#' @param partidos partidos de interés
#' @param nominal lista nominal de la unidad geográfica definida.
#'
#' @return base con dos columnas pct y label
#' @export
#'
calcular_relativos <- function(bd, partidos = bd$info$partidos, nominal) {
  bd |>
    filter(name %in% partidos & name != "participacion") |>
    mutate(pct = value/nominal,
           label = glue::glue("{scales::label_number(scale_cut = scales::cut_short_scale())(value)} ({scales::percent(pct, 1)})"))
}

#' Procesamiento para Sankey
#'
#' @param bd base desagregada a nivel sección con columna de ganador
#' @param elecciones Elecciones de interés para visualización de sankey
#'
#' @return base en formato idóneo para sankey
#' @export
procesar_sankey <- function(bd, elecciones){
  bd |>
    as_tibble() |>
    select(contains(elecciones) & contains("ganador"), seccion) |>
    rename_with(.cols = starts_with("ganador_"),
                .fn = ~stringr::str_remove(.x,"ganador_") %>%
                  stringr::str_to_upper() %>%
                  stringr::str_replace(pattern = "_", replacement = "-")) |>
    ggsankey::make_long(-seccion) |>
    mutate(node=forcats::fct_lump(node, n=6,other_level = "Otros"),
           next_node=forcats::fct_lump(next_node, n=6,other_level = "Otros"))
}

#' Procesamiento para gráfica pointrange
#'
#' @param bd base de datos electoral con columna 'quant_'
#' @param indice los cuantiles que se van a graficar en el eje x
#' @param partidos los partidos para los que se calcula el pointrange
#' @param elecciones en caso de que no sean partidos, sino elecciones, por default es NULL
#'
#' @return base con datos procesado para gráfica pointrange
#' @export
#'
procesar_pointrange <- function(bd, indice, partidos, elecciones = NULL) {
  # Create quantiles variable
  quantiles <- glue::glue("quant_{indice}")

  # Prepare and filter data
  filtered_data <- bd |>
    as_tibble() |>
    filter(!is.na(.data[[quantiles]])) |>
    select(all_of(quantiles), contains(glue::glue("{partidos}_")) & contains("pct_"), -contains("participacion")) |>
    tidyr::pivot_longer(cols = -starts_with("quant"),
                        names_to = "eleccion",
                        values_to = "voto")

  # Process data based on 'partido' flag
  if (is.null(elecciones)) {
    processed_data <- filtered_data |>
      tidyr::separate(eleccion, c("basura", "partidos", "eleccion", "año"), "_") |>
      mutate(partidos = factor(partidos, levels = unique(partidos)))
    group_var <- "partidos"
  } else {
    processed_data <- filtered_data |>
      mutate(eleccion = factor(stringr::str_sub(eleccion, -5, -1), levels = elecciones))
    group_var <- "eleccion"
  }

  # Summarize data
  summarized_data <- processed_data |>
    summarise(median = median(voto, na.rm = TRUE),
              min = quantile(voto, 0.25, na.rm = TRUE),
              max = quantile(voto, 0.75, na.rm = TRUE),
              .by = c(.data[[quantiles]], .data[[group_var]]))

  return(summarized_data)
}

# Visualizaciones ---------------------------------------------------------

#' Mapa electoral estático
#'
#' @param shp shapefile que se quiere dibujar, tiene que tener una variable con el string de los colores que se van a mostrar.
#' @param fill variable que se quiere colorear en el mapa. Para índice de morenismo se usa 'morena', para resultados de una elección se usa la elección 'pm_21'
#' @param linewidth el ancho de las líneas del mapa
#'
#' @return objeto de tipo ggplot
#' @export
#'
crear_mapa <- function(shp, fill, linewidth) {
  ggplot(shp) +
    geom_sf(aes(fill = .data[[fill]]), color = "gray66", linewidth = linewidth) +
    scale_fill_identity() +
    theme_void()
}

#' Configuración general para gráfica de barras
#'
#' @param bd base electoral
#' @param x variable que irá en el eje x de la gráfica
#' @param y variable que irá en el eje y de la gráfica
#' @param fill variable por la que se colorea
#' @param label etiquetas de las barras
#' @param colores los colores a los que se asocia el parámetro fill - se utiliza scales_fill_manual
#' @param eje_x etiqueta del eje x
#' @param eje_y etiqueta del eje y
#' @param size tamaño de la letra de gráfico, parámetro de 'base_size'
#'
#' @return objeto de ggplot
#' @export
graficar_barras <- function(bd, x, y, fill, label, colores, eje_x, eje_y, size = 12){
  ggplot(bd, aes(x = reorder(.data[[x]], .data[[y]]), y = .data[[y]], fill = .data[[x]])) +
    geom_col(width = 0.6) +
    ggfittext::geom_bar_text(aes(label = .data[[label]]), contrast = T) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent) +
    scale_x_discrete(labels = toupper) +
    scale_fill_manual(values = colores, guide = 'none') +
    labs(x = eje_x, y = eje_y) +
    theme_minimal(base_size = size)
}

#' Gráfica de distribución tipo violín
#'
#' @param bd base electoral
#' @param x variable que irá en el eje x de la gráfica
#' @param y variable que irá en el eje y de la gráfica
#' @param fill variable por la que se colorea
#' @param colores los colores a los que se asocia el parámetro fill - se utiliza scales_fill_identity
#' @param eje_x etiqueta del eje x
#' @param eje_y etiqueta del eje y
#' @param size tamaño de la letra de gráfico, parámetro de 'base_size', por default tiene tamaño 12
#'
#' @return objeto tipo ggplot
#' @export
graficar_violin <- function(bd, x, y, fill, colores, eje_x, eje_y, size = 12){
  ggplot(as_tibble(bd), aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill]])) +
    geom_violin(trim = T) +
    stat_summary(fun.y = median, geom="point", size = 2, color="white") +
    scale_fill_manual(values = colores, guide = "none") +
    scale_y_continuous(labels = scales::percent,
                       limits = c(0, 1)) +
    scale_x_discrete(labels = toupper) +
    labs(x = eje_x, y = eje_y) +
    theme_minimal(base_size = size)
}

#' Gráfica sankey
#'
#' @param bd base procesada mediante `procesar_sankey`
#' @param colores colores que se utilizan para el sankey
#'
#' @return objeto tipo ggplot
#' @export
#'
ejecutar_sankey <- function(bd, colores){
  ggplot(bd, aes(x = x,
                 next_x = next_x,
                 node = node,
                 next_node = next_node,
                 color =factor(node),
                 fill = factor(node))) +
    ggsankey::geom_sankey(flow.alpha=.5) +
    scale_fill_manual(values = colores, name = "Partidos")+
    scale_color_manual(values = colores, guide = 'none') +
    theme(panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = "white"))
}

#' Gráfica tipo pointrange
#'
#' @param bd base producto de `procesar_pointrange`
#' @param eje_x variable del eje x
#' @param grupo variable que puede tomar dos valores 'partidos' o 'elecciones'
#' @param colores colores que se utilizan dentro de un scale_color_manual
#' @param indice indice graficado
#' @param size tamaño del base_size
#'
#' @return objeto tipo ggplot
#' @export
graficar_pointrange <- function(bd, eje_x, grupo, colores, indice, size = 12) {
  if(!(grupo %in% c("partidos", "elecciones"))) {
    stop("Error: 'grupo' must be either 'partidos' or 'elecciones'")
  }
  var <- if_else(grupo == "partidos", "Partidos", "Elecciones")

  ggplot(bd, aes(x = .data[[glue::glue("quant_{eje_x}")]],
                 y = median, ymin = min,
                 ymax = max, color = .data[[grupo]])) +
    geom_pointrange(size = 0.8, alpha = 0.6, linewidth = 1.5) +
    geom_line(aes(group = .data[[grupo]]), linewidth = 0.5) +
    scale_y_continuous(labels = scales::percent,
                       limits = c(min(bd$min), max(bd$max))) +
    scale_color_manual(values = colores, name = var) +
    labs(x = glue::glue("Índice de {toupper(indice)}"), y = "Voto relativo",
         #color = glue::glue("{stringr::str_to_title(var3)}")
    ) +
    theme_minimal(base_size = size)
}

#' Grafica un geom_tiles
#'
#' @description
#' Compara la coincidencia de dos variables categóricas
#'
#'
#' @param bd base electoral con variable 'quant_'
#' @param x variable categórica que se mostrará en el eje x
#' @param y variable categórica que se mostrará en el eje y
#' @param low elemento más chico del gradiente
#' @param high elemento más grande del gradiente
#' @param name nombre que se colocará en la leyenda
#' @param eje_x título del eje x
#' @param eje_y título del eje y
#' @param size tamaño del base_size
#'
#' @return objeto de ggplot
#' @export
#'
graficar_tiles <- function(bd, x, y, low, high, name, eje_x, eje_y, size = 12){
  bd <- bd |>
    as_tibble() |>
    count(var1 = .data[[x]], var2 = .data[[y]]) |>
    na.omit()

  ggplot(bd, aes(x = var1, y = var2, fill = n)) +
    geom_tile(color = "white", lwd = 1.5) +
    scale_fill_gradient2(low = low, mid = "white", high = high, midpoint = mean(bd$n),
                         name = name, na.value = "gray33") +
    guides(
      fill = guide_colorbar(
        barwidth = unit(5, 'cm'),
        barheight = unit(0.25, 'cm'),
        title.position = 'top'
      )
    ) +
    labs(x = eje_x,
         y = eje_y,
         title = "",
         subtitle = "") +
    theme_minimal(base_size = size) +
    theme(panel.grid = element_blank(),
          plot.title.position = 'plot',
          plot.subtitle = element_text(
            margin = margin(b = 0.5, unit = 'cm')
          ),
          legend.position = c(0.37, 1.3),
          legend.direction = 'horizontal') +
    coord_cartesian(expand = FALSE)
}

