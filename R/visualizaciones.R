
# Preprocesamiento --------------------------------------------------------
obtener_absolutos <- function(bd, eleccion){
  bd |>
    as_tibble() |>
    select(contains("ele_") & contains(eleccion)) |>
    summarise(across(where(is.numeric), ~sum(.x, na.rm = T)))
}

nominal <- function(bd, eleccion){
  sum(na.omit(bd[[glue::glue("ele_nominal_{eleccion}")]]))
}

# Procesamiento -----------------------------------------------------------

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

calcular_relativos <- function(bd, partidos = bd$info$partidos, nominal) {
  bd |>
    filter(name %in% partidos & name != "participacion") |>
    mutate(pct = value/nominal,
           label = glue::glue("{scales::label_number(scale_cut = scales::cut_short_scale())(value)} ({scales::percent(pct, 1)})"))
}

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

procesar_pointrange <- function(bd, indice, partidos, elecciones = NULL, partido = FALSE){
  quantiles <- glue::glue("quant_{indice}")
  ja <- bd |>
    as_tibble() |>
    filter(!is.na(.data[[quantiles]])) |>
    select(all_of(quantiles), contains(glue::glue("{partidos}_")) & contains("pct_"), -contains("participacion")) |>
    tidyr::pivot_longer(cols = -starts_with("quant"),
                        names_to = "eleccion",
                        values_to = "voto")
  if(partido) {
    ja <- ja |>
      tidyr::separate(eleccion, c("basura", "partidos", "eleccion", "año"), "_") |>
      mutate(partidos = factor(partidos, levels = unique(partidos)))
    var <- "partidos"
  } else {
    ja <- ja |>
      mutate(eleccion = factor(stringr::str_sub(eleccion, -5, -1), levels = elecciones))
    var <- "eleccion"
  }
  ja |>
    summarise(median = median(voto, na.rm = T),
              min = quantile(voto, 0.25, na.rm = T),
              max = quantile(voto, 0.75, na.rm = T),
              .by = c(.data[[quantiles]], .data[[var]]))
}

# Visualizaciones ---------------------------------------------------------

crear_mapa <- function(shp, fill, linewidth) {
  ggplot(shp) +
    geom_sf(aes(fill = .data[[fill]]), color = "gray66", linewidth = linewidth) +
    scale_fill_identity() +
    theme_void()
}

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

graficar_pointrange <- function(bd, eje_x, grupo, colores, indice, texto = 12) {
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
    theme_minimal(base_size = texto)
}

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

