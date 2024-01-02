

#' Title
#' Resultado de las diferencias entre elecciones, eligiendo una como contraste
#'
#' @param bd base de datos con resultados electorales
#' @param partido partidos o vector de partidos (ver base de datos de partidos)
#' @param eleccion_referencia elección con la que se contrastarán las elecciones.
#' @param eleccion_contraste elección o vector de elecciones contra los que se comparan
#' la elecciones de referencia.
#'
#' @return base con una columna de diferencias y  con el prefijo "dif_"
#' @export
#' @import dplyr purrr
#' @examples
#' #NOT RUN
#' #calcular_diferencias(edomex::edomex_final, partido = c("pvem", "pt"),
#' #eleccion_referencia =  "df_21", eleccion_contraste = c("dl_21", "pm_21"))

calcular_diferencias <- function(bd, partido, eleccion_referencia, eleccion_contraste){
  res <- map(partido,
             ~   {
               eleccion_referencia <- paste("ele", .x, eleccion_referencia, sep = "_")
               eleccion_contraste <- paste("ele", .x, eleccion_contraste, sep="_")
               bd <- map(eleccion_contraste,
                         ~ {
                           bd %>%
                             mutate("dif_{stringr::str_remove(eleccion_referencia, 'ele_')}_{stringr::str_sub(string = .x, start = -5, end = -1)}":=!!sym(eleccion_referencia)-!!sym(.x))
                         }
               ) %>%
                 reduce(full_join)
               return(bd)

             })%>%
    reduce(full_join) %>%
    as_tibble()
  return(res)
}



#' Title
#' Obtiene el porcentaje de votos obtenidos por partidos con respecto a la lista nominal
#'
#' @param bd base de datos con resultados electorales
#' @param partido partidos o vector de partidos (ver base de datos de partidos)
#' @param eleccion elección o vector de elecciones de la cual se van a obtener los totales
#' @param grupo unidad de análisis que se desea observar (sección, distrito, municipio)
#'
#' @return base de datos con cada una de las votaciones totales por partido en las elecciones solicitadas
#' @export
#' @import dplyr purrr
#' @examples
calcular_votos_relativos <- function(bd, partido, eleccion, grupo){
  res  <- map(eleccion,
              ~{
                sufijo <- paste("ele",partido, .x, sep = "_")
                nominal <-  paste("ele_nominal", .x, sep = "_")
                bd %>%
                  group_by({{grupo}}) %>%
                  summarise(across(any_of(sufijo),
                                   ~sum(.x, na.rm = T)/sum(!!sym(nominal),
                                                           na.rm=T))) %>%
                  filter(!is.na({{grupo}}))
              }) %>%
    reduce(full_join) |>
    rename_with(~gsub("ele_","pct_", .x), starts_with("ele_"))
  return(res)
}


#' Title
#' Obtiene el total de votos obtenidos por partido
#' @param bd base de datos con resultados electorales
#' @param partido partidos o vector de partidos (ver base de datos de partidos)
#' @param eleccion elección o vector de elecciones de la cual se van a obtener los totales
#' @param grupo nivel de observación de la gráfica (secciones, municipios, distritos)
#'
#' @return base de datos con cada una de las votaciones totales por partido en las elecciones solicitadas
#' @export
#' @import dplyr purrr
#' @examples
calcular_votos_totales <- function(bd, partido, eleccion, grupo=NULL){
  res <- bd %>% {if(!rlang::quo_is_null(enquo(grupo))) bd %>% group_by({{grupo}}) else .} %>%
    summarise(across(matches(cross(list(partido, eleccion)) %>%
                               map_chr(.f = ~.x %>% unlist() %>% paste(collapse="_")) %>%
                               paste("ele",., sep="_")),
                     ~sum(.x, na.rm = T)))

  return(res)
}


#' Title
#' Obtiene el partido ganador para cada una de las observaciones de la base. Puede ser sección, municipio o distrito
#' @param bd base de datos con resultados electorales
#' @param eleccion elección o vector de elecciones de la cual se van a obtener los ganadores
#'
#' @return base de datos con una columna que indica el partido ganador de cada eleccion referida
#' @export
#' @import dplyr purrr
#' @examples
ganador_eleccion <- function(bd, eleccion, tipo = NULL, nivel, partido = NULL){
  if(is.null(partido)){
    partido <- extraer_partidos(bd, eleccion, tipo)
  }

  res <- bd %>%
    select(nivel, matches(glue::glue("ele_{partido}_{eleccion}"))) |>
    mutate(ganador = pmap(across(ends_with(glue::glue("_{eleccion}")) &
                                   -contains("_nominal_") &
                                   -contains("_total_")),
                          ~ names(c(...)[which.max(c(...))])),
           ganador =stringr::str_remove(string = ganador, "ele_|cand_|pct_") %>%
             stringr::str_remove(string = ., pattern = glue::glue("_{eleccion}"))
    ) %>%
    rename("ganador_{eleccion}":= ganador) %>%
    as_tibble() |>
    left_join(select(bd, nivel, !contains("ele")), by = nivel)
  return(res)
}


#' Title
#' Se utiliza un método matemático llamada análisis de componentes principales (PCA por sus siglas en inglés) que captura los patrones de votación y los sintetiza en una gráfica de dos dimensiones.
#' @param bd base de datos con resultados electorales
#' @param eleccion elección o vector de elecciones de la cual se van a obtener los ganadores
#' @param año año de las elecciones seleccionadas
#' @param partido vector que partidos que se van a graficar
#' @param grupo nivel de observación de la gráfica (secciones, municipios, distritos)
#' @return gráfica de modelo pca
#' @export
#' @import dplyr purrr ggplot2
#' @examples
crear_mapa_electoral <- function(bd,
                                 eleccion,
                                 año,
                                 grupo=NA,
                                 partido=NA){
  base <- bd %>% calcular_votos_relativos(partido = partido,
                                          eleccion = eleccion,grupo = !!enquo(grupo)) %>% na.omit()
  pca_modelo <- base %>%
    select(-{{grupo}}) %>%
    stats::prcomp(scale=T)

  calidad <- pca_modelo %>%
    broom::tidy(matrix = "eigenvalues") %>%
    filter(PC<=2)

  g <- pca_modelo %>%
    broom::tidy(matrix = "rotation") %>%
    tidyr::pivot_wider(names_from = "PC", names_prefix = "PC", values_from = "value") %>%
    mutate(column=stringr::str_remove(column, "ele_") %>%
             stringr::str_remove(glue::glue("_{eleccion}_{stringr::str_sub(año, -2, -1)}"))) %>%
    ggplot(aes(PC1, PC2)) +
    ggrepel::geom_text_repel(hjust="inward",
                             aes(label = column), nudge_x = -0.02,
                             color = "#904C2F"
    ) +
    geom_vline(xintercept = 0)+
    geom_hline(yintercept = 0)+
    coord_fixed(ratio = 1/1) +
    labs( title = "Mapa electoral",
          subtitle = glue::glue("VE {scales::percent(last(calidad$cumulative),accuracy=1)}"),
          x="CP1 ()",
          y="CP2 ()",
          caption = glue::glue("{scales::comma(nrow(base), accuracy=1)} observaciones"))+
    theme_void()
  # Output
  res <- list(bd=base,
              modelo=pca_modelo,
              gráfico=g)
  return(res)
}



#' Title
#' Se obtiene una gráfica con las cantidades de secciones ganadas por cada uno de los partidos en cada elección.
#' De esta manera, al comparar una elección con otra vemos las secciones que un partido ganó en una elección pero no en otra y qué partido ganó dichas secciones.
#' @param bd base de datos con resultados electorales
#' @param eleccion elección o vector de elecciones de la cual se van a obtener los ganadores
#' @param grupo nivel de observación de la gráfica (secciones, municipios, distritos)
#' @import dplyr purrr ggplot2
#' @return Se obtiene una gráfica con el comparativo de las cantidades de secciones ganadas por cada uno de los partidos en cada elección.
#' @export
#'
#' @examples
graficar_sankey_ganadores <- function(bd, elecciones, unidad_analisis){
  bd <- elecciones %>%
    map(~bd %>% ganador_eleccion(eleccion = .x)) %>%
    reduce(full_join)
  bd <- bd %>% select({{unidad_analisis}}, starts_with("ganador_"))
  bd <- bd %>%
    rename_with(.cols = starts_with("ganador_"),
                .fn = ~stringr::str_remove(.x,"ganador_") %>%
                  stringr::str_to_upper() %>%
                  stringr::str_replace(pattern = "_", replacement = "-")) %>%
    make_long(-{{unidad_analisis}}) %>%
    mutate(node=forcats::fct_lump(node, n=6,other_level = "Otros"),
           next_node=forcats::fct_lump(next_node, n=6,other_level = "Otros"),
    )
  ggplot(bd, aes(x = x,
                 next_x = next_x,
                 node = node,
                 next_node = next_node,
                 color =factor(node),
                 fill = factor(node))) +
    geom_sankey(flow.alpha=.5) +
    scale_fill_manual(values = colores_partidos, name="Partidos")+
    scale_color_manual(values = colores_partidos, guide="none")


}


#' Title
#' grafica una cloropeta con la distribución del apoyo del partido solicitado.
#' una vez coloreada la sección se hace un degradado en función de la intensidad con la que dicho partido resultó ganador.
#' Es decir, se colorea más fuerte mientras mayor haya sido la victoria y menos fuerte si fue una sección muy competida.
#' @param bd base de datos con resultados electorales
#' @param shp archivo .shp con el polígono del nivel de observación correspondiente
#' @param colores_nombrados vector compuesto con los nombres de partidos y colores que le corresponden
#' @param eleccion elección o vector de elecciones de la cual se van a obtener los ganadores
#' @param grupo nivel de observación de la gráfica (secciones, municipios, distritos)
#' @import dplyr purrr ggplot2
#' @return mapa con la intensidad de apoyo por partido
#' @export
#' @examples
graficar_cloropeta <- function(bd, shp, colores_nombrados, eleccion, grupo){
  partido <- names(colores_nombrados)
  bd <- calcular_votos_relativos(bd=bd,
                                 partido = partido,
                                 eleccion=eleccion,
                                 grupo = !!rlang::enquo(grupo))
  bd <- bd %>%
    pivot_longer(cols = matches(cross(list(partido, eleccion)) %>%
                                  map_chr(.f = ~.x %>% unlist() %>% paste(collapse="_")) %>%
                                  paste("ele",., sep="_")),
                 names_to = c("partido","eleccion", "ano"),
                 values_to =  "votos",
                 names_prefix = "ele_",
                 names_sep = "_"
    )
  valor_referencia <- max(bd$votos, na.rm = T)
  bd <- degradar_color_partido(bd, nombre=partido, variable = votos, colores_nombrados = colores_nombrados,valor_maximo = valor_referencia)
  res <- bd %>%
    split(list(.$partido,.$eleccion, .$ano)) %>%
    map(~{
      bd <- left_join(shp,
                      .x,by="seccion") %>%
        filter(!is.na(votos))

      ggplot() +
        geom_sf(data=bd, aes(fill=color), size=0) +
        scale_fill_identity() +
        labs( title = glue::glue("{toupper(unique(bd$eleccion))} 20{unique(bd$ano)}"))

    })

  return(res)
}






#' Title
#' Gráfica de líneas con el total de votos comparándolos entre elecciones.
#' @param bd base de datos con resultados electorales
#' @param colores_nombrados vector compuesto con los nombres de partidos y colores que le corresponden
#' @param eleccion elección o vector de elecciones de la cual se van a obtener los ganadores
#' @param grupo nivel de observación de la gráfica (secciones, municipios, distritos)
#' @import dplyr purrr ggplot2
#' @return Grafica de líneas con los totales de las elecciones solicitadas
#' @export
#'
#' @examples
graficar_totales_eleccion <- function (bd, colores_nombrados, eleccion, grupo = NULL)
{
  partido <- names(colores_nombrados)

  bd <- bd %>% pivot_longer(cols = starts_with("ele"),
                            names_to = c("partido",
                                         "eleccion", "ano"), names_prefix = "ele_", names_sep = "_",
                            values_to = "resultado") %>%
    mutate(eleccion = toupper(eleccion),
           eleccion = paste(eleccion, ano, sep = " "),
           eleccion = forcats::fct_relevel(eleccion,
                                           c("GB 17", "DF 18", "DL 18", "PM 18", "PR 18", "DF 21",
                                             "DL 21", "PM 21"))
    )
  grafica <- bd %>% ggplot(aes(x = eleccion,
                               y = resultado, group = partido)) +
    geom_line(aes(color = partido), alpha = 0.9) +
    geom_point(aes(group = eleccion, color = partido)) + scale_color_manual(values = colores_nombrados) +
    scale_y_continuous(label = scales::comma) +
    geom_text(aes(x = eleccion,
                  y = resultado, label = scales::comma(round(resultado, 0))), vjust = 0, nudge_y = 10000) + labs(x = "Elección",
                                                                                                                 y = "Total de votos", color = "Partido")
  return(grafica)
}

#' Title
#'
#' @param bd base con resultados electorales y una columna adicional con el ganador de cada sección
#' @param ganador partido del que se busca analizar la independencia de su resultado
#' @param eleccion elección elegida para analizar
#' @param ... variables del censo de las que se busca analizar la independencia
#'
#' @return
#' @export
#'
#' @examples
probar_independencia_ganador <-function(bd, ganador, eleccion, ...){
  bd <- bd %>% mutate(triunfo=as.factor(!!sym(glue::glue("ganador_{eleccion}"))==ganador))
  dots <- enquos(...)
  res <- map_df(dots,
                ~{
                  prueba_formula <- rlang::new_formula(rlang::quo_get_expr(.x),
                                                       quote(triunfo))
                  prueba_t <- infer::t_test(formula = prueba_formula,
                                            alternative = "two-sided",
                                            x = bd)
                  prueba_wc <- broom::tidy(wilcox.test(prueba_formula, data=bd))
                  resultado <- tibble(explicativa=rlang::expr_text(enquo(.x)),
                                      p_prueba_t=prueba_t$p_value,
                                      p_prueba_wc=prueba_wc$p.value) %>%
                    mutate(determinacion_prueba_t=if_else(p_prueba_t>.05,
                                                          "Rechazo dependencia",
                                                          "Rechazo independencia"),
                           determinacion_prueba_wc=if_else(p_prueba_wc>.05,
                                                           "Rechazo dependencia",
                                                           "Rechazo independencia"))
                })
  return(res)
}

extraer_partidos <- function(bd, eleccion, tipo){
  prefijo <- if_else(tipo == "relativo", "pct_", "ele_")
  bd %>%
    select(matches(glue::glue("{prefijo}|cand_"))) %>%
    names() %>%
    stringr::str_remove(glue::glue("{prefijo}|cand_")) %>%
    stringr::str_remove(eleccion) %>%
    stringr::str_remove("nominal_") %>%
    stringr::str_remove("total_") %>%
    stringr::str_remove("noreg_") %>%
    stringr::str_remove("nulos_") %>%
    (function(x) gsub("_", "", x)) () |>
    unique()
}

#' @title Cálculo de índice para variables electorales
#' @description
#' Función que calcula el índice para la variable o variables que se le soliciten.
#' Requiere que la base de datos esté en términos relativos.
#' La función estandariza los resultados para que no importe el signo de los datos.
#' @param bd Base de datos en términos relativos
#' @param partido variable para la cual se quiere calcular el índice.
#' @param nivel El nivel de agregación en el que se encuentran los datos.
#' Si la base de datos tiene más de uno, se pueden incluir dentro de la función.
#'
crear_indice <- function(bd, partido, nivel){
      bd_partido <- bd |>
        as_tibble() |>
        select(all_of(nivel), contains(c(glue::glue("pct_{partido}_")))) |>
        mutate(across(where(is.numeric), ~tidyr::replace_na(.x, 0))) |>
        na.omit()

      pca_modelo <- bd_partido |>
        select(-all_of(nivel)) |>
        stats::prcomp(scale. = T)

      aux <- pca_modelo %>%
        broom::tidy(matrix = "rotation") %>%
        tidyr::pivot_wider(names_from = "PC", names_prefix = "PC",
                           values_from = "value")

      pred <- predict(pca_modelo, newdata = bd_partido)
      pred <- as.data.frame(pred)
      if(sum(aux$PC1) > 0) {
        ind <- (pred$PC1 + mean(pred$PC1) / sd(pred$PC1))
      } else{
        ind <- (pred$PC1 + mean(pred$PC1) / sd(pred$PC1)) *-1
      }
      bd_partido <- cbind(bd_partido, ind = ind)

      bd_partido <- bd_partido |>
        select(all_of(nivel), ind) |>
        rename_with(~glue::glue("{partido}"), ind)

      return(as_tibble(bd_partido))
}

#' @title Creación de paletas para índice
#' @description
#' Añade una nueva columna a la base de datos. Recibe una variable de tipo índice, calcula el color complementario
#' a un color entregado y relaciona los valores del índice con un color de la paleta.
#' @param bd Base de datos con columna de índice.
#' @param c_principal el color que tomará el valor más alto del índice
#' @param var la variable de tipo índice a la que se le asociará el color.

colorear_indice <- function(bd, c_principal, var){
  no_principal <- last(colortools::complementary(c_principal, plot = F))

  bd <- bd %>% mutate(col := !!rlang::sym(var))

  colorear <- leaflet::colorQuantile(colorRamp(c(no_principal,"white", c_principal),
                                               space = "Lab",bias=1.5,
                                               interpolate="spline"),
                                     domain = bd[["col"]], n = 10)

  bd %>% mutate(!!rlang::sym(glue::glue("col_{var}")) := colorear(col)) %>% select(-col)

}

crear_quantiles <- function(bd, partido, grupos = 4){
  bd |>
    mutate(quant = dvmisc::create_qgroups(!!rlang::sym(partido), groups = grupos),
           quant = factor(quant, labels = c("Nada", "Poco", "Algo", "Mucho"))) |>
    rename_with(~glue::glue("quant_{partido}"), quant)
}

crear_label <- function(bd, nivel){
  votos <- bd |>
    as_tibble() |>
    ungroup() |>
    select(all_of(nivel), contains("pct")) |>
    mutate(across(contains("pct"), ~scales::percent(.x,1))) |>
    tidyr::pivot_longer(-!!rlang::sym(nivel)) |>
    tidyr::separate(col = name, into = c("basura", "partido", "eleccion", "ano")) |>
    mutate(
      partido = if_else(partido == "total", "Participacion", toupper(partido)),
      label = glue::glue("Votos {partido}: {value}")) |>
    summarise(label_v = paste(label, collapse = "<br>"), .by = c(!!rlang::sym(nivel), eleccion, ano))

  if(sum(grepl("ganador", names(bd))) > 0) {
    ganador <- bd |>
      as_tibble() |>
      ungroup() |>
      select(all_of(nivel), contains("ganador")) |>
      tidyr::pivot_longer(-!!rlang::sym(nivel)) |>
      tidyr::separate(col = name, into = c("basura", "eleccion", "ano")) |>
      summarise(label_g = glue::glue("Ganador: {toupper(value)}<br>"), .by = c(!!rlang::sym(nivel), eleccion, ano))
  }

  if(sum(grepl("quant", names(bd))) > 0){

    indice <- bd |>
      select(all_of(nivel), contains("quant")) |>
      tidyr::pivot_longer(-!!rlang::sym(nivel)) |>
      tidyr::separate(col = name, into = c("basura", "partido")) |>
      mutate(label = glue::glue("Indice {toupper(partido)}: {value}")) |>
      summarise(label_ind = paste(label, collapse = "<br>"), .by = c(!!rlang::sym(nivel))) |>
      mutate(label_ind_2 = paste("Seccion:", stringr::str_sub(seccion, -4, -1), "<br>", label_ind))

  }

  label <- votos %>%
    { if (exists("ganador")) left_join(., ganador, by = join_by(!!sym(nivel), eleccion, ano)) else . } %>%
    { if (exists("indice")) left_join(., indice, by = join_by(!!sym(nivel))) else . } %>%
    transmute(!!rlang::sym(nivel),
              eleccion = paste(eleccion, ano, sep = "_"),
              label = paste(glue::glue("Seccion: {stringr::str_sub(seccion, -4, -1)}"), label_g, label_v, label_ind, sep = "<br>"),
              label_ind_2) |>
    tidyr::pivot_wider(id_cols = !!rlang::sym(nivel), names_from = eleccion, values_from = label) |>
    rename_with(~paste0("label_", .x), -!!rlang::sym(nivel)) %>%
    { if (exists("indice")) left_join(., select(indice, all_of(nivel), label_ind_2), by = join_by(!!sym(nivel))) else . }
}

