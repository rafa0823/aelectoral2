#' Relativiza el censo
#'
#' Función de poca utilidad, se valorará su remoción
#'
#' @param bd
#'
relativizar_censo <- function(bd) {
  dicc <- diccionario_censo20 %>%
    dplyr::filter(!is.na(denominador),
                  denominador != "-")
  dicc <- dicc %>%
    filter(
      glue::glue("cen_{variable}") %in% names(bd),
      glue::glue("cen_{denominador}") %in% names(bd)
    )
  res <- purrr::map_dfc(.x = unique(dicc$denominador),
                        ~ {
                          numerador <- dicc %>%
                            dplyr::filter(denominador == .x) %>%
                            pull(variable)
                          denominador = glue::glue("cen_{.x}")
                          bd <- bd %>%
                            select(glue::glue("cen_{numerador}"), denominador) %>%
                            mutate(across(everything(),
                                          ~ .x / !!rlang::sym(denominador))) %>%
                            select(-denominador)
                        })

  res <- bd %>%
    select(-names(res)) %>%
    bind_cols(res)

  return(res)
}

#' Calcula el índice de rezago para la elección y nivel deseado.
#'
#' Añade a la base electoral en cuestión y al nivel seleccionado en el momento el cálculo de rezago social.
#'
#' @param bd Censo cargado para nivel en cuestión
#' @param electoral Base electoral cargada en el flujo de la clase Electoral
#' @param nivel nivel seleccionado: Sección, Municipio, Distrito local, Distrito Federal
#' @param c_principal color que se utilizará para representar el bajo rezago y permitirá obtener el complemento para el bajo rezago
#' @return Una base con dos columnas adicionales: el cálculo del índice de rezago y el degradado de color asociado
#' @export
calcular_irs <- function(bd, electoral, nivel, c_principal) {
  base <- bd %>%
    semi_join(electoral, nivel) |>
    transmute(
      !!rlang::sym(nivel),
      i_analf = p15ym_an / p_15ymas,
      i_asistesc = (p6a11_noa + p12a14noa) / (p_6a11 + p_12a14),
      i_edbasinc = (p15ym_se + p15pri_in + p15pri_co + p15sec_in) / p_15ymas,
      i_sdsalud = psinder / pobtot,
      i_ptierra =  vph_pisoti / vivparh_cv,
      i_nosan = 1 - ((vph_excsa + vph_letr) / vivparh_cv),
      i_noagua = vph_aguafv / vivparh_cv,
      i_nodren = vph_nodren / vivparh_cv,
      i_noelec = vph_s_elec / vivparh_cv,
      i_nolav = 1 - (vph_lavad / vivparh_cv),
      i_noref = 1 - (vph_refri / vivparh_cv)
    ) |>
    filter_all(all_vars(!is.na(.)))

  cp <- stats::prcomp(select(base,-all_of(nivel)), scale. = T)

  pred <- stats::predict(cp, newdata = base)
  pred <- as.data.frame(pred)
  pc1_pos <- pred$PC1 + abs(min(pred$PC1))
  irs <- (pc1_pos + mean(pc1_pos) / sd(pc1_pos))
  #El Método Dalenius Hodges forma estratos en los cuales la varianza es mínima intragrupos y máxima
  #intergrupos.
  ja <- stratification::strata.cumrootf(irs, CV = 0.05, Ls = 5)
  #Necesitamos incluir un valor antes y otro después de los breaks para que 'cut' entienda que debe incluir
  #los valores menores al límite inferior y mayores al límite superior.
  intervalo <- c(-Inf, ja$bh, Inf)
  if (sum(as_tibble(cp$rotation)$PC1) > 0) {
    labels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto")
  } else {
    labels = c("Muy alto", "Alto", "Medio", "Bajo", "Muy bajo")
    c_principal <- last(colortools::complementary(c_principal))
  }

  base <- bind_cols(base |> select(all_of(nivel)), rezago = irs) |>
    mutate(
      quant_rezago = cut(
        irs,
        breaks = intervalo,
        labels = labels,
        include.lowest = T
      ),
      quant_rezago = factor(quant_rezago, levels = labels)
    ) |>
    mutate(col_rezago = case_when(quant_rezago == "Muy bajo" ~ "#140A8C",
                                  quant_rezago == "Bajo" ~ "#2F2A6B",
                                  quant_rezago == "Medio" ~ "#ffffff",
                                  quant_rezago == "Alto" ~ "#666B2A",
                                  quant_rezago == "Muy alto" ~ "#828C0A"
                                  ))

  return(base)
}
