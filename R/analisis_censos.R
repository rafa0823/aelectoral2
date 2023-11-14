#' Title
#'
#' @param bd base de datos electoral
#'
#' @return
#' @export
#'
#' @examples
relativizar_censo <- function(bd){
  dicc <- diccionario_censo20 %>%
    dplyr::filter(!is.na(denominador),
                  denominador!="-")
  dicc <- dicc %>%
    filter(glue::glue("cen_{variable}") %in% names(bd),
           glue::glue("cen_{denominador}") %in% names(bd)
    )
  res <- purrr::map_dfc(.x=unique(dicc$denominador),
                        ~{
                          numerador <- dicc %>%
                            dplyr::filter(denominador==.x) %>%
                            pull(variable)
                          denominador=glue::glue("cen_{.x}")
                          bd <- bd %>%
                            select(glue::glue("cen_{numerador}"), denominador) %>%
                            mutate(across(everything(),
                                          ~.x/!!rlang::sym(denominador))) %>%
                            select(-denominador)})

  res <- bd %>%
    select(-names(res)) %>%
    bind_cols(res)


  return(res)

}

calcular_irs <- function(bd, electoral, nivel, c_principal){
  base <- bd %>%
    semi_join(electoral, nivel) |>
    transmute(!!rlang::sym(nivel),
              i_analf = p15ym_an/p_15ymas,
              i_asistesc = (p6a11_noa + p12a14noa) / (p_6a11+ p_12a14),
              i_edbasinc = (p15ym_se + p15pri_in + p15pri_co + p15sec_in) / p_15ymas,
              i_sdsalud = psinder / pobtot,
              i_ptierra =  vph_pisoti / vivparh_cv,
              i_nosan = 1-((vph_excsa + vph_letr) / vivparh_cv),
              i_noagua = vph_aguafv / vivparh_cv,
              i_nodren = vph_nodren / vivparh_cv,
              i_noelec = vph_s_elec / vivparh_cv,
              i_nolav = 1-( vph_lavad / vivparh_cv),
              i_noref = 1-( vph_refri / vivparh_cv)
    ) |>
    filter_all(all_vars(!is.na(.)))

  cp <- prcomp(select(base, -all_of(nivel)), scale.=T )

  pred <- predict(cp, newdata = base)
  pred <- as.data.frame(pred)
  irs <- (pred$PC1 - mean(pred$PC1)) / sd(pred$PC1)

  base <- bind_cols(base |> select(all_of(nivel)), rezago = irs) |>
    mutate(y = 100/(max(rezago)-min(rezago))*
             (rezago - min(rezago)),
           intervalo = case_when(y <= 10 ~ 1,
                                 y > 10 & y <= 20 ~ 2,
                                 y > 20 & y <= 30 ~ 3,
                                 y > 30 & y <= 40 ~ 4,
                                 y > 40 & y <= 50 ~ 5,
                                 y > 50 & y <= 60 ~ 6,
                                 y > 60 & y <= 70 ~ 7,
                                 y > 70 & y <= 80 ~ 8,
                                 y > 80 & y <= 90 ~ 9,
                                 y > 90 ~ 10 ),
           quant_rezago = case_when(intervalo <= 1 ~ "Muy bajo",
                               intervalo > 1 & intervalo <= 3 ~ "Bajo",
                               intervalo > 3 & intervalo <= 4 ~ "Medio",
                               intervalo > 4 & intervalo <= 6 ~ "Alto",
                               intervalo > 6 ~ "Muy alto"),
           quant_rezago = factor(quant_rezago, levels = c("Muy bajo", "Bajo", "Medio", "Alto", "Muy alto"))) |>
    select(-y,-intervalo) |>
    obtener_color(c_principal = c_principal, "rezago")

  return(base)

}

