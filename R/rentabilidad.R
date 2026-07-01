# ============================================================
# Modelo de Rentabilidad Electoral
# Clasifica secciones/municipios en cuadrantes y calcula el
# Índice de Rentabilidad (IR) a partir de tres elecciones.
# ============================================================

# ---- Helpers internos ----------------------------------------

columns_match <- function(bd, patterns) {
  matches <- sapply(patterns, function(p) grepl(p, names(bd)))
  names(bd)[rowSums(matches) > 0]
}

#' @keywords internal
resolver_nivel_geo <- function(nivel_geo) {
  switch(nivel_geo,
    seccion   = "seccion",
    municipio = "municipio_24",
    dl        = "distritol_24",
    df        = "distritof_24",
    stop("nivel_geo debe ser uno de: 'seccion', 'municipio', 'dl', 'df'")
  )
}

calc_slope <- function(x, years) {
  if (sum(!is.na(x)) < 2) return(NA_real_)
  df <- tibble::tibble(y = x, t = years)
  stats::coef(stats::lm(y ~ t, data = df))[["t"]]
}

calc_winstreak <- function(g) {
  if (all(is.na(g))) return(NA_integer_)
  r <- rle(g)
  max(r$lengths[r$values == 1], default = 0L)
}

bd_variables_necesarias <- function(bd, eleccion, estrato,
                                    coalicion_morena,
                                    coalicion_oposicion,
                                    coalicion_oposicion2 = NULL) {
  cols_m   <- columns_match(bd, paste0("ele_", coalicion_morena,   "_", eleccion))
  cols_op1 <- columns_match(bd, paste0("ele_", coalicion_oposicion, "_", eleccion))

  col_m   <- paste0("ele_coalicion_morena_",     eleccion)
  col_op1 <- paste0("ele_coalicion_oposicion_",  eleccion)

  bd_fin <- bd |>
    dplyr::select(dplyr::contains(eleccion) & dplyr::starts_with("ele_"),
                  dplyr::all_of(estrato)) |>
    dplyr::mutate(
      dplyr::across(dplyr::contains(eleccion) & dplyr::starts_with("ele_"),
                    ~ tidyr::replace_na(.x, 0)),
      !!col_m   := rowSums(dplyr::across(dplyr::all_of(cols_m))),
      !!col_op1 := rowSums(dplyr::across(dplyr::all_of(cols_op1)))
    )

  nuevas <- c(col_m, col_op1)
  # contains() sólo acepta un string; usamos matches() con regex para vectores
  a_eliminar <- unique(c(
    names(dplyr::select(bd_fin, dplyr::matches(paste(coalicion_morena,    collapse = "|")))),
    names(dplyr::select(bd_fin, dplyr::matches(paste(coalicion_oposicion, collapse = "|"))))
  ))

  if (!is.null(coalicion_oposicion2)) {
    cols_op2 <- columns_match(bd, paste0("ele_", coalicion_oposicion2, "_", eleccion))
    col_op2  <- paste0("ele_coalicion_oposicion2_", eleccion)

    bd_fin <- bd_fin |>
      dplyr::mutate(!!col_op2 := rowSums(dplyr::across(dplyr::all_of(cols_op2))))

    nuevas     <- c(nuevas, col_op2)
    a_eliminar <- unique(c(a_eliminar,
                           names(dplyr::select(bd_fin, dplyr::matches(paste(coalicion_oposicion2, collapse = "|"))))))
  }

  a_eliminar <- setdiff(a_eliminar, nuevas)
  aux_vars   <- paste0(c("ele_validos_", "ele_nulos_", "ele_noreg_", "ele_total_"), eleccion)
  cols_ind   <- names(bd_fin)[grepl("ind", names(bd_fin))]
  drop       <- intersect(c(aux_vars, cols_ind, a_eliminar), names(bd_fin))

  dplyr::select(bd_fin, -dplyr::all_of(drop))
}

agregar_por_geografia <- function(obj_eleccion, nivel_geo, eleccion, key_geo = NULL) {
  if (is.null(key_geo)) key_geo <- resolver_nivel_geo(nivel_geo)
  bd_sec     <- as.data.frame(obj_eleccion$info$shp$seccion)
  patron     <- paste0("^ele_.*_", eleccion, "_\\d{2}$")

  bd_sec |>
    dplyr::summarise(
      dplyr::across(dplyr::matches(patron), ~ sum(.x, na.rm = TRUE)),
      .by = dplyr::all_of(key_geo)
    ) |>
    dplyr::rename_with(~ gsub("participacion", "total", .x),
                       dplyr::matches("participacion"))
}

calcular_nominal_por_nivel <- function(obj_eleccion, eleccion, nivel_geo, key_geo = NULL) {
  id_geo  <- if (!is.null(key_geo)) key_geo else resolver_nivel_geo(nivel_geo)
  shp_sec <- obj_eleccion$info$shp$seccion |>
    as.data.frame() |>
    dplyr::select(seccion, dplyr::any_of(id_geo))

  patron      <- paste0("^ele_nominal_", eleccion, "_\\d{2}$")
  bd_nom      <- obj_eleccion$info$bd |>
    as.data.frame() |>
    dplyr::left_join(shp_sec, by = "seccion")
  cols_nominal <- names(bd_nom)[stringr::str_detect(names(bd_nom), patron)]

  if (length(cols_nominal) == 0)
    stop("No se encontraron columnas ele_nominal para la eleccion: ", eleccion)

  bd_nom |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_geo))) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(cols_nominal),
                                   ~ sum(.x, na.rm = TRUE)),
                     .groups = "drop")
}

coaliciones_por_anio <- function(bd, eleccion, anio2,
                                 coal_morena, coal_opo1,
                                 coal_opo2 = NULL, estrato) {
  bd_variables_necesarias(
    bd                 = bd,
    eleccion           = paste0(eleccion, "_", anio2),
    estrato            = estrato,
    coalicion_morena   = coal_morena,
    coalicion_oposicion = coal_opo1,
    coalicion_oposicion2 = coal_opo2
  )
}

construir_panel_largo <- function(bd_geo, bd_coal, eleccion, anios, key_geo) {
  lista_anual <- purrr::map(anios, function(anio) {
    a2         <- stringr::str_sub(as.character(anio), -2)
    col_total  <- glue::glue("ele_total_{eleccion}_{a2}")
    col_nom    <- glue::glue("ele_nominal_{eleccion}_{a2}")
    col_m      <- glue::glue("ele_coalicion_morena_{eleccion}_{a2}")
    col_op1    <- glue::glue("ele_coalicion_oposicion_{eleccion}_{a2}")
    col_op2    <- glue::glue("ele_coalicion_oposicion2_{eleccion}_{a2}")

    base <- bd_geo |>
      dplyr::select(dplyr::all_of(key_geo),
                    dplyr::all_of(col_total),
                    dplyr::all_of(col_nom)) |>
      dplyr::left_join(
        bd_coal |> dplyr::select(dplyr::all_of(key_geo),
                                 dplyr::any_of(c(col_m, col_op1, col_op2))),
        by = key_geo
      ) |>
      dplyr::mutate(dplyr::across(dplyr::contains("ele_"), as.numeric))

    base |>
      dplyr::mutate(
        votos_validos        = dplyr::coalesce(.data[[col_total]], 0),
        lista_nominal        = dplyr::coalesce(.data[[col_nom]], 0),
        votos_morena_ticket  = dplyr::coalesce(.data[[col_m]], 0),
        votos_oponente_ticket = pmax(
          dplyr::coalesce(.data[[col_op1]], 0),
          dplyr::coalesce(
            if (col_op2 %in% names(base)) .data[[col_op2]] else 0, 0),
          na.rm = TRUE
        ),
        # The denominator is ele_participacion (total ballots cast). A
        # small share of sections carry an understated/zero participacion
        # for a given year while their per-party votes are complete, which
        # would push pct above 1. Those sections have a bad turnout figure,
        # not a coalition effect: flag them as NA instead of rescaling.
        denom      = votos_validos,
        participacion_valida = denom > 0 &
                               (votos_morena_ticket + votos_oponente_ticket) <= denom,
        pct_morena = dplyr::if_else(participacion_valida,
                                    votos_morena_ticket / denom, NA_real_),
        pct_opo    = dplyr::if_else(participacion_valida,
                                    votos_oponente_ticket / denom, NA_real_),
        margen     = pct_morena - pct_opo,
        ganado     = as.integer(margen > 0),
        turnout    = dplyr::if_else(participacion_valida & lista_nominal > 0,
                                    votos_validos / lista_nominal, NA_real_),
        anio       = anio
      )
  })

  dplyr::bind_rows(lista_anual) |>
    dplyr::arrange(.data[[key_geo]], anio)
}

resumen_desempeno <- function(panel_largo, key_geo, anios) {
  if (length(anios) != 3)
    stop("El modelo de rentabilidad requiere exactamente 3 elecciones.")

  a1 <- anios[1]; a2 <- anios[2]; a3 <- anios[3]

  panel_largo |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key_geo))) |>
    dplyr::summarise(
      pct_m_1  = dplyr::first(pct_morena[anio == a1]),
      pct_m_2  = dplyr::first(pct_morena[anio == a2]),
      pct_m_3  = dplyr::first(pct_morena[anio == a3]),
      margen_1 = dplyr::first(margen[anio == a1]),
      margen_2 = dplyr::first(margen[anio == a2]),
      margen_3 = dplyr::first(margen[anio == a3]),
      turnout_3 = dplyr::first(turnout[anio == a3]),
      votos_3   = dplyr::first(votos_validos[anio == a3]),
      delta_12  = pct_m_2 - pct_m_1,
      delta_23  = pct_m_3 - pct_m_2,
      delta_13  = pct_m_3 - pct_m_1,
      slope_pct = calc_slope(c(pct_m_1, pct_m_2, pct_m_3), years = anios),
      var_morena = stats::var(c(pct_m_1, pct_m_2, pct_m_3), na.rm = TRUE),
      win_streak = calc_winstreak(as.integer(c(margen_1, margen_2, margen_3) > 0)),
      fortaleza  = mean(c(margen_1, margen_2, margen_3), na.rm = TRUE),
      .groups = "drop"
    )
}

# ---- Funciones exportadas ------------------------------------

#' Extrae los años disponibles para un tipo de elección
#'
#' @param obj_eleccion Objeto `Tablero` o `Electoral` con `$info$elecciones_agregadas`.
#' @param eleccion Tipo de elección: `"pm"`, `"dl"`, `"df"`, `"pr"`, `"sen"`, `"gb"`.
#' @return Vector numérico de años ordenado de menor a mayor.
#' @export
#' @import stringr
extraer_anios_eleccion <- function(obj_eleccion, eleccion) {
  patron <- paste0("^", eleccion, "_\\d{2}$")
  elecs  <- stringr::str_subset(obj_eleccion$info$elecciones_agregadas, patron)

  if (length(elecs) == 0)
    stop("No se encontraron elecciones del tipo '", eleccion,
         "' en obj_eleccion$info$elecciones_agregadas")

  anios2 <- as.integer(stringr::str_extract(elecs, "\\d{2}$"))
  sort(ifelse(anios2 < 50, 2000 + anios2, 1900 + anios2))
}


#' Clasifica secciones/municipios y calcula el Índice de Rentabilidad
#'
#' @description
#' A partir de un resumen de desempeño (tres elecciones), asigna cuadrantes
#' Fuerza × Momentum, clases Afines (A1–A3) y Flip (B1–B3), y calcula
#' los scores SA, SF e IR.
#'
#' @param df Data frame con las columnas producidas por el resumen interno
#'   (`margen_3`, `fortaleza`, `slope_pct`, `delta_23`, `delta_13`,
#'   `turnout_3`, `votos_3`, `win_streak`, `var_morena`).
#' @param params Lista de parámetros del modelo. Ver [params_rentabilidad_default()].
#' @return El mismo data frame con columnas adicionales:
#'   `momentum`, `cuadrante`, `clase_afines`, `clase_flip`, `SA`, `SF`, `IR`.
#' @export
#' @import dplyr
clasificar_rentabilidad <- function(df, params) {
  rank_up   <- function(x) rank(x,  na.last = "keep", ties.method = "average")
  rank_down <- function(x) rank(-x, na.last = "keep", ties.method = "average")

  df <- df |>
    dplyr::mutate(
      momentum    = dplyr::coalesce(slope_pct, delta_23),
      turnout_gap = turnout_3 - stats::median(turnout_3, na.rm = TRUE),
      tamano      = votos_3,
      fuerza_pos  = fortaleza > 0,
      momentum_pos = dplyr::case_when(
        is.na(momentum) ~ NA,
        TRUE            ~ momentum > params$momentum_min_alza
      )
    ) |>
    dplyr::mutate(
      cuadrante = dplyr::case_when(
        fuerza_pos  & !is.na(momentum_pos) &  momentum_pos ~ "Q1: Fuerte + Al alza",
        !fuerza_pos & !is.na(momentum_pos) &  momentum_pos ~ "Q2: Debil + Al alza",
        fuerza_pos  & !is.na(momentum_pos) & !momentum_pos ~ "Q3: Fuerte + A la baja",
        !fuerza_pos & !is.na(momentum_pos) & !momentum_pos ~ "Q4: Debil + A la baja",
        fuerza_pos  &  is.na(momentum_pos)                 ~ "Q1: Fuerte + Al alza",
        !fuerza_pos &  is.na(momentum_pos)                 ~ "Q4: Debil + A la baja"
      )
    ) |>
    dplyr::mutate(
      clase_afines = dplyr::case_when(
        margen_3 >= params$bastion_seguro_min                                        ~ "A1: Bastion seguro",
        margen_3 >= params$ventaja_comoda_min &
          (momentum >= params$momentum_min_alza | delta_23 >= params$delta_comoda_min) ~ "A2: Ventaja comoda",
        margen_3 >= params$retencion_sensible_min                                    ~ "A3: Retencion sensible",
        TRUE ~ NA_character_
      ),
      clase_flip = dplyr::case_when(
        margen_3 < 0 & margen_3 > params$flip_inmediato_maxloss &
          (momentum > params$momentum_min_alza |
             delta_23 >= params$delta_fuerte_min | turnout_gap < 0)             ~ "B1: Flip inmediato",
        margen_3 <= params$flip_inmediato_maxloss & margen_3 > params$flip_probable_maxloss &
          (delta_23 >= params$delta_comoda_min | momentum > params$momentum_min_alza) ~ "B2: Flip probable",
        margen_3 <= params$flip_probable_maxloss & margen_3 > params$flip_paquete_maxloss &
          (delta_13 >= params$delta_fuerte_min | momentum > params$momentum_min_alza) ~ "B3: Flip con paquete",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::mutate(
      r_margen = rank_up(margen_3),
      r_streak = rank_up(win_streak),
      r_varinv = rank_down(var_morena),
      r_tamano = rank_up(tamano),
      SA = params$w_SA_margen    * r_margen +
           params$w_SA_winstreak * r_streak +
           params$w_SA_varinv    * r_varinv +
           params$w_SA_tamano    * r_tamano,
      r_cercania = rank_up(-abs(margen_3)),
      r_momentum = rank_up(dplyr::coalesce(delta_23, momentum)),
      r_tgap     = rank_up(-turnout_gap),
      r_tamano2  = rank_up(tamano),
      SF = params$w_SF_cercania    * r_cercania +
           params$w_SF_momentum    * r_momentum +
           params$w_SF_turnout_gap * r_tgap     +
           params$w_SF_tamano      * r_tamano2,
      IR = params$peso_IR_SA * SA + params$peso_IR_SF * SF
    )

  df
}


#' Parámetros por defecto del modelo de rentabilidad
#'
#' @description
#' Devuelve la lista de parámetros con valores razonables para comenzar.
#' Modifica los que necesites y pásalos a [ejecutar_modelo_rentabilidad()].
#'
#' @return Lista con todos los umbrales y pesos del modelo.
#' @export
params_rentabilidad_default <- function() {
  list(
    bastion_seguro_min      = 0.10,
    ventaja_comoda_min      = 0.05,
    retencion_sensible_min  = 0.02,
    momentum_min_alza       = 0.00,
    delta_comoda_min        = 0.03,
    delta_fuerte_min        = 0.05,
    flip_inmediato_maxloss  = -0.05,
    flip_probable_maxloss   = -0.08,
    flip_paquete_maxloss    = -0.12,
    w_SA_margen             = 0.35,
    w_SA_winstreak          = 0.25,
    w_SA_varinv             = 0.20,
    w_SA_tamano             = 0.20,
    w_SF_cercania           = 0.30,
    w_SF_momentum           = 0.30,
    w_SF_turnout_gap        = 0.20,
    w_SF_tamano             = 0.20,
    peso_IR_SA              = 0.50,
    peso_IR_SF              = 0.50
  )
}


#' Ejecuta el modelo completo de rentabilidad electoral
#'
#' @description
#' A partir de un objeto `Tablero`/`Electoral`, una lista de coaliciones por año
#' y un conjunto de parámetros, construye el panel largo de secciones/municipios
#' y calcula el Índice de Rentabilidad (IR) con su clasificación completa.
#'
#' @param obj_eleccion Objeto `Tablero` o `Electoral`.
#' @param eleccion Tipo de elección: `"pm"`, `"dl"`, `"df"`, `"pr"`, `"sen"`, `"gb"`.
#' @param nivel_geo Nivel de agregación: `"seccion"` (default), `"municipio"`, `"dl"`, `"df"`.
#' @param coaliciones Lista nombrada por año (e.g. `"2018"`, `"2021"`, `"2024"`), donde
#'   cada elemento tiene `$morena`, `$opo1`, y opcionalmente `$opo2`.
#' @param params Lista de parámetros. Usa [params_rentabilidad_default()] como punto de partida.
#' @param anios Vector entero de exactamente 3 años a usar. Si es `NULL` (default), se
#'   detectan automáticamente y se toman los 3 más recientes disponibles en el objeto.
#' @return Lista con dos elementos:
#'   \describe{
#'     \item{`panel_largo`}{Panel seccional/municipal por año con porcentajes y márgenes.}
#'     \item{`resumen`}{Tabla con cuadrantes, clases A/B, scores SA/SF e IR.}
#'   }
#' @export
#' @import dplyr purrr stringr glue
#' @examples
#' \dontrun{
#' params <- params_rentabilidad_default()
#' coaliciones <- list(
#'   "2018" = list(morena = c("morena", "pt", "pes"), opo1 = c("pan", "prd")),
#'   "2021" = list(morena = c("morena"), opo1 = c("pan", "pri", "prd")),
#'   "2024" = list(morena = c("morena", "pt", "pvem"), opo1 = c("pan", "pri", "prd"))
#' )
#' resultado <- ejecutar_modelo_rentabilidad(mi_tablero, "pm", "seccion",
#'                                           coaliciones, params)
#' resultado$resumen
#' }
ejecutar_modelo_rentabilidad <- function(obj_eleccion,
                                         eleccion  = "pm",
                                         nivel_geo = "seccion",
                                         coaliciones,
                                         params,
                                         anios     = NULL) {
  key_geo <- resolver_nivel_geo(nivel_geo)

  # Auto-detect the geo column when the SHP uses a different year suffix
  if (nivel_geo != "seccion") {
    bd_check <- as.data.frame(obj_eleccion$info$shp$seccion)
    if (!key_geo %in% names(bd_check)) {
      base     <- gsub("_\\d{2}$", "", key_geo)
      detected <- grep(paste0("^", base, "_"), names(bd_check), value = TRUE)
      if (length(detected) > 0) key_geo <- detected[1]
    }
  }

  if (is.null(anios))
    anios <- tail(extraer_anios_eleccion(obj_eleccion, eleccion), 3)

  bd_geo <- agregar_por_geografia(obj_eleccion, nivel_geo, eleccion, key_geo = key_geo)

  nominal_geo <- calcular_nominal_por_nivel(obj_eleccion, eleccion, nivel_geo, key_geo = key_geo)
  bd_geo      <- dplyr::left_join(bd_geo, nominal_geo, by = key_geo)

  lista_coal <- purrr::map(anios, function(a) {
    a2   <- stringr::str_sub(as.character(a), -2)
    coal <- coaliciones[[as.character(a)]]
    coaliciones_por_anio(
      bd          = bd_geo,
      eleccion    = eleccion,
      anio2       = a2,
      coal_morena = coal$morena,
      coal_opo1   = coal$opo1,
      coal_opo2   = coal$opo2,
      estrato     = key_geo
    )
  })

  bd_coal    <- purrr::reduce(lista_coal, dplyr::left_join, by = key_geo)
  panel_largo <- construir_panel_largo(bd_geo, bd_coal, eleccion, anios, key_geo)
  resumen     <- resumen_desempeno(panel_largo, key_geo, anios)
  clasif      <- clasificar_rentabilidad(resumen, params)

  list(panel_largo = panel_largo, resumen = clasif)
}


# ============================================================
# Optimización de parámetros
# ============================================================

softmax <- function(x) {
  e <- exp(x - max(x))
  e / sum(e)
}

#' Convierte un vector libre (16 valores) en una lista de params válida
#'
#' @description
#' Aplica las transformaciones necesarias para garantizar que los umbrales
#' queden ordenados y los pesos sumen 1. Útil para inspeccionar qué params
#' corresponden a un vector dado durante la optimización.
#'
#' El vector tiene esta estructura:
#' `par[1:3]`  — log-diferencias para umbrales positivos (retencion, ventaja, bastion)
#' `par[4:6]`  — log-diferencias para umbrales negativos (flip inmediato, probable, paquete)
#' `par[7:10]` — pesos SA (softmax)
#' `par[11:14]`— pesos SF (softmax)
#' `par[15:16]`— mezcla IR (softmax)
#'
#' @param par Vector numérico de longitud 16 (escala no restringida).
#' @param escala_umbral Factor de escala para los umbrales. Default `0.02`.
#' @param params_fijos Lista de params que no se optimizan (momentum, deltas).
#' @return Lista de params compatible con [clasificar_rentabilidad()].
#' @export
vec_a_params <- function(par, escala_umbral = 0.02,
                          params_fijos = list(momentum_min_alza = 0,
                                              delta_comoda_min  = 0.03,
                                              delta_fuerte_min  = 0.05)) {
  d_pos <- cumsum(exp(par[1:3])) * escala_umbral
  d_neg <- cumsum(exp(par[4:6])) * escala_umbral

  w_SA <- softmax(par[7:10])
  w_SF <- softmax(par[11:14])
  w_IR <- softmax(par[15:16])

  c(
    list(
      retencion_sensible_min = d_pos[1],
      ventaja_comoda_min     = d_pos[2],
      bastion_seguro_min     = d_pos[3],
      flip_inmediato_maxloss = -d_neg[1],
      flip_probable_maxloss  = -d_neg[2],
      flip_paquete_maxloss   = -d_neg[3],
      w_SA_margen            = w_SA[1],
      w_SA_winstreak         = w_SA[2],
      w_SA_varinv            = w_SA[3],
      w_SA_tamano            = w_SA[4],
      w_SF_cercania          = w_SF[1],
      w_SF_momentum          = w_SF[2],
      w_SF_turnout_gap       = w_SF[3],
      w_SF_tamano            = w_SF[4],
      peso_IR_SA             = w_IR[1],
      peso_IR_SF             = w_IR[2]
    ),
    params_fijos
  )
}

#' Convierte una lista de params al vector libre para inicializar la optimización
#'
#' @param params Lista de params (e.g. de [params_rentabilidad_default()]).
#' @param escala_umbral Mismo valor usado en [vec_a_params()].
#' @return Vector numérico de longitud 16.
#' @export
params_a_vec <- function(params, escala_umbral = 0.02) {
  # Umbrales positivos: retencion < ventaja < bastion
  r <- params$retencion_sensible_min / escala_umbral
  v <- params$ventaja_comoda_min     / escala_umbral
  b <- params$bastion_seguro_min     / escala_umbral
  d_pos <- c(r, v - r, b - v)

  # Umbrales negativos: |flip_inmediato| < |flip_probable| < |flip_paquete|
  fi <- abs(params$flip_inmediato_maxloss) / escala_umbral
  fp <- abs(params$flip_probable_maxloss)  / escala_umbral
  fk <- abs(params$flip_paquete_maxloss)   / escala_umbral
  d_neg <- c(fi, fp - fi, fk - fp)

  c(
    log(pmax(d_pos, 1e-6)),
    log(pmax(d_neg, 1e-6)),
    log(c(params$w_SA_margen, params$w_SA_winstreak,
          params$w_SA_varinv, params$w_SA_tamano)),
    log(c(params$w_SF_cercania, params$w_SF_momentum,
          params$w_SF_turnout_gap, params$w_SF_tamano)),
    log(c(params$peso_IR_SA, params$peso_IR_SF))
  )
}

#' Función objetivo estructural para optimizar params
#'
#' @description
#' Penaliza clasificaciones que no cubren la fracción objetivo de votos en
#' clases Afines, que dejan demasiadas secciones sin clase, y recompensa
#' un IR con mayor dispersión (más discriminante).
#'
#' Puedes usarla como plantilla para construir tu propia función objetivo.
#'
#' @param par Vector libre de longitud 16 (ver [vec_a_params()]).
#' @param resumen_df Data frame producido por el paso de resumen interno
#'   (incluye `margen_3`, `votos_3`, etc.).
#' @param objetivo_cobertura_A Fracción objetivo de votos totales en clase Afín. Default `0.40`.
#' @param escala_umbral Pasado a [vec_a_params()].
#' @param params_fijos Pasado a [vec_a_params()].
#' @return Escalar a minimizar.
#' @export
objetivo_rentabilidad <- function(par, resumen_df,
                                   objetivo_cobertura_A = 0.40,
                                   escala_umbral = 0.02,
                                   params_fijos  = list(momentum_min_alza = 0,
                                                        delta_comoda_min  = 0.03,
                                                        delta_fuerte_min  = 0.05)) {
  params <- tryCatch(
    vec_a_params(par, escala_umbral, params_fijos),
    error = function(e) return(NULL)
  )
  if (is.null(params)) return(1e6)

  clasif <- tryCatch(
    clasificar_rentabilidad(resumen_df, params),
    error = function(e) return(NULL)
  )
  if (is.null(clasif)) return(1e6)

  votos_total <- sum(clasif$votos_3, na.rm = TRUE)
  if (votos_total == 0) return(1e6)

  pct_A      <- sum(clasif$votos_3[!is.na(clasif$clase_afines)], na.rm = TRUE) / votos_total
  pct_vacio  <- mean(is.na(clasif$clase_afines) & is.na(clasif$clase_flip), na.rm = TRUE)
  spread_IR  <- stats::sd(clasif$IR, na.rm = TRUE)
  if (is.na(spread_IR) || spread_IR == 0) return(1e6)

  (pct_A - objetivo_cobertura_A)^2 + 0.5 * pct_vacio - 0.01 * spread_IR
}

#' Optimiza los parámetros del modelo de rentabilidad
#'
#' @description
#' Busca la combinación de umbrales y pesos que minimiza la función objetivo
#' sobre el `resumen_df` dado. Usa `stats::optim()` con múltiples puntos de
#' inicio para reducir la dependencia de óptimos locales.
#'
#' @param resumen_df Data frame con el resumen de desempeño (salida del paso
#'   interno de [ejecutar_modelo_rentabilidad()], o construido manualmente).
#' @param objetivo_fn Función objetivo a minimizar. Debe aceptar `(par, resumen_df, ...)`.
#'   Por default usa [objetivo_rentabilidad()].
#' @param params_iniciales Lista de params para el punto de inicio principal.
#'   Default: [params_rentabilidad_default()].
#' @param n_starts Número de puntos de inicio aleatorios adicionales. Default `10`.
#' @param objetivo_cobertura_A Pasado a `objetivo_fn`. Default `0.40`.
#' @param escala_umbral Pasado a [vec_a_params()] y [params_a_vec()]. Default `0.02`.
#' @param params_fijos Params que no se optimizan (momentum, deltas). Default estándar.
#' @param seed Semilla para reproducibilidad de puntos aleatorios. Default `42`.
#' @param ... Argumentos adicionales pasados a `objetivo_fn`.
#' @return Lista con:
#'   \describe{
#'     \item{`params`}{Lista de params óptimos, compatible con [clasificar_rentabilidad()].}
#'     \item{`valor`}{Valor de la función objetivo en el óptimo.}
#'     \item{`convergencia`}{Código de convergencia de `optim()`.}
#'     \item{`todos_starts`}{Data frame con resultados de todos los puntos de inicio.}
#'   }
#' @export
#' @examples
#' \dontrun{
#' # resumen_df viene del paso intermedio del modelo
#' res <- ejecutar_modelo_rentabilidad(mi_tablero, "pm", "seccion",
#'                                     coaliciones, params_rentabilidad_default())
#' opt <- optimizar_params(res$resumen, objetivo_cobertura_A = 0.35)
#' opt$params   # params optimizados
#'
#' # Aplicar params optimizados
#' resumen_opt <- clasificar_rentabilidad(res$resumen, opt$params)
#' }
optimizar_params <- function(resumen_df,
                              objetivo_fn          = objetivo_rentabilidad,
                              params_iniciales     = params_rentabilidad_default(),
                              n_starts             = 10L,
                              objetivo_cobertura_A = 0.40,
                              escala_umbral        = 0.02,
                              params_fijos         = list(momentum_min_alza = 0,
                                                          delta_comoda_min  = 0.03,
                                                          delta_fuerte_min  = 0.05),
                              seed                 = 42L,
                              ...) {
  par0 <- params_a_vec(params_iniciales, escala_umbral)

  correr_optim <- function(par_inicio) {
    stats::optim(
      par    = par_inicio,
      fn     = objetivo_fn,
      resumen_df           = resumen_df,
      objetivo_cobertura_A = objetivo_cobertura_A,
      escala_umbral        = escala_umbral,
      params_fijos         = params_fijos,
      ...,
      method  = "Nelder-Mead",
      control = list(maxit = 5000, reltol = 1e-8)
    )
  }

  # Punto de inicio principal
  resultados <- list(correr_optim(par0))

  # Puntos aleatorios adicionales
  if (n_starts > 0) {
    set.seed(seed)
    starts_extra <- purrr::map(seq_len(n_starts), function(i) {
      correr_optim(par0 + stats::rnorm(length(par0), sd = 0.5))
    })
    resultados <- c(resultados, starts_extra)
  }

  valores <- vapply(resultados, `[[`, numeric(1), "value")
  mejor   <- resultados[[which.min(valores)]]

  params_opt <- vec_a_params(mejor$par, escala_umbral, params_fijos)

  list(
    params      = params_opt,
    valor       = mejor$value,
    convergencia = mejor$convergence,
    todos_starts = tibble::tibble(
      start = seq_along(valores),
      valor = valores,
      convergencia = vapply(resultados, `[[`, integer(1), "convergence")
    )
  )
}
