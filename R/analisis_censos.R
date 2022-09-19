#' Title
#'
#' @param bd base de datos electoral
#'
#' @return
#' @export
#'
#' @examples
relativizar_censo <- function(bd){
  bd <- diccionario_censo20 %>%
    filter(!is.na(denominador),
           denominador!="-")
  res <- purrr::map_dfc(.x=unique(bd$denominador),
                        ~{numerador <- bd %>%
                          filter(denominador==.x) %>%
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



