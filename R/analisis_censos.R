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



