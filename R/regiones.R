#' Function Name: d0_d1
#'
#' This function performs spatial and attribute clustering analysis using either Gower's distance or Euclidean distance. It takes a dataset, a shapefile, a number 'K' for clustering, and a type of distance metric (defaulting to "gower"). It returns a list containing distance matrices and clustering results along with the original shapefile.
#'
#' @param bd Data frame containing the data to be clustered. The function expects this data frame to contain columns starting with "ele_", which are transformed using a logarithm.
#' @param shp A shapefile (spatial data) used for calculating spatial distances.
#' @param K An integer representing the number of clusters to be used in the ClustGeo algorithm.
#' @param tipo A string specifying the type of distance metric to be used. "gower" for Gower's distance (default) and any other value for Euclidean distance.
#'
#' @return Returns a list containing the following elements:
#'   - d0: A distance matrix (either Gower's or Euclidean) computed from the 'bd' data.
#'   - d1: A distance matrix computed from the 'shp' shapefile.
#'   - cr: Clustering results obtained from the ClustGeo::choicealpha function.
#'   - shp: The original shapefile passed to the function.
#'
#'
#' @export
d0_d1 <- function(bd, shp, K, tipo = "gower"){
  ele_log <- bd |>
    mutate(across(starts_with("ele_"), ~log(.x)))

  if(tipo == "gower"){
    gower <- cluster::daisy(ele_log |>
                              # filter(municipio != "E") |>
                              select(starts_with("ele_")), metric = "gower")
    d0 <- gower |>
      as.matrix() |>
      as.dist()
  } else{
    d0 <- ele_log |>
      # filter(municipio != "E") |>
      select(starts_with("ele_")) |>
      dist()
  }

  d1 <- shp |>
    sf::st_distance() |>
    as.dist()

  # dim(d0)
  # dim(d1)

  range.alpha <- seq(0,1, length.out = nrow(d0))

  cr <- ClustGeo::choicealpha(D0 = d0,
                              D1 =  d1,
                              range.alpha =  range.alpha,
                              K, graph = FALSE)
  # print(cr$Q) # proportion of explained inertia
  # print(cr)

  return(lst(d0, d1, cr, shp))
}

#' Dibuja el mapa de una región
#'
#' @param d0_d1 base obtenida de la función `d0_d1`
#' @param alpha transparencia del leaflet
#'
#' @return un leaflet
#' @export
mapa_region <- function(d0_d1, alpha){
  clusters <- ClustGeo::hclustgeo(d0_d1$d0,d0_d1$d1, alpha = alpha)

  clusterCut <- cutree(clusters, d0_d1$cr$K)

  reg <- d0_d1$shp |>
    mutate(grupo = clusterCut)

  colores <- RColorBrewer::brewer.pal(n = min(d0_d1$cr$K,12), name = "Paired") |>
    append(
      RColorBrewer::brewer.pal(n = min(d0_d1$cr$K,9), name = "Set1")
    ) |>
    append(
      RColorBrewer::brewer.pal(n = min(d0_d1$cr$K,8), name = "Set2")
    ) |>
    append(
      RColorBrewer::brewer.pal(n = min(d0_d1$cr$K,12), name = "Set3")
    )

  pal <- colorFactor(colores[seq_len(d0_d1$cr$K)],
                     domain = clusterCut)

  reg |>
    leaflet() |>
    addProviderTiles("CartoDB.Positron") |>
    addPolygons(fillColor = ~pal(grupo),
                weight = 1, fillOpacity = 1#,
                # label = ~glue::glue("{nombre_municipio}-{grupo}")
    ) |>
    addLegend(pal = pal, values = ~grupo)

  return(reg)

}
