#' Load covariates
#'
#' @param sp.code (character) 4-letter species code
#' @param region (list) output from make_region()
#'
#' @returns (data.frame) dataframe holding covariate values for species grid
#' @export
#'
#' @examples
#'
#'
#'


load_covar <- function(sp.code,
                       region) {



  # use grid.ids from region$sp.grid
  locs <- region$sp.grid %>%
            dplyr::select(conus.grid.id) %>%
            sf::st_drop_geometry()


  # load gridded covariates
  load("data/USA/grid-covar.rdata")

  # get covariates for grid.ids in region
  covar <- dplyr::inner_join(locs, conus.covar.grid, by = "conus.grid.id")



  # now add n.iNat obs
  if (file.exists(paste0("data/species/", sp.code, "/iNat-supp.csv"))) {
    cat("loading and aggregating iNat records of supplemental species\n")

    inat <- read.csv(paste0("data/species/", sp.code, "/iNat-supp.csv"))

    inat <- inat %>%
              sf::st_as_sf(crs = 4326,
                           coords = c("lon", "lat")) %>%
              sf::st_transform(sf::st_crs(region$sp.grid)) %>%
              sf::st_join(region$sp.grid, join = st_within) %>%
              dplyr::filter(is.na(conus.grid.id) == F) %>%
              dplyr::group_by(conus.grid.id) %>%
              dplyr::summarize(n.inat = dplyr::n(), .groups = 'drop') %>%
              sf::st_drop_geometry()


    covar <- dplyr::left_join(covar, inat, by = c("conus.grid.id"))
    covar$n.inat[which(is.na(covar$n.inat))] <- 0
  } else {
    cat("Warning: iNat records of supplemental species have not been downloaded yet\n")
  }

  # add centroid lat and lon of each grid cell to covar
  centroid <- sf::st_centroid(region$sp.grid) %>%
                sf::st_coordinates() %>%
                as.data.frame() %>%
    dplyr::mutate(conus.grid.id = region$sp.grid$conus.grid.id)

  colnames(centroid)[1:2] <- c("lon", "lat")
  covar <- dplyr::left_join(covar, centroid, by = "conus.grid.id")

  # return
  return(covar)

}
