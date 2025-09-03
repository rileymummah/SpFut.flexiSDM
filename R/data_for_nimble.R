#' Format species data and covariate data for nimble
#'
#' @param sp.data (list) output from sppdata_for_nimble()
#' @param covar (data.frame) dataframe containing process covariates
#' @param covs.z (character vector) vector of column names from covar to use as process covariates
#' @param sp.auto (logical) whether to use spatial autocorrelation in the model (T) or not (F); defaults to T
#' @param coarse.grid (logical) whether to use a coarse grid (T) or not (F); defaults to T
#' @param region (list) output from make_region()
#' @param process.intercept (logical) whether to include an intercept in the process model or not; defaults to T
#' @param gridkey (data.frame) contains conus.grid.ids and associated grid.ids to use for indexing in nimble
#' @param spatRegion (list) output from make_spatKey(); only required if coarse.grid == T
#'
#' @returns (list) list of data and constants fully formatted for nimble
#' @export
#'
#' @importFrom dplyr inner_join
#' @importFrom spdep poly2nb nb2WB
#'
#' @examples


data_for_nimble <- function(sp.data,
                            covar,
                            covs.z,
                            sp.auto = T,
                            coarse.grid = T,
                            region,
                            process.intercept = T,
                            gridkey,
                            spatRegion) {

  # get number of datasets
  nD <- length(sp.data)

  # setup Z
  nCell <- nrow(covar)

  if (process.intercept == T) {
    covariates <- cbind(conus.grid.id = covar$conus.grid.id, Z1 = rep(1, nrow(covar)),
                        covar[,covs.z])
  } else if (process.intercept == F) {
    covariates <- cbind(conus.grid.id = covar$conus.grid.id,
                        covar[,covs.z])
  }

  covariates <- covariates[order(match(covariates$conus.grid.id, gridkey$conus.grid.id)),]

  Z <- z_for_nimble(covariates)


  # combine
  data <- sp.data

  names(data) <- NULL
  input <- c(Z, unlist(data, recursive = F, use.names = T))

  data <- input[names(input) == "data"]
  names(data) <- NULL
  data <- unlist(data, recursive  = F, use.names = T)

  constants <- input[names(input) == "constants"]
  names(constants) <- NULL
  constants <- unlist(constants, recursive  = F, use.names = T)
  constants$nD <- nD


  if (coarse.grid == T) {
    spatkey <- spatRegion$spatkey
    spat.grid <- spatRegion$spat.grid

  } else {
    spat.grid <- region$sp.grid
  }



  if (sp.auto == T) {
    NB <- spdep::poly2nb(spat.grid)
    NBinfo <- spdep::nb2WB(NB)

    constants$L <- length(NBinfo$adj)
    constants$adj <- NBinfo$adj
    constants$weights <- NBinfo$weights
    constants$num <- NBinfo$num
  }



  if (coarse.grid == T) {
    # These are the spatial cells that correspond to each grid cell i
    # When indexed by i in the model, they should correspond correctly
    constants$spatCells <- spatkey$spat.grid.id
    constants$nSpatCell <- length(unique(spatkey$spat.grid.id))
  }


  # for each dataset, change conus.grid.id to sp.grid.id
  cells <- grep("cells", names(constants))
  for (e in cells) {
    tmp <- data.frame(conus.grid.id = as.character(constants[[e]])) %>%
            dplyr::inner_join(gridkey, by = "conus.grid.id")
    constants[[names(constants)[e]]] <- tmp$grid.id
  }


  return(list(data = data,
              constants = constants))


}
