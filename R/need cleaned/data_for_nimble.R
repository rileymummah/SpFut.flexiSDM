#' Format species data and covariate data for nimble
#'
#' @description
#'
#' @param 
#'
#' @returns 
#' @export
#'
#' @examples
#' \dontrun{
#' range <- get_range('ANMI')
#' }


data_for_nimble <- function(sp.data,
                            covar,
                            covs.z,
                            sp.auto = T,
                            coarse.grid = T,
                            region,
                            area = F,
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
      inner_join(gridkey, by = "conus.grid.id")
    constants[[names(constants)[e]]] <- tmp$grid.id
  }

  # now add train.i
  #constants$train.i <- gridkey$grid.id[which(gridkey$group == "train")]

  return(list(data = data,
              constants = constants))


}
