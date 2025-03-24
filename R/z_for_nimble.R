#' Set up process data for nimble
#'
#' @description Creates data and constants necessary to fit process covariates in nimble code
#'
#' @param covariates (data.frame) dataframe containing process covariates
#'
#' @returns A list containing data and constants for nimble code



z_for_nimble <- function(covariates) {

  if (is.na(sum(covariates[,3:ncol(covariates)]))) {stop("Remove NA's from covariates")}
  if (length(grep("conus.grid.id", colnames(covariates))) != 1) {stop("There must be exactly one column named 'conus.grid.id' in covariates")}

  Xz <- dplyr::select(covariates, !conus.grid.id)
  nCovZ <- ncol(Xz)
  nCell <- nrow(covariates)

  data <- list(Xz = Xz)
  constants <- list(nCovZ = nCovZ, nCell = nCell)

  dat <- list(data = data, constants = constants)


  return(dat)

}
