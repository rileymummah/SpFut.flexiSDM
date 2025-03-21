#' Set up z data for nimble
#'
#' @description
#'
#' @param 
#'
#' @returns A list containing ranges as sf objects
#' @export
#'
#' @examples
#' \dontrun{
#' range <- get_range('ANMI')
#' }



z_for_nimble <- function(covariates) {

  if (is.na(sum(covariates[,3:ncol(covariates)]))) {stop("Remove NA's from covariates")}
  if (length(grep("conus.grid.id", colnames(covariates))) != 1) {stop("There must be exactly one column named 'conus.grid.id' in covariates")}

  Xz <- select(covariates, !conus.grid.id)
  nCovZ <- ncol(Xz)
  nCell <- nrow(covariates)

  data <- list(Xz = Xz)
  constants <- list(nCovZ = nCovZ, nCell = nCell)

  dat <- list(data = data, constants = constants)


  return(dat)

}
