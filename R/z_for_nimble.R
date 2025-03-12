# z_for_nimble <- function(nCell,
#                          covariates)
# {
#
#   if (is.na(sum(covariates[,3:ncol(covariates)]))) {stop("Remove NA's from covariates")}
#
#   if (length(grep("conus.grid.id", colnames(covariates))) != 1) {stop("There must be exactly one column named 'conus.grid.id' in covariates")}
#
#   # covariates <- covariates %>%
#   #   arrange(as.numeric(conus.grid.id))
#
#   covnames <- colnames(covariates)[2:ncol(covariates)]
#
#   Xz <- covariates[,which(colnames(covariates) %in% covnames)]
#
#   nCovZ <- ncol(Xz)
#   nCell <- nCell
#   #train.i <- covariates$conus.grid.id
#
#   data <- list(Xz = Xz)
#   #constants <- list(nCovZ = nCovZ, nCell = nCell, train.i = train.i)
#   constants <- list(nCovZ = nCovZ, nCell = nCell)
#
#   dat <- list(data = data, constants = constants)
#
#
#   return(dat)
#
# }
