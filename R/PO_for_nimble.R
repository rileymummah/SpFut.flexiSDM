#' Format PO data for nimble
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




PO_for_nimble <- function(POdata,      ## dataframe containing PO data. Must include column names "grid.id" and "count"
                          covariates,  ## dataframe containing covariates for PO effort. First column must be "grid.id", then column for intercept, followed by one column for each covariate
                          rename = "") {


  if (length(grep("count", colnames(POdata))) != 1) {stop("There must be exactly one column named 'count' in POdata")}
  if (length(grep("conus.grid.id", colnames(POdata))) != 1) {stop("There must be exactly one column named 'conus.grid.id' in POdata")}
  if (length(grep("conus.grid.id", colnames(covariates))) != 1) {stop("There must be exactly one column named 'conus.grid.id' in covariates")}

  if (nrow(POdata) != nrow(covariates)) {warning("POdata and covariates contain different number of rows. Unmatched grid.ids will be dropped")}
  
  # check that conus.grid.ids are in the same order
  if ("FALSE" %in% names(table(POdata$conus.grid.id == covariates$conus.grid.id))) {
    stop("STOP! conus.grid.id in POdata and covariates need to be in the same order")
  }

  cov.names <- select(covariates, !conus.grid.id) %>%
    colnames()

  # set W covariates
  Xw <- covariates %>%
    select(!conus.grid.id)
    #select(all_of(cov.names))

  W <- POdata %>%
    pull(count)

  Wcells <- POdata %>%
    pull(conus.grid.id)

  nCovW <- ncol(Xw)

  # return
  data <- list(Xw = Xw,
               W = W)
  constants <- list(nCovW = nCovW,
                    Wcells = Wcells,
                    nW = length(Wcells))

  # rename with number
  names(data) <- paste0(names(data), rename)
  names(constants) <- paste0(names(constants), rename)


  # return
  dat <- list(data = data,
              constants = constants)
  return(dat)

}
