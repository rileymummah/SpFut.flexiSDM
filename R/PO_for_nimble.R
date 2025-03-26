#' Format PO data for nimble
#'
#' @description Creates data and constants necessary to fit PO data in nimble code
#'
#' @param POdata (data.frame) dataframe containing PO data
#' @param covariates (data.frame) dataframe containing covariates for PO effort
#' @param rename (character) character suffix to add to data and constants object names; defaults to empty string
#'
#' @returns A list containing data and constants for nimble code
#'
#' @importFrom dplyr pull select


PO_for_nimble <- function(POdata,
                          covariates,
                          rename = "") {


  if (length(grep("count", colnames(POdata))) != 1) {stop("There must be exactly one column named 'count' in POdata")}
  if (length(grep("conus.grid.id", colnames(POdata))) != 1) {stop("There must be exactly one column named 'conus.grid.id' in POdata")}
  if (length(grep("conus.grid.id", colnames(covariates))) != 1) {stop("There must be exactly one column named 'conus.grid.id' in covariates")}

  if (nrow(POdata) != nrow(covariates)) {warning("POdata and covariates contain different number of rows. Unmatched grid.ids will be dropped")}

  # check that conus.grid.ids are in the same order
  if ("FALSE" %in% names(table(POdata$conus.grid.id == covariates$conus.grid.id))) {
    stop("STOP! conus.grid.id in POdata and covariates need to be in the same order")
  }

  cov.names <- dplyr::select(covariates, !conus.grid.id) %>% colnames()

  # set W covariates
  Xw <- dplyr::select(covariates, !conus.grid.id)

  W <- dplyr::pull(POdata, count)

  Wcells <- dplyr::pull(POdata, conus.grid.id)

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
