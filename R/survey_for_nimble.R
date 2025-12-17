#' Format survey data for nimble
#'
#' @description Format survey data to fit into our general NIMBLE framework
#'
#' @param data (data.frame) dataframe containing DND or count data and detection covariates
#' @param cov.names (character vector) names of columns to be used as detection covariates
#' @param rename (character) suffix to add to data and constants object names; defaults to empty string
#' @param type (character) indicates type of survey: 'count' or 'DND'
#' @param keep.conus.grid.id (character vector) vector of conus.grid.ids to keep; default comes from gridkey
#'
#' @returns A list containing data and constants for nimble code
#'
#' @importFrom tidyselect any_of
#' @importFrom stats median
#' @importFrom dplyr select


survey_for_nimble <- function(data,
                              cov.names,
                              rename = "",
                              type = "count",
                              keep.conus.grid.id) {


  if ("count" %in% colnames(data) == F) {stop("There must be exactly one column named 'count' in survey")}

  if (length(grep("grid.id", colnames(data))) != 1) {stop("There must be exactly one column named 'grid.id' in survey")}
  if (length(grep("site.id", colnames(data))) != 1) {stop("There must be exactly one column named 'site.id' in survey")}


  data <- filter(data, .data$conus.grid.id %in% keep.conus.grid.id)

  surveydata <- select(data, !any_of(cov.names))
  covariates <- select(data, .data$conus.grid.id, .data$site.id, .data$survey.id, any_of(cov.names))

  Y <- data$count
  surveyCell <- data$conus.grid.id
  nY <- length(Y)
  site.id <- data$site.id
  nSites <- length(unique(site.id))

  Xy <- select(data, any_of(cov.names))
  nCovY <- ncol(Xy)

  # get nVisit
  nVisit <- median(table(data$site.id))
  minVisit <- min(table(data$site.id))

  data <- list(Xy = Xy,
               Y = Y)
  constants <- list(nCovY = nCovY,
                    nY = nY,
                    Ycells = surveyCell,
                    nVisitsY = nVisit,
                    minVisit = minVisit)

  names(data) <- paste0(names(data), rename)
  names(constants) <- paste0(names(constants), rename)


  dat <- list(data = data,
              constants = constants)



  # if DND, swap Ys for Vs
  if (type == "DND") {
    tmp <- names(dat$data)
    tmp1 <- gsub("Y", "V", tmp)
    tmp2 <- gsub("y", "v", tmp1)
    names(dat$data) <- tmp2

    tmp <- names(dat$constants)
    tmp1 <- gsub("Y", "V", tmp)
    tmp2 <- gsub("y", "v", tmp1)
    names(dat$constants) <- tmp2
  }



  return(dat)
}
