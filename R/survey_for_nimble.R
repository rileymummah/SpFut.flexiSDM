survey_for_nimble <- function(data,
                              cov.names,
                              rename = "",
                              type = "count") {


  if ("count" %in% colnames(data) == F) {stop("There must be exactly one column named 'count' in survey")}

  if (length(grep("grid.id", colnames(data))) != 1) {stop("There must be exactly one column named 'grid.id' in survey")}
  if (length(grep("site.id", colnames(data))) != 1) {stop("There must be exactly one column named 'site.id' in survey")}

  surveydata <- select(data, !any_of(cov.names))
  covariates <- select(data, conus.grid.id, site.id, survey.id, any_of(cov.names))

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
