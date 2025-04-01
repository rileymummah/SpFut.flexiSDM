#' Title
#'
#' @param samples (list of mcmc objects) output from fitting a NIMBLE model
#' @param data (NIMBLE data) data created and formatted for NIMBLE
#' @param project (numeric) the number of projections
#' @param coarse.grid (logical) whether or not a coarse grid was used to fit the spatial model
#' @param spatRegion (list) object created from make_spatkey() and used to fit the spatial model
#' @param pathToProj (character) path to R script where projections are set up
#'
#' @returns A dataframe of the original MCMC samples with estimated psi and lambda, psi, and XB for each projection (if projections are specified)
#' @export
#'
#' @importFrom VGAM clogloglink
#' @importFrom tidyselect all_of
#' @importFrom dplyr select bind_cols
#' @importFrom coda as.mcmc
#'
#' @examples

get_derived <- function(samples,
                        data,
                        project,
                        coarse.grid,
                        spatRegion,
                        pathToProj = NULL) {

  # Pull out lambda
  keep <- grep("lambda0", colnames(samples))

  as.data.frame(coda::as.mcmc(samples)) %>%
    dplyr::select(tidyselect::all_of(keep)) -> lambda0


  # Convert lambda to psi for all samples
  psi0 <- lapply(lambda0, function(x) VGAM::clogloglink(log(x), inverse = T)) %>%
            data.frame()
  names(psi0) <- gsub('lambda0.', 'psi0[', names(psi0))
  names(psi0) <- gsub('[.]', ']', names(psi0))


  # Projections

  # Load projections
  if (project > 0) {
    source(pathToProj)


    # Pull spat from output file
    keep <- grep("spat", colnames(samples))

    as.data.frame(coda::as.mcmc(samples)) %>%
      dplyr::select(tidyselect::all_of(keep)) %>%
      as.matrix() -> spat

    # Realign the spatial grid to the species grid if coarse.grid
    # This loop could probably be sped up if it was vectorized.
    if (coarse.grid == T) {
      cells <- spatRegion$spatkey$spat.grid.id

      tmp <- c()
      suppressMessages(for (i in 1:length(cells)) {
        index <- which(colnames(spat) == paste0('spat[', cells[i], ']'))
        tmp <- dplyr::bind_cols(tmp, spat[, index])
      })

      colnames(tmp) <- paste0('spat[', 1:length(cells), ']')
      spat <- tmp
      rm(tmp)

    }

    # Pull beta from output file
    keep <- grep('^B', colnames(samples))

    as.data.frame(coda::as.mcmc(samples)) %>%
      dplyr::select(tidyselect::all_of(keep)) %>%
      as.matrix() -> beta

    # Get projections
    proj <- lapply(1:project, get_projections, data = data,
                   beta = beta, spat = spat, lambda0 = lambda0)

    proj <- do.call(cbind, proj)

    derived <- dplyr::bind_cols(samples, psi0, proj)

  } else {
    derived <- dplyr::bind_cols(samples, psi0)
  }

  return(derived)
}
