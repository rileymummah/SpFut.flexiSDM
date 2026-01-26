#' Sets up initial values for nimble
#'
#' @description Creates initial values for nimble based on content of 'data' and 'constants'
#'
#' @param data (list) data formatted for nimble
#' @param constants (list) constants formatted for nimble
#' @param sp.auto (logical) whether model should have spatial model (T) or not (F)
#' @param min.visits.incl (numeric) minimum number of median visits per site (inclusive) to use occupancy or N-mixture model; default is 3
#' @param seed (numeric) set seed to use for random number generation
#'
#' @returns Initial values to be used in nimble code
#' @export
#'
#' @importFrom tidyselect everything
#' @importFrom stats rnorm rbeta
#'
#' @examples
#'\dontrun{
#'
#'inits <- function(x){nimble_inits(data,
#'                                  constants,
#'                                  sp.auto = T,
#'                                  seed = x)}
#'
#'}



nimble_inits <- function(data,
                         constants,
                         sp.auto,
                         seed = as.numeric(Sys.time())) {

  
  # For Version 1 of package, only use single-visit models
  # Therefore, the minimum number of visits to use a multi-visit model
  # should be Inf. The next version of the package will allow this value
  # to be input as an argument.
  min.visits.incl <- Inf
  
  
  set.seed(seed)

  # initialize betas and w (for alpha)
  dat <- list(B = rnorm(constants$nCovZ, 0, 0.01),
              w = rnorm(constants$nD, 0, 0.1))


  # initialize parameters for spatial model
  if (sp.auto == T) {
    dat[["tau"]] <- 1
    dat[["spat"]] <- rep(0, length(constants$num))

  }


  # initialize for each dataset
  for (d in 1:constants$nD) {

    if (paste0("Xy", d) %in% names(data)) { # If it's a count dataset...


      if (constants[[paste0("nCovY", d)]] == 0) {
        # p is fixed
        dat[[paste0("p", d)]] <- rbeta(1, 2, 2)

      } else if (constants[[paste0("nCovY", d)]] > 0) {
        dat[[paste0("C", d)]] <- rnorm(constants[[paste0("nCovY", d)]])
        dat[[paste0("Xy", d)]] <- matrix(nrow = nrow(data[[paste0("Xy", d)]]),
                                         ncol = ncol(data[[paste0("Xy", d)]]),
                                         0)
      }

      # initialize ND if N-mixture model
      if (constants[[paste0("nVisitsY", d)]] > min.visits.incl) {
        dat[[paste0("ND", d)]] <- rep(1, constants[[paste0("nY", d)]])
      }



    } else if (paste0("Xw", d) %in% names(data)) { # If it's a PO dataset....

      if (constants[[paste0("nCovW", d)]] == 0) {
        # p is fixed
        dat[[paste0("E", d)]] <- rbeta(1, 2, 2)

      } else if (constants[[paste0("nCovW", d)]] > 0) {
        dat[[paste0("A", d)]] <- rnorm(constants[[paste0("nCovW", d)]])
        dat[[paste0("Xw", d)]] <- matrix(nrow = nrow(data[[paste0("Xw", d)]]),
                                         ncol = ncol(data[[paste0("Xw", d)]]),
                                         0)
      }


    } else if (paste0("Xv", d) %in% names(data)) { # If it's a DND dataset

      if (constants[[paste0("nCovV", d)]] == 0) {
        # p is fixed
        dat[[paste0("d", d)]] <- rbeta(1, 2, 2)

      } else if (constants[[paste0("nCovV", d)]] > 0) {
        dat[[paste0("D", d)]] <- rnorm(constants[[paste0("nCovV", d)]])
        dat[[paste0("Xv", d)]] <- matrix(nrow = nrow(data[[paste0("Xv", d)]]),
                                         ncol = ncol(data[[paste0("Xv", d)]]),
                                         0)
      }

      # initialize ND if occupancy model
      if (constants[[paste0("nVisitsV", d)]] > min.visits.incl) {
        dat[[paste0("ND", d)]] <- rep(1, constants[[paste0("nV", d)]])
      }
    }

  } # end loop through datasets


  return(dat)

}
