#' nimbleParallel
#'
#' @description Run NIMBLE chains in parallel using a local cluster
#'
#' @param cores (numeric) The number of cores (and, in turn, chains) to run in parallel. Default = 3.
#' @param code (nimbleCode)
#' @param constants (nimbleConstants)
#' @param data (nimbleData)
#' @param inits (function)
#' @param param (list?)
#' @param iter (numeric) The number of iterations to run in each chain
#' @param burnin (numeric) The number of iterations to discard as burnin
#' @param thin (numeric) The number of iterations to thin by
#'
#' @returns A list of mcmc objects. Each element of the list is one chain of samples saved as a coda::mcmc object.
#' @export
#'
#' @importFrom parallel makeCluster stopCluster parLapply
#'
#' @examples


nimbleParallel <- function(cores = 3, code, constants, data,
                           inits, param, iter, burnin, thin) {

  this_cluster <- parallel::makeCluster(cores)

  tmp <- lapply(1:cores, function(x) list(seed = x,
                                          inits = inits(x)))

  # run in parallel
  cat("Jack be NIMBLE... \n")
  cat("Jack be quick... \n")

  chain_output <- parallel::parLapply(cl = this_cluster,
                                      X = tmp,
                                      fun = run_nimbleMCMC,
                                      code = code,
                                      constants = constants,
                                      data = data,
                                      param = param,
                                      iter = iter,
                                      burnin = burnin,
                                      thin = thin)

  cat("Jack jumped over the candlestick! \n")
  cat("Ha ha aren't we funny?! No, really, your model is done fitting.")

  parallel::stopCluster(this_cluster)

  return(chain_output)
}
