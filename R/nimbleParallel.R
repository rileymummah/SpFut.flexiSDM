#' Title
#'
#' @param HPC
#' @param code
#' @param constants
#' @param data
#' @param inits
#' @param param
#' @param iter
#' @param burnin
#' @param thin
#' @param number
#' @param sp.code
#'
#' @returns
#' @importFrom parallel makeCluster stopCluster parLapply
#' @export
#'
#' @examples


nimbleParallel <- function(HPC, code, constants, data, inits, param,
                           iter, burnin, thin, number, sp.code) {

  cores = 3

  this_cluster <- makeCluster(cores, outfile = paste0(getwd(),"/nimble_report_", number, "-", sp.code, ".txt"))

  tmp <- lapply(1:cores, function(x) list(seed = x,
                                          inits = inits(x)))

  # run in parallel
  cat("Jack be NIMBLE... \n")
  cat("Jack be quick... \n")

  chain_output <- parLapply(cl = this_cluster,
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

  stopCluster(this_cluster)

  return(chain_output)
}
