#' run_nimbleMCMC
#'
#' @description A self-contained NIMBLE workflow to configure, build, compile, and run a NIMBLE model for a single chain.
#'
#' @param info (list) A list which contains 2 elements: info$inits which contains the initial values for a single MCMC chain, and info$seed which is a single numeric to set the seed for the chain.
#' @param code (nimbleCode)
#' @param constants (nimbleData)
#' @param data (nimbleData)
#' @param param (list?)
#' @param iter (numeric) The number of iterations to run the chain for
#' @param burnin (numeric) The number of iterations to discard as burnin
#' @param thin (numeric) The number of iterations to thin by
#'
#' @import nimble
#'
#' @returns A coda::mcmc object of the posterior samples
#' @export
#'
#' @examples



run_nimbleMCMC <- function(info, code, constants, data, param,
                           iter, burnin, thin) {

  # samples <- nimbleMCMC(code = code,
  #                       constants = constants,
  #                       data = data,
  #                       inits = info$inits,
  #                       monitors = param,
  #                       niter = iter,
  #                       nburnin = burnin,
  #                       nchains = 1,
  #                       thin = thin,
  #                       setSeed = info$seed,
  #                       samplesAsCodaMCMC = TRUE)

  Rmodel <- nimble::nimbleModel(code = code,
                                name = 'Rmodel',
                                constants = constants,
                                data = data,
                                inits = info$inits,
                                check = FALSE)

  # Must have this for HMC
  # buildDerivs = T)
  # Rmodel$initializeInfo()
  conf <- nimble::configureMCMC(Rmodel, monitors = param)
  # conf$addSampler(target = 'B', type = 'NUTS')
  Rmcmc <- nimble::buildMCMC(conf)
  Cmodel <- nimble::compileNimble(Rmodel)
  Cmcmc <- nimble::compileNimble(Rmcmc, project = Rmodel)

  samples <- nimble::runMCMC(Cmcmc,
                             niter = iter,
                             nburnin = burnin,
                             thin = thin,
                             setSeed = info$seed,
                             samplesAsCodaMCMC = TRUE)

  return(samples)
}
