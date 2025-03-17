#' Title
#'
#' @param info
#' @param code (nimbleCode)
#' @param constants (nimbleData)
#' @param data (nimbleData)
#' @param param
#' @param iter (List or function?)
#' @param burnin
#' @param thin
#'
#' @importFrom nimble nimbleModel configureMCMC buildMCMC compileNimble runMCMC
#' @returns
#' @export
#'
#' @examples
#' \dontrun{
#'
#'
#'
#' }


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

  Rmodel <- nimbleModel(code = code,
                        name = 'Rmodel',
                        constants = constants,
                        data = data,
                        inits = info$inits,
                        check = FALSE)
  # Must have this for HMC
  # buildDerivs = T)
  # Rmodel$initializeInfo()
  conf <- configureMCMC(Rmodel, monitors = param)
  # conf$addSampler(target = 'B', type = 'NUTS')
  Rmcmc <- buildMCMC(conf)
  Cmodel <- compileNimble(Rmodel)
  Cmcmc <- compileNimble(Rmcmc, project = Rmodel)

  samples <- runMCMC(Cmcmc,
                     niter = iter,
                     nburnin = burnin,
                     thin = thin,
                     setSeed = info$seed,
                     samplesAsCodaMCMC = TRUE)

  return(samples)
}
