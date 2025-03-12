# run_nimbleMCMC <- function(info, code, constants, data, param,
#                            iter, burnin, thin) {
#
#   library(nimble)
#   # library(nimbleHMC)
#
#   # samples <- nimbleMCMC(code = code,
#   #                       constants = constants,
#   #                       data = data,
#   #                       inits = info$inits,
#   #                       monitors = param,
#   #                       niter = iter,
#   #                       nburnin = burnin,
#   #                       nchains = 1,
#   #                       thin = thin,
#   #                       setSeed = info$seed,
#   #                       samplesAsCodaMCMC = TRUE)
#
#   Rmodel <- nimbleModel(code = code,
#                         name = 'Rmodel',
#                         constants = constants,
#                         data = data,
#                         inits = info$inits,
#                         check = FALSE)
#   # Must have this for HMC
#   # buildDerivs = T)
#   # Rmodel$initializeInfo()
#   conf <- configureMCMC(Rmodel, monitors = param)
#   # conf$addSampler(target = 'B', type = 'NUTS')
#   Rmcmc <- buildMCMC(conf)
#   Cmodel <- compileNimble(Rmodel)
#   Cmcmc <- compileNimble(Rmcmc, project = Rmodel)
#
#   ## Alternative way of compiling/building after the Rmodel step
#   # Cmodel <- compileNimble(Rmodel)
#
#   # mcmc <- buildMCMC(Cmodel, monitors = param)
#
#   # Cmcmc <- compileNimble(mcmc)
#
#   samples <- runMCMC(Cmcmc,
#                      niter = iter,
#                      nburnin = burnin,
#                      thin = thin,
#                      setSeed = info$seed,
#                      samplesAsCodaMCMC = TRUE)
#
#   return(samples)
# }
