# nimbleParallel <- function(HPC, code, constants, data, inits, param,
#                            iter, burnin, thin, number, sp.code) {
#   library(parallel)
#   # if (HPC == T) {
#   #   cores <- get_cpus_per_task() - 1
#   # } else {
#   cores = 3
#   # }
#
#
#   this_cluster <- makeCluster(cores, outfile = paste0(getwd(),"/nimble_report_", number, "-", sp.code, ".txt"))
#
#   tmp <- lapply(1:cores, function(x) list(seed = x,
#                                           inits = inits(x)))
#
#   # info <- tmp[[1]]
#   # run in parallel
#   cat("Jack be NIMBLE... \n")
#   cat("Jack be quick... \n")
#
#   chain_output <- parLapply(cl = this_cluster,
#                             X = tmp,
#                             fun = run_nimbleMCMC,
#                             code = code,
#                             constants = constants,
#                             data = data,
#                             param = param,
#                             iter = iter,
#                             burnin = burnin,
#                             thin = thin)
#
#   cat("Jack jumped over the candlestick! \n")
#
#   stopCluster(this_cluster)
#
#   return(chain_output)
# }
