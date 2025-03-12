# nimble_inits <- function(data,
#                          constants,
#                          sp.auto,
#                          min.visits.incl = 3,
#                          seed = as.numeric(Sys.time())) {
#
#   set.seed(seed)
#
#   dat <- list(B = rnorm(constants$nCovZ, 0, 0.01),
#               w = rnorm(constants$nD, 0, 0.1))
#
#
#   if (sp.auto == T) {
#     dat[["tau"]] <- 1
#     dat[["spat"]] <- rep(0, length(constants$num))
#
#   }
#
#
#
#   for (d in 1:constants$nD) {
#
#     if (paste0("Xy", d) %in% names(data)) {
#       # count
#
#       if (constants[[paste0("nCovY", d)]] == 0) {
#         # p is fixed
#         dat[[paste0("p", d)]] <- rbeta(1, 2, 2)
#
#       } else if (constants[[paste0("nCovY", d)]] > 0) {
#         dat[[paste0("C", d)]] <- rnorm(constants[[paste0("nCovY", d)]])
#         dat[[paste0("Xy", d)]] <- matrix(nrow = nrow(data[[paste0("Xy", d)]]),
#                                          ncol = ncol(data[[paste0("Xy", d)]]),
#                                          0)
#       }
#
#       # initialize ND if N-mixture model
#       if (constants[[paste0("nVisitsY", d)]] > min.visits.incl) {
#         dat[[paste0("ND", d)]] <- rep(1, constants[[paste0("nY", d)]])
#       }
#
#
#
#     } else if (paste0("Xw", d) %in% names(data)) {
#       # PO
#
#       if (constants[[paste0("nCovW", d)]] == 0) {
#         # p is fixed
#         dat[[paste0("E", d)]] <- rbeta(1, 2, 2)
#
#       } else if (constants[[paste0("nCovW", d)]] > 0) {
#         dat[[paste0("A", d)]] <- rnorm(constants[[paste0("nCovW", d)]])
#         dat[[paste0("Xw", d)]] <- matrix(nrow = nrow(data[[paste0("Xw", d)]]),
#                                          ncol = ncol(data[[paste0("Xw", d)]]),
#                                          0)
#       }
#
#
#     } else if (paste0("Xv", d) %in% names(data)) {
#       # DND
#
#       if (constants[[paste0("nCovV", d)]] == 0) {
#         # p is fixed
#         dat[[paste0("d", d)]] <- rbeta(1, 2, 2)
#
#       } else if (constants[[paste0("nCovV", d)]] > 0) {
#         dat[[paste0("D", d)]] <- rnorm(constants[[paste0("nCovV", d)]])
#         dat[[paste0("Xv", d)]] <- matrix(nrow = nrow(data[[paste0("Xv", d)]]),
#                                          ncol = ncol(data[[paste0("Xv", d)]]),
#                                          0)
#       }
#
#       # initialize ND if N-mixture model
#       if (constants[[paste0("nVisitsV", d)]] > min.visits.incl) {
#         dat[[paste0("ND", d)]] <- rep(1, constants[[paste0("nV", d)]])
#       }
#     }
#
#   } # end loop through datasets
#
#
#   return(dat)
#
# }
