# nimble_params <- function(data,
#                           constans,
#                           sp.auto,
#                           # project = 0,
#                           lambda = T,
#                           # psi = F,
#                           XB = F) {
#
#   params <- c("B", "alpha")
#
#
#   if (sp.auto == T) params <- c(params, "spat", "tau")
#
#
#   if (lambda == T) {
#     params <- c(params, "lambda0")
#
#     # if (project > 0) {
#     #   for (z in 1:project) {
#     #     params <- c(params, paste0("lambda", z))
#     #   }
#     # }
#   }
#
#   # if (psi == T) {
#   #params <- c(params, "psi0")
#
#   # if (project > 0) {
#   #   for (z in 1:project) {
#   #     params <- c(params, paste0("psi", z))
#   #   }
#   # }
#   # }
#
#   if (XB == T) {
#     params <- c(params, "XB0")
#
#     # if (project > 0) {
#     #   for (z in 1:project) {
#     #     params <- c(params, paste0("XB", z))
#     #   }
#     # }
#   }
#
#
#   for (d in 1:constants$nD) {
#
#     if (paste0("Xy", d) %in% names(data)) {
#       # count
#
#       # if (constants[[paste0("nCovY", d)]] == 0) {
#       #   params <- c(params, paste0("p", d))
#       #
#       # } else {
#       params <- c(params, paste0("C", d))
#       # }
#
#
#     } else if (paste0("Xw", d) %in% names(data)) {
#       # PO
#
#       # if (constants[[paste0("nCovW", d)]] == 0) {
#       #   params <- c(params, paste0("E", d))
#       #
#       # } else {
#       params <- c(params, paste0("A", d))
#       # }
#
#
#     } else if (paste0("Xv", d) %in% names(data)) {
#       # DND
#
#       # if (constants[[paste0("nCovV", d)]] == 0) {
#       #   params <- c(params, paste0("p", d))
#       #
#       # } else {
#       params <- c(params, paste0("D", d))
#       #}
#     }
#
#   } # end loop through datasets
#
#
#   return(params)
# }
