# add_int_cols <- function(covar,
#                          int.factor,
#                          int.cont) {
#
#   ind.cont <- which(colnames(covar) == int.cont)
#
#
#   ind.factor <- grep(paste0(int.factor, "."), colnames(covar))
#   ind.factor.rm <- grep(paste0("_", int.factor), colnames(covar))
#   if (length(ind.factor.rm) > 0) {
#     ind.factor <- ind.factor[-which(ind.factor %in% ind.factor.rm)]
#   }
#   namecols <- colnames(covar)[ind.factor]
#
#   # multiply by cont variable
#   for (i in 1:length(namecols)) {
#     covar[,paste0(int.cont, "_x_", namecols[i])] <- covar[,int.cont] * covar[,namecols[i]]
#   }
#
#
#   tmp <- list(covar = covar,
#               names = paste0(int.cont, "_x_", namecols))
#
#   return(tmp)
# }
