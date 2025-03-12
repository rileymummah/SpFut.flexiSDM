# get_derived <- function(samples, project, coarse.grid, spatRegion) {
#
#   # Pull out lambda
#   keep <- grep("lambda0", colnames(samples))
#
#   as.data.frame(samples) %>%
#     select(all_of(keep)) -> lambda0
#
#
#   # Convert lambda to psi for all samples
#   psi0 <- lapply(lambda0, function(x) VGAM::clogloglink(log(x), inverse = T)) %>%
#     data.frame()
#   names(psi0) <- gsub('lambda0.','psi0[',names(psi0))
#   names(psi0) <- gsub('[.]',']',names(psi0))
#
#
#   # Projections
#
#   # Load projections
#   if (project > 0) {
#     source('code/03-species-models/setup-projections.R')
#
#
#     # Pull spat and beta out of the output file
#     keep <- grep("spat", colnames(samples))
#
#     as.data.frame(samples) %>%
#       select(all_of(keep)) %>%
#       as.matrix() -> spat
#
#     # Realign the spatial grid to the species grid if coarse.grid
#     # This loop could probably be sped up if it was vectorized.
#     if (coarse.grid == T) {
#       cells <- spatRegion$spatkey$spat.grid.id
#
#       tmp <- c()
#       suppressMessages(
#         for (i in 1:length(cells)) {
#
#           index <- which(colnames(spat) == paste0('spat[',cells[i],']'))
#           tmp <- bind_cols(tmp, spat[,index])
#         }
#       )
#       colnames(tmp) <- paste0('spat[',1:length(cells),']')
#       spat <- tmp
#       rm(tmp)
#
#     }
#
#     keep <- grep('^B', colnames(samples))
#
#     as.data.frame(samples) %>%
#       select(all_of(keep)) %>%
#       as.matrix() -> beta
#
#     # Get projections
#     proj <- lapply(1:project, get_projections, data, beta, spat, lambda0)
#
#     proj <- do.call(cbind, proj)
#
#     derived <- bind_cols(samples, psi0, proj)
#   } else {
#
#     derived <- bind_cols(samples, psi0)
#   }
#
#   return(derived)
# }
