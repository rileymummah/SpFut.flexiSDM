# data_for_nimble <- function(sp.data,
#                             covar,
#                             covs.z,
#                             sp.auto = F,
#                             coarse.grid = T,
#                             sp.grid,
#                             area = F,
#                             process.intercept = T,
#                             gridkey,
#                             spatRegion) {
#
#   # get number of datasets
#   nD <- length(sp.data)
#
#   # setup Z
#   nCell <- nrow(covar)
#
#   if (process.intercept == T) {
#     covariates <- cbind(conus.grid.id = covar$conus.grid.id, Z1 = rep(1, nrow(covar)),
#                         covar[,covs.z])
#   }
#   if (process.intercept == F) {
#     covariates <- cbind(conus.grid.id = covar$conus.grid.id,
#                         covar[,covs.z])
#   }
#
#   Z <- z_for_nimble(nCell, covariates = covariates)
#
#
#   # combine
#   data <- sp.data
#
#   names(data) <- NULL
#   input <- c(Z, unlist(data, recursive = F, use.names = T))
#
#   data <- input[names(input) == "data"]
#   names(data) <- NULL
#   data <- unlist(data, recursive  = F, use.names = T)
#
#   constants <- input[names(input) == "constants"]
#   names(constants) <- NULL
#   constants <- unlist(constants, recursive  = F, use.names = T)
#   constants$nD <- nD
#
#   # set up area
#   if (area == T) {
#     surv <- grep("count|PO|DND", names(sp.data))
#     surv <- as.numeric(gsub("count|PO|POstates|DND", "", names(sp.data)[surv]))
#     for (i in 1:length(surv)) {
#
#       if (paste0("Xy", surv[i]) %in% names(data)) {
#         tmp <- data[[paste0("Xy", surv[i])]]
#
#
#         if("area" %in% colnames(tmp)) {
#           data[[paste0("area", surv[i])]] <- tmp$area
#           tmp$area <- NULL
#         } else {
#           data[[paste0("area", surv[i])]] <- rep(1, nrow(tmp))
#         }
#       } else if (paste0("Xv", surv[i]) %in% names(data)) {
#         tmp <- data[[paste0("Xv", surv[i])]]
#
#
#         if("area" %in% colnames(tmp)) {
#           data[[paste0("area", surv[i])]] <- tmp$area
#           tmp$area <- NULL
#         } else {
#           data[[paste0("area", surv[i])]] <- rep(1, nrow(tmp))
#         }
#       } else {
#         if (paste0("Y", surv[i]) %in% names(data)) {
#           data[[paste0("area", surv[i])]] <- rep(1, length(data[[paste0("Y", surv[i])]]))
#         }
#         if (paste0("W", surv[i]) %in% names(data)) {
#           data[[paste0("area", surv[i])]] <- rep(1, length(data[[paste0("W", surv[i])]]))
#         }
#         if (paste0("V", surv[i]) %in% names(data)) {
#           data[[paste0("area", surv[i])]] <- rep(1, length(data[[paste0("V", surv[i])]]))
#         }
#       }
#
#
#     }
#
#   }
#
#   if (coarse.grid == T) {
#     spatkey <- spatRegion$spatkey
#
#     spat.grid <- spatRegion$spat.grid
#
#   } else {
#     spat.grid <- sp.grid
#   }
#
#   if (sp.auto == T) {
#     NB <- spdep::poly2nb(spat.grid)
#     NBinfo <- spdep::nb2WB(NB)
#
#     constants$L <- length(NBinfo$adj)
#     constants$adj <- NBinfo$adj
#     constants$weights <- NBinfo$weights
#     constants$num <- NBinfo$num
#   }
#
#   if (coarse.grid == T) {
#     # These are the spatial cells that correspond to each grid cell i
#     # When indexed by i in the model, they should correspond correctly
#     constants$spatCells <- spatkey$spat.grid.id
#     constants$nSpatCell <- length(unique(spatkey$spat.grid.id))
#   }
#
#
#   # fix cell values -- these need to be 1:ncells
#   # grid <- data.frame(grid.id = 1:constants$nCell,
#   #                    conus.grid.id = Z$constants$train.i)
#   #grid <- gridkey1
#
#   # for each dataset, change conus.grid.id to sp.grid.id
#   cells <- grep("cells", names(constants))
#   for (e in cells) {
#     tmp <- data.frame(conus.grid.id = as.character(constants[[e]])) %>%
#       inner_join(gridkey, by = "conus.grid.id")
#     constants[[names(constants)[e]]] <- tmp$grid.id
#   }
#
#   # now add train.i
#   # tmp <- data.frame(conus.grid.id = constants[["train.i"]]) %>%
#   #   inner_join(grid, by = "conus.grid.id")
#   # constants[["train.i"]] <- tmp$grid.id
#   constants$train.i <- gridkey$grid.id[which(gridkey$group == "train")]
#
#   # grid <- grid %>%
#   #   mutate(group = case_when(conus.grid.id %in% keep.conus.grid.id ~ "train",
#   #                            T ~ "test"))
#   #
#   # return(list(data = data,
#   #             constants = constants,
#   #             gridkey = grid))
#   return(list(data = data,
#               constants = constants))
#
#
# }
