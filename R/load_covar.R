# load_covar <- function(sp.code,
#                        region) {
#
#
#
#   # use grid.ids from region$sp.grid
#   locs <- region$sp.grid %>%
#     select(conus.grid.id, sp.grid.id) %>%
#     st_drop_geometry()
#
#
#   # load gridded covariates
#   load("data/USA/grid-covar.rdata")
#
#   # get covariates for grid.ids in region
#   covar <- inner_join(locs, conus.covar.grid, by = "conus.grid.id")
#
#
#
#   # now add n.iNat obs
#   if (file.exists(paste0("data/species/", sp.code, "/iNat-supp.csv"))) {
#     cat("loading and aggregating iNat records of supplemental species\n")
#
#     inat <- read.csv(paste0("data/species/", sp.code, "/iNat-supp.csv"))
#
#     inat <- inat %>%
#       st_as_sf(crs = 4326,
#                coords = c("lon", "lat")) %>%
#       st_transform(st_crs(region$sp.grid)) %>%
#       st_join(region$sp.grid, join = st_within) %>%
#       filter(is.na(conus.grid.id) == F) %>%
#       group_by(conus.grid.id) %>%
#       summarize(n.inat = n(), .groups = 'drop') %>%
#       st_drop_geometry()
#
#
#     covar <- left_join(covar, inat, by = c("conus.grid.id"))
#     covar$n.inat[which(is.na(covar$n.inat))] <- 0
#   } else {
#     cat("Warning: iNat records of supplemental species have not been downloaded yet\n")
#   }
#
#   # add centroid lat and lon to covar
#   centroid <- st_centroid(region$sp.grid) %>%
#     st_coordinates() %>%
#     as.data.frame() %>%
#     mutate(conus.grid.id = region$sp.grid$conus.grid.id)
#   colnames(centroid)[1:2] <- c("lon", "lat")
#   covar <- left_join(covar, centroid, by = "conus.grid.id")
#
#   # return
#   return(covar)
#
# }
