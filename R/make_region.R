# make_region <- function(sp.code,
#                         buffer = 100000,
#                         sub = F,
#                         usa.path = "data/USA/",
#                         crs = 3857,
#                         overwrite = F,
#                         lat.hi = NA,
#                         lat.lo = NA,
#                         lon.hi = NA,
#                         lon.lo = NA,
#                         continuous = T,
#                         rm.clumps = F) {
#   sf::sf_use_s2(FALSE)
#
#   latlon <- paste0("_", lon.lo, "_", lat.lo, "_", lon.hi, "_", lat.hi)
#
#   # if it already exists, load it
#   if (file.exists(paste0("data/species/", sp.code, "/region-", buffer, "-", sub, latlon, ".rds")) & overwrite == F) {
#     dat <- read_rds(paste0("data/species/", sp.code, "/region-", buffer, "-", sub, latlon, ".rds"))
#   } else {
#
#     range <- get_range(sp.code)
#     if (nrow(range[[1]]) == 0) {
#       region <- NA
#       return(region)
#     } else {
#
#       for (r in 1:length(range)) {
#         range[[r]] <- st_transform(range[[r]], crs = crs)
#       }
#
#
#       # combine ranges
#       if (length(range) == 1) {
#         full <- range[[r]]
#         full1 <- range[[r]]
#       } else if (length(range) == 2) {
#         full <- bind_rows(range[[1]], range[[2]])
#         full1 <- st_union(range[[1]], range[[2]]) %>%
#           summarize()
#       } else if (length(range) > 2) {
#         stop("Not equipped to handle more than 2 ranges")
#       }
#
#
#       if (sub == T) {
#         full1 <- st_centroid(full)[1,]
#       }
#
#       # add buffer
#       # dist = 100000 means 100,000m means 100km
#       region <- full1 %>%
#         st_buffer(dist = buffer)
#
#       # crop with bounding box
#       # since we use degrees to crop, switch region to degree projection first
#       bb <- st_bbox(st_transform(region, 4326))
#       if (is.na(lon.hi) == F) {
#         bb[3] <- lon.hi
#       }
#       if (is.na(lon.lo) == F) {
#         bb[1] <- lon.lo
#       }
#       if (is.na(lat.hi) == F) {
#         bb[4] <- lat.hi
#       }
#       if (is.na(lat.lo) == F) {
#         bb[2] <- lat.lo
#       }
#
#       region <- st_crop(st_transform(region, 4326), bb) %>%
#         st_transform(crs)
#
#
#       exclude <- c("Alaska", "Hawaii", "Commonwealth of the Northern Mariana Islands",
#                    "American Samoa", "United States Virgin Islands", "Guam", "Puerto Rico")
#
#       usa <- st_read("data/USA/maps/cb_2018_us_state_500k/cb_2018_us_state_500k.shp") %>%
#         filter((NAME %in% exclude) == F) %>%
#         # Change CRS from 4269 (NAD83) to 3857 (WGS84) (degrees to meters)
#         st_transform(crs = crs)
#
#       usa1 <- usa %>%
#         st_union()
#
#
#       # now get intersection with usa grid
#       if (file.exists(paste0(usa.path, "grid-covar.rdata"))) {
#         load(paste0(usa.path, "grid-covar.rdata"))
#         region <- st_intersection(region, usa1)
#
#         if (nrow(region) > 0) {
#           usa.grid1 <- st_transform(conus.grid, crs = crs)
#
#
#           # this gets grid cells but with some cut off
#           grid <- st_intersection(region, usa.grid1)
#
#           # this gets the whole grid cells and assigns sp.grid.id
#           grida <- usa.grid1 %>%
#             filter(conus.grid.id %in% grid$conus.grid.id)
#
#           # find cells that are too small and remove
#           gridb <- grida %>%
#             mutate(area = as.numeric(st_area(geometry))) %>%
#             filter(area > 24999998) %>%
#             select(!area)
#
#           # Find which grid cells are split into multiple pieces (eg by water) and remove
#           grid1 <- gridb %>%
#             st_cast("MULTIPOLYGON") %>%
#             st_cast("POLYGON")
#           mult <- as.data.frame(table(grid1$conus.grid.id)) %>%
#             filter(Freq > 1) %>%
#             pull(Var1)
#
#           if (length(mult) > 0) {
#             grid <- grid[-which(grid$conus.grid.id %in% mult),]
#           }
#
#           # Find cells that are not continuous and remove
#           if (continuous == T) {
#             grid2 <- gridb %>%
#               st_union() %>%
#               st_as_sf() %>%
#               st_cast("POLYGON") %>%
#               mutate(area = st_area(x)) %>%
#               filter(area == max(area))
#             gridc <- st_intersection(gridb, grid2)
#           } else if (continuous == F) {
#             gridc <- gridb
#           }
#
#
#           if (rm.clumps == T) {
#             # Find and remove cells that are in clumps of less than 20 cells
#             grid1 <- gridc %>%
#               # group neighboring cells
#               summarize(geometry = st_union(geometry, by_feature = F)) %>%
#               st_cast("POLYGON")%>%
#
#               # remove cells in group of < 20 (area < 500000000)
#               mutate(area = as.numeric(st_area(geometry))) %>%
#               filter(area >= 500000000)
#             gridd <- st_intersection(gridc, grid1) %>%
#               ungroup()
#             #gridd <- gridc
#           } else {
#             gridd <- gridc
#           }
#
#
#
#           # find which cells are orphans and remove
#           tmp <- st_touches(gridd)
#           tmp1 <- unlist(lapply(tmp, length))
#           orphans <- which(tmp1 <= 1)
#
#           cat("Removing cells with <=1 neighbor...\n")
#
#           while (length(orphans) > 0) {
#
#             cat(paste0((length(orphans)), " remaining ", "\n"))
#             gridd <- gridd[-orphans,]
#
#             tmp <- st_touches(gridd)
#             tmp1 <- unlist(lapply(tmp, length))
#             orphans <- which(tmp1 <= 1)
#           }
#
#
#           gride <- gridd %>%
#             mutate(sp.grid.id = row_number())
#
#
#
#           dat <- list(range = full,
#                       region = region,
#                       sp.grid = gride,
#                       conus = usa)
#         } else {
#           dat <- NA
#         }
#
#
#       } else {
#         cat("USA Grid must be generated on this computer before sp.grid can be produced.")
#         dat <- list(range = full,
#                     region = region,
#                     conus = usa)
#       }
#
#       write_rds(dat, file = paste0("data/species/", sp.code, "/region-", buffer, "-", sub, latlon, ".rds"))
#
#       return(dat)
#
#     }
#
#
#   }
#
#
# }
