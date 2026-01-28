# test_that("add_state_ind() works", {
#   
#   
#   
#   # set up ----
#   st.map <- ne_states(country = c("Canada", "Mexico", "United States of America"),
#                       returnclass = "sf") 
#   
#   rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
#                                         paste0("../../../species-futures/data/species/GPOR/IUCN/")),
#                          range.name = c("GAP", "IUCN"), crs = 4326)
#   
#   boundary <- rangelist[[1]]
#   grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))
#   
#   region <- make_region(rangelist,
#                         buffer = 1,
#                         sub = F,
#                         boundary = boundary,
#                         grid = grid,
#                         rm.clumps = F,
#                         clump.size = 2,
#                         continuous = F)
#   
#   covariates <- data.frame(conus.grid.id = region$sp.grid$conus.grid.id,
#                            temp = rnorm(nrow(region$sp.grid), 0, 1),
#                            prec = rnorm(nrow(region$sp.grid), 0, 1) + runif(nrow(region$sp.grid), 0, 1))
#   
#   stategrid <- get_state_grid(region, st.map)
#   
#   # works for state PO ----
#   allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO", "Dodd_test_DND", "NEARMI_test_count"),
#                          file.label = c("iNat_test", "iNat_test1", "Dodd_test", "NEARMI_test"),
#                          covar.mean = c(NA, NA, NA, NA),
#                          covar.sum = c(NA, NA, NA, NA),
#                          data.type = c("PO", "PO", "DND", "count"),
#                          PO.extent = c("CONUS", "PA", NA, NA))
#   
#   species.data <- load_species_data(sp.code = "GPOR",
#                                     sp.code.all = "GPOR",
#                                     file.info = allfiles,
#                                     file.path = "../../../species-futures/pkg-tests/data-ready-testfunctions/",
#                                     #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                     region = region,
#                                     filter.region = T,
#                                     year.start = 1800,
#                                     year.end = 2025,
#                                     coordunc = 1000,
#                                     coordunc_na.rm = T,
#                                     spat.thin = F,
#                                     keep.conus.grid.id = region$sp.grid$conus.grid.id)
#   
#   sp.data <- sppdata_for_nimble(species.data,
#                                 region,
#                                 file.info = allfiles,
#                                 covar = covariates,
#                                 stategrid = stategrid,
#                                 covs.inat = "prec",
#                                 covs.PO = NA,
#                                 DND.maybe = 1,
#                                 keep.conus.grid.id = region$sp.grid$conus.grid.id)
#   
#   gridkey <- select(region$sp.grid, conus.grid.id) %>%
#     st_drop_geometry() %>%
#     mutate(grid.id = 1:nrow(.),
#            group = "train")
#   
#   tmp <- data_for_nimble(sp.data,
#                          covar = covariates,
#                          covs.z = c("temp", "prec"),
#                          sp.auto = T,
#                          coarse.grid = F,
#                          region = region,
#                          process.intercept = F,
#                          gridkey = gridkey,
#                          spatRegion= spatRegion)
#   
#   data <- tmp$data
#   constants <- tmp$constants
#   
#   constants <- add_state_ind(species.data,
#                              region,
#                              gridkey,
#                              constants,
#                              stategrid = stategrid,
#                              covs.inat = "prec",
#                              obsc.state = obsc.state,
#                              keep.conus.grid.id = gridkey$conus.grid.id[which(gridkey$group == "train")])
#   
#   
#   expect_equal(length(constants$S2), constants$nCell)
#   
#   
#   # works for iNat ----
#   allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO", "Dodd_test_DND", "NEARMI_test_count"),
#                          file.label = c("iNaturalist", "iNat_test1", "Dodd_test", "NEARMI_test"),
#                          covar.mean = c(NA, NA, NA, NA),
#                          covar.sum = c(NA, NA, NA, NA),
#                          data.type = c("PO", "PO", "DND", "count"),
#                          PO.extent = c("CONUS", "PA", NA, NA))
#   
#   species.data <- load_species_data(sp.code = "GPOR",
#                                     sp.code.all = "GPOR",
#                                     file.info = allfiles,
#                                     file.path = "../../../species-futures/pkg-tests/data-ready-testfunctions/",
#                                     #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                     region = region,
#                                     filter.region = T,
#                                     year.start = 1800,
#                                     year.end = 2025,
#                                     coordunc = 1000,
#                                     coordunc_na.rm = T,
#                                     spat.thin = F,
#                                     keep.conus.grid.id = region$sp.grid$conus.grid.id)
#   
#   sp.data <- sppdata_for_nimble(species.data,
#                                 region,
#                                 file.info = allfiles,
#                                 covar = covariates,
#                                 stategrid = stategrid,
#                                 covs.inat = "prec",
#                                 covs.PO = NA,
#                                 DND.maybe = 1,
#                                 keep.conus.grid.id = region$sp.grid$conus.grid.id)
#   
#   tmp <- data_for_nimble(sp.data,
#                          covar = covariates,
#                          covs.z = c("temp", "prec"),
#                          sp.auto = T,
#                          coarse.grid = F,
#                          region = region,
#                          process.intercept = F,
#                          gridkey = gridkey,
#                          spatRegion= spatRegion)
#   
#   data <- tmp$data
#   constants <- tmp$constants
#   
#   constants <- add_state_ind(species.data,
#                              region,
#                              gridkey,
#                              constants,
#                              stategrid = stategrid,
#                              covs.inat = "prec",
#                              obsc.state = "PA",
#                              keep.conus.grid.id = gridkey$conus.grid.id[which(gridkey$group == "train")])
#   
#   expect_equal(length(constants$S1), constants$nCell)
#   expect_equal(length(constants$S2), constants$nCell)
#   
#   
#   # works with coarse.grid == T ----
#   spatRegion <- suppressWarnings(make_spatkey(region$sp.grid))
#   tmp <- data_for_nimble(sp.data,
#                          covar = covariates,
#                          covs.z = c("temp", "prec"),
#                          sp.auto = T,
#                          coarse.grid = F,
#                          region = region,
#                          process.intercept = F,
#                          gridkey = gridkey,
#                          spatRegion= spatRegion)
#   
#   data <- tmp$data
#   constants <- tmp$constants
#   
#   constants <- add_state_ind(species.data,
#                              region,
#                              gridkey,
#                              constants,
#                              stategrid = stategrid,
#                              covs.inat = "prec",
#                              obsc.state = "PA",
#                              keep.conus.grid.id = gridkey$conus.grid.id[which(gridkey$group == "train")])
#   
#   expect_equal(length(constants$S1), constants$nCell)
#   expect_equal(length(constants$S2), constants$nCell)
#   
# })
