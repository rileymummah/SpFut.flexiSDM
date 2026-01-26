# test_that("nimble_code() works", {
#   
#   # set up ----
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
#   
#   # No covariates for PO ----
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
#                                     file.path = "../../../species-futures/DATA SWAMP/data-ready-testfunctions/",
#                                     #file.path = "../species-futures/DATA SWAMP/data-ready-testfunctions/",
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
#                          sp.auto = F,
#                          coarse.grid = F,
#                          region = region,
#                          process.intercept = F,
#                          gridkey = gridkey,
#                          spatRegion= spatRegion)
#   
#   data <- tmp$data
#   constants <- tmp$constants
#   
#   code <- nimble_code(data,
#                       constants, 
#                       path = "",
#                       sp.auto = T, 
#                       coarse.grid = F,
#                       Bprior = "dnorn(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
#   
#   ## I don't think this is turning out right when there are no covariates
#   
#   
#   # One covariate for PO ----
#   sp.data <- sppdata_for_nimble(species.data,
#                                 region,
#                                 file.info = allfiles,
#                                 covar = covariates,
#                                 covs.inat = "prec",
#                                 covs.PO = "prec",
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
#                          sp.auto = F,
#                          coarse.grid = F,
#                          region = region,
#                          process.intercept = F,
#                          gridkey = gridkey,
#                          spatRegion= spatRegion)
#   
#   data <- tmp$data
#   constants <- tmp$constants
#   
#   code <- nimble_code(data,
#                       constants, 
#                       path = "",
#                       sp.auto = T, 
#                       coarse.grid = F,
#                       Bprior = "dnorn(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
#   
#   # Two covariates for PO ----
#   sp.data <- sppdata_for_nimble(species.data,
#                                 region,
#                                 file.info = allfiles,
#                                 covar = covariates,
#                                 covs.inat = "prec",
#                                 covs.PO = c("temp", "prec"),
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
#                          sp.auto = F,
#                          coarse.grid = F,
#                          region = region,
#                          process.intercept = F,
#                          gridkey = gridkey,
#                          spatRegion= spatRegion)
#   
#   data <- tmp$data
#   constants <- tmp$constants
#   
#   code <- nimble_code(data,
#                       constants, 
#                       path = "",
#                       sp.auto = T, 
#                       coarse.grid = F,
#                       Bprior = "dnorn(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
#   
#   
#   # One covariate for inat ----
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
#                                     #file.path = "../../../species-futures/DATA SWAMP/data-ready-testfunctions/",
#                                     file.path = "../species-futures/DATA SWAMP/data-ready-testfunctions/",
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
#                          sp.auto = F,
#                          coarse.grid = F,
#                          region = region,
#                          process.intercept = F,
#                          gridkey = gridkey,
#                          spatRegion= spatRegion)
#   
#   data <- tmp$data
#   constants <- tmp$constants
#   
#   code <- nimble_code(data,
#                       constants, 
#                       path = "",
#                       sp.auto = T, 
#                       coarse.grid = F,
#                       Bprior = "dnorn(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
#   
#   # No covariates for inat ----
#   sp.data <- sppdata_for_nimble(species.data,
#                                 region,
#                                 file.info = allfiles,
#                                 covar = covariates,
#                                 covs.inat = "",
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
#                          sp.auto = F,
#                          coarse.grid = F,
#                          region = region,
#                          process.intercept = F,
#                          gridkey = gridkey,
#                          spatRegion= spatRegion)
#   
#   data <- tmp$data
#   constants <- tmp$constants
#   
#   code <- nimble_code(data,
#                       constants, 
#                       path = "",
#                       sp.auto = T, 
#                       coarse.grid = F,
#                       Bprior = "dnorn(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
#   
#   
#   # Covariates for surveys ----
#   allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO", "Dodd_test_DND", "NEARMI_test_count"),
#                          file.label = c("iNat_test", "iNat_test1", "Dodd_test", "NEARMI_test"),
#                          covar.mean = c(NA, NA, "elevation", NA),
#                          covar.sum = c(NA, NA, NA, "EffectValue"),
#                          data.type = c("PO", "PO", "DND", "count"),
#                          PO.extent = c("CONUS", "PA", NA, NA))
#   
#   species.data <- load_species_data(sp.code = "GPOR",
#                                     sp.code.all = "GPOR",
#                                     file.info = allfiles,
#                                     #file.path = "../../../species-futures/DATA SWAMP/data-ready-testfunctions/",
#                                     file.path = "../species-futures/DATA SWAMP/data-ready-testfunctions/",
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
#                          sp.auto = F,
#                          coarse.grid = F,
#                          region = region,
#                          process.intercept = F,
#                          gridkey = gridkey,
#                          spatRegion= spatRegion)
#   
#   data <- tmp$data
#   constants <- tmp$constants
#   
#   code <- nimble_code(data,
#                       constants, 
#                       path = "",
#                       sp.auto = T, 
#                       coarse.grid = F,
#                       Bprior = "dnorn(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
# })
