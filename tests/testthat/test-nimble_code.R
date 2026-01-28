# test_that("nimble_code() works", {
# 
#   source("../../../species-futures/functions/FXN-nimbleParallel.R")
# 
#   # set up ----
#   rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
#                                         paste0("../../../species-futures/data/species/GPOR/IUCN/")),
#                          range.name = c("GAP", "IUCN"), crs = 4326)
# 
#   boundary <- rangelist[[1]]
#   grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>% st_as_sf() %>%
#     mutate(conus.grid.id = 1:nrow(.)) %>%
#     rename(geometry = x)
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
#   st.map <- rnaturalearth::ne_states(country = c("Canada", "Mexico", "United States of America"),
#                       returnclass = "sf")
# 
#   stategrid <- get_state_grid(region, st.map)
# 
#   covariates <- data.frame(conus.grid.id = region$sp.grid$conus.grid.id,
#                            temp = rnorm(nrow(region$sp.grid), 0, 1),
#                            prec = rnorm(nrow(region$sp.grid), 0, 1) + runif(nrow(region$sp.grid), 0, 1))
# 
# 
#   # No covariates for PO ----
#   covs.inat <- ""
#   covs.PO <- ""
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
#                                 covs.inat = covs.inat,
#                                 covs.PO = covs.PO,
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
#                              obsc.state = "",
#                              keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#   code <- nimble_code(data,
#                       constants,
#                       path = "",
#                       sp.auto = T,
#                       coarse.grid = F,
#                       Bprior = "dnorm(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
# 
#   txt <- trimws(readLines("modelfull.r"))
# 
#   # lambda is estimated for each dataset
#   for (d in 1:4) {
#     expect_true(paste0("lambdaD", d, "[j] <- lambda0[Wcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Vcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Ycells", d, "[j]] * alpha[", d, "]") %in% txt)
#   }
# 
#   # dataset 1
#   expect_true("W1[j] ~ dpois(lambdaD1[j] * E1) # Poisson" %in% txt)
#   expect_true("log(E1) <- 0" %in% txt)
# 
# 
#   # dataset 2
#   expect_true("E2[j] <- eff2[j] * S2[j]" %in% txt)
#   expect_true("W2[j] ~ dpois(lambdaD2[j] * E2[j]) # Poisson" %in% txt)
# 
#   # dataset 3
#   expect_true("V3[j] ~ dbern(1-exp(-lambdaD3[j] * p3)) # Bernoulli" %in% txt)
#   expect_true("log(p3) <- 0" %in% txt)
# 
#   # dataset 3
#   expect_true("Y4[j] ~ dpois(lambdaD4[j] * p4) # Poisson" %in% txt)
#   expect_true("log(p4) <- 0" %in% txt)
# 
# 
#   inits <- function(x){nimble_inits(data,
#                                     constants,
#                                     sp.auto = T,
#                                     seed = x)}
# 
#   params <- nimble_params(data,
#                           constants,
#                           lambda = F,
#                           XB = F,
#                           sp.auto = F,
#                           effort = F)
# 
#   samples <- nimbleParallel(code = code,
#                             data = data,
#                             constants = constants,
#                             inits = inits,
#                             param = params,
#                             iter = 100,
#                             burnin = 25,
#                             thin = 5)
# 
#   # One covariate for PO ----
#   covs.inat <- ""
#   covs.PO <- "prec"
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
#                                 covs.inat = covs.inat,
#                                 covs.PO = covs.PO,
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
#                              obsc.state = "",
#                              keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#   code <- nimble_code(data,
#                       constants,
#                       path = "",
#                       sp.auto = T,
#                       coarse.grid = F,
#                       Bprior = "dnorm(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
# 
#   txt <- trimws(readLines("modelfull.r"))
# 
#   # lambda is estimated for each dataset
#   for (d in 1:4) {
#     expect_true(paste0("lambdaD", d, "[j] <- lambda0[Wcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Vcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Ycells", d, "[j]] * alpha[", d, "]") %in% txt)
#   }
# 
#   # dataset 1
#   expect_true("W1[j] ~ dpois(lambdaD1[j] * E1[j]) # Poisson" %in% txt)
#   expect_true("log(E1[j]) <- A1[1] * Xw1[j,1]" %in% txt)
# 
# 
#   # dataset 2
#   expect_true("E2[j] <- eff2[j] * S2[j]" %in% txt)
#   expect_true("W2[j] ~ dpois(lambdaD2[j] * E2[j]) # Poisson" %in% txt)
# 
#   # dataset 3
#   expect_true("V3[j] ~ dbern(1-exp(-lambdaD3[j] * p3)) # Bernoulli" %in% txt)
#   expect_true("log(p3) <- 0" %in% txt)
# 
#   # dataset 3
#   expect_true("Y4[j] ~ dpois(lambdaD4[j] * p4) # Poisson" %in% txt)
#   expect_true("log(p4) <- 0" %in% txt)
# 
#   inits <- function(x){nimble_inits(data,
#                                     constants,
#                                     sp.auto = T,
#                                     seed = x)}
# 
#   params <- nimble_params(data,
#                           constants,
#                           lambda = F,
#                           XB = F,
#                           sp.auto = F,
#                           effort = F)
# 
#   samples <- nimbleParallel(code = code,
#                             data = data,
#                             constants = constants,
#                             inits = inits,
#                             param = params,
#                             iter = 100,
#                             burnin = 25,
#                             thin = 5)
# 
#   # Two covariates for PO ----
#   covs.inat <- ""
#   covs.PO <- c("prec", "temp")
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
#                                 covs.inat = covs.inat,
#                                 covs.PO = covs.PO,
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
#                              obsc.state = "",
#                              keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#   code <- nimble_code(data,
#                       constants,
#                       path = "",
#                       sp.auto = T,
#                       coarse.grid = F,
#                       Bprior = "dnorm(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
# 
#   txt <- trimws(readLines("modelfull.r"))
# 
#   # lambda is estimated for each dataset
#   for (d in 1:4) {
#     expect_true(paste0("lambdaD", d, "[j] <- lambda0[Wcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Vcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Ycells", d, "[j]] * alpha[", d, "]") %in% txt)
#   }
# 
#   # dataset 1
#   expect_true("W1[j] ~ dpois(lambdaD1[j] * E1[j]) # Poisson" %in% txt)
#   expect_true("log(E1[j]) <- inprod(A1[1:nCovW1], Xw1[j,1:nCovW1])" %in% txt)
# 
# 
#   # dataset 2
#   expect_true("E2[j] <- eff2[j] * S2[j]" %in% txt)
#   expect_true("W2[j] ~ dpois(lambdaD2[j] * E2[j]) # Poisson" %in% txt)
# 
#   # dataset 3
#   expect_true("V3[j] ~ dbern(1-exp(-lambdaD3[j] * p3)) # Bernoulli" %in% txt)
#   expect_true("log(p3) <- 0" %in% txt)
# 
#   # dataset 3
#   expect_true("Y4[j] ~ dpois(lambdaD4[j] * p4) # Poisson" %in% txt)
#   expect_true("log(p4) <- 0" %in% txt)
# 
# 
#   inits <- function(x){nimble_inits(data,
#                                     constants,
#                                     sp.auto = T,
#                                     seed = x)}
# 
#   params <- nimble_params(data,
#                           constants,
#                           lambda = F,
#                           XB = F,
#                           sp.auto = F,
#                           effort = F)
# 
#   samples <- nimbleParallel(code = code,
#                             data = data,
#                             constants = constants,
#                             inits = inits,
#                             param = params,
#                             iter = 100,
#                             burnin = 25,
#                             thin = 5)
# 
#   # One covariate for iNat ----
#   covs.inat <- "prec"
#   covs.PO <- ""
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
#                                 covs.inat = covs.inat,
#                                 covs.PO = covs.PO,
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
#                              obsc.state = "",
#                              keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
# 
#   code <- nimble_code(data,
#                       constants,
#                       path = "",
#                       sp.auto = T,
#                       coarse.grid = F,
#                       Bprior = "dnorm(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
# 
# 
#   txt <- trimws(readLines("modelfull.r"))
# 
#   # lambda is estimated for each dataset
#   for (d in 1:4) {
#     expect_true(paste0("lambdaD", d, "[j] <- lambda0[Wcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Vcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Ycells", d, "[j]] * alpha[", d, "]") %in% txt)
#   }
# 
#   # dataset 1
#   expect_true("W1[j] ~ dpois(lambdaD1[j] * E1[j]) # Poisson" %in% txt)
#   expect_true("logit(eff1[j]) <- inprod(A1[1:nCovW1], Xw1[j,1:nCovW1])" %in% txt)
# 
# 
#   # dataset 2
#   expect_true("E2[j] <- eff2[j] * S2[j]" %in% txt)
#   expect_true("W2[j] ~ dpois(lambdaD2[j] * E2[j]) # Poisson" %in% txt)
# 
#   # dataset 3
#   expect_true("V3[j] ~ dbern(1-exp(-lambdaD3[j] * p3)) # Bernoulli" %in% txt)
#   expect_true("log(p3) <- 0" %in% txt)
# 
#   # dataset 4
#   expect_true("Y4[j] ~ dpois(lambdaD4[j] * p4) # Poisson" %in% txt)
#   expect_true("log(p4) <- 0" %in% txt)
# 
# 
#   inits <- function(x){nimble_inits(data,
#                                     constants,
#                                     sp.auto = T,
#                                     seed = x)}
# 
#   params <- nimble_params(data,
#                           constants,
#                           lambda = F,
#                           XB = F,
#                           sp.auto = F,
#                           effort = F)
# 
#   samples <- nimbleParallel(code = code,
#                             data = data,
#                             constants = constants,
#                             inits = inits,
#                             param = params,
#                             iter = 100,
#                             burnin = 25,
#                             thin = 5)
# 
#   # No covariates for iNat ----
#   covs.inat <- ""
#   covs.PO <- ""
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
#                                 covs.inat = covs.inat,
#                                 covs.PO = covs.PO,
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
#                              obsc.state = "",
#                              keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#   code <- nimble_code(data,
#                       constants,
#                       path = "",
#                       sp.auto = T,
#                       coarse.grid = F,
#                       Bprior = "dnorm(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
# 
#   txt <- trimws(readLines("modelfull.r"))
# 
#   # lambda is estimated for each dataset
#   for (d in 1:4) {
#     expect_true(paste0("lambdaD", d, "[j] <- lambda0[Wcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Vcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Ycells", d, "[j]] * alpha[", d, "]") %in% txt)
#   }
# 
#   # dataset 1
#   expect_true("W1[j] ~ dpois(lambdaD1[j] * E1[j]) # Poisson" %in% txt)
#   expect_true("logit(eff1[j]) <- A1[1] * Xw1[j, 1]" %in% txt)
# 
# 
#   # dataset 2
#   expect_true("E2[j] <- eff2[j] * S2[j]" %in% txt)
#   expect_true("W2[j] ~ dpois(lambdaD2[j] * E2[j]) # Poisson" %in% txt)
# 
#   # dataset 3
#   expect_true("V3[j] ~ dbern(1-exp(-lambdaD3[j] * p3)) # Bernoulli" %in% txt)
#   expect_true("log(p3) <- 0" %in% txt)
# 
#   # dataset 4
#   expect_true("Y4[j] ~ dpois(lambdaD4[j] * p4) # Poisson" %in% txt)
#   expect_true("log(p4) <- 0" %in% txt)
#   inits <- function(x){nimble_inits(data,
#                                     constants,
#                                     sp.auto = T,
#                                     seed = x)}
# 
#   params <- nimble_params(data,
#                           constants,
#                           lambda = F,
#                           XB = F,
#                           sp.auto = F,
#                           effort = F)
# 
#   samples <- nimbleParallel(code = code,
#                             data = data,
#                             constants = constants,
#                             inits = inits,
#                             param = params,
#                             iter = 100,
#                             burnin = 25,
#                             thin = 5)
# 
#   # iNat indicator variable, no covariates ----
#   covs.inat <- ""
#   covs.PO <- ""
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
#                                 covs.inat = covs.inat,
#                                 covs.PO = covs.PO,
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
#                              obsc.state = "WV",
#                              keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#   code <- nimble_code(data,
#                       constants,
#                       path = "",
#                       sp.auto = T,
#                       coarse.grid = F,
#                       Bprior = "dnorm(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
# 
#   txt <- trimws(readLines("modelfull.r"))
# 
#   # lambda is estimated for each dataset
#   for (d in 1:4) {
#     expect_true(paste0("lambdaD", d, "[j] <- lambda0[Wcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Vcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Ycells", d, "[j]] * alpha[", d, "]") %in% txt)
#   }
# 
#   # dataset 1
#   expect_true("W1[j] ~ dpois(lambdaD1[j] * E1[j]) # Poisson" %in% txt)
#   expect_true("logit(eff1[j]) <- A1[1] * Xw1[j, 1]" %in% txt)
#   expect_true("E1[j] <- eff1[j] * S1[j]" %in% txt)
# 
# 
#   # dataset 2
#   expect_true("E2[j] <- eff2[j] * S2[j]" %in% txt)
#   expect_true("W2[j] ~ dpois(lambdaD2[j] * E2[j]) # Poisson" %in% txt)
# 
#   # dataset 3
#   expect_true("V3[j] ~ dbern(1-exp(-lambdaD3[j] * p3)) # Bernoulli" %in% txt)
#   expect_true("log(p3) <- 0" %in% txt)
# 
#   # dataset 4
#   expect_true("Y4[j] ~ dpois(lambdaD4[j] * p4) # Poisson" %in% txt)
#   expect_true("log(p4) <- 0" %in% txt)
# 
#   inits <- function(x){nimble_inits(data,
#                                     constants,
#                                     sp.auto = T,
#                                     seed = x)}
# 
#   params <- nimble_params(data,
#                           constants,
#                           lambda = F,
#                           XB = F,
#                           sp.auto = F,
#                           effort = F)
# 
#   samples <- nimbleParallel(code = code,
#                             data = data,
#                             constants = constants,
#                             inits = inits,
#                             param = params,
#                             iter = 100,
#                             burnin = 25,
#                             thin = 5)
# 
#   # iNat indicator variable, one+ covariates ----
#   covs.inat <- "prec"
#   covs.PO <- ""
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
#                                 covs.inat = covs.inat,
#                                 covs.PO = covs.PO,
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
#                              obsc.state = "WV",
#                              keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#   code <- nimble_code(data,
#                       constants,
#                       path = "",
#                       sp.auto = T,
#                       coarse.grid = F,
#                       Bprior = "dnorm(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
# 
#   txt <- trimws(readLines("modelfull.r"))
# 
#   # lambda is estimated for each dataset
#   for (d in 1:4) {
#     expect_true(paste0("lambdaD", d, "[j] <- lambda0[Wcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Vcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Ycells", d, "[j]] * alpha[", d, "]") %in% txt)
#   }
# 
#   # dataset 1
#   expect_true("W1[j] ~ dpois(lambdaD1[j] * E1[j]) # Poisson" %in% txt)
#   expect_true("logit(eff1[j]) <- inprod(A1[1:nCovW1], Xw1[j,1:nCovW1])" %in% txt)
#   expect_true("E1[j] <- eff1[j] * S1[j]" %in% txt)
# 
# 
#   # dataset 2
#   expect_true("E2[j] <- eff2[j] * S2[j]" %in% txt)
#   expect_true("W2[j] ~ dpois(lambdaD2[j] * E2[j]) # Poisson" %in% txt)
# 
#   # dataset 3
#   expect_true("V3[j] ~ dbern(1-exp(-lambdaD3[j] * p3)) # Bernoulli" %in% txt)
#   expect_true("log(p3) <- 0" %in% txt)
# 
#   # dataset 4
#   expect_true("Y4[j] ~ dpois(lambdaD4[j] * p4) # Poisson" %in% txt)
#   expect_true("log(p4) <- 0" %in% txt)
# 
#   inits <- function(x){nimble_inits(data,
#                                     constants,
#                                     sp.auto = T,
#                                     seed = x)}
# 
#   params <- nimble_params(data,
#                           constants,
#                           lambda = F,
#                           XB = F,
#                           sp.auto = F,
#                           effort = F)
# 
#   samples <- nimbleParallel(code = code,
#                             data = data,
#                             constants = constants,
#                             inits = inits,
#                             param = params,
#                             iter = 100,
#                             burnin = 25,
#                             thin = 5)
# 
#   # Put iNat second ----
#   covs.inat <- "prec"
#   covs.PO <- ""
#   allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO", "Dodd_test_DND", "NEARMI_test_count"),
#                          file.label = c("iNat_test", "iNaturalist", "Dodd_test", "NEARMI_test"),
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
#                                 covs.inat = covs.inat,
#                                 covs.PO = covs.PO,
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
#                              obsc.state = "WV",
#                              keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#   code <- nimble_code(data,
#                       constants,
#                       path = "",
#                       sp.auto = T,
#                       coarse.grid = F,
#                       Bprior = "dnorm(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
# 
#   txt <- trimws(readLines("modelfull.r"))
# 
#   # lambda is estimated for each dataset
#   for (d in 1:4) {
#     expect_true(paste0("lambdaD", d, "[j] <- lambda0[Wcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Vcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Ycells", d, "[j]] * alpha[", d, "]") %in% txt)
#   }
# 
#   # dataset 1
#   expect_true("W1[j] ~ dpois(lambdaD1[j] * E1[j]) # Poisson" %in% txt)
#   expect_true("logit(eff1[j]) <- inprod(A1[1:nCovW1], Xw1[j,1:nCovW1])" %in% txt)
#   expect_true("E1[j] <- eff1[j] * S1[j]" %in% txt)
# 
# 
#   # dataset 2
#   expect_true("log(E2) <- 0" %in% txt)
#   expect_true("W2[j] ~ dpois(lambdaD2[j] * E2) # Poisson" %in% txt)
# 
#   # dataset 3
#   expect_true("V3[j] ~ dbern(1-exp(-lambdaD3[j] * p3)) # Bernoulli" %in% txt)
#   expect_true("log(p3) <- 0" %in% txt)
# 
#   # dataset 4
#   expect_true("Y4[j] ~ dpois(lambdaD4[j] * p4) # Poisson" %in% txt)
#   expect_true("log(p4) <- 0" %in% txt)
# 
#   inits <- function(x){nimble_inits(data,
#                                     constants,
#                                     sp.auto = T,
#                                     seed = x)}
# 
#   params <- nimble_params(data,
#                           constants,
#                           lambda = F,
#                           XB = F,
#                           sp.auto = F,
#                           effort = F)
# 
#   samples <- nimbleParallel(code = code,
#                             data = data,
#                             constants = constants,
#                             inits = inits,
#                             param = params,
#                             iter = 100,
#                             burnin = 25,
#                             thin = 5)
# 
#   # Covariates for surveys ----
#   covs.inat <- ""
#   covs.PO <- ""
#   allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO", "Dodd_test_DND", "NEARMI_test_count"),
#                          file.label = c("iNat_test", "iNat_test1", "Dodd_test", "NEARMI_test"),
#                          covar.mean = c(NA, NA, "elevation", "depth"),
#                          covar.sum = c(NA, NA, NA, "EffectValue"),
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
#                                 covs.inat = covs.inat,
#                                 covs.PO = covs.PO,
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
#                              obsc.state = "",
#                              keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#   code <- nimble_code(data,
#                       constants,
#                       path = "",
#                       sp.auto = T,
#                       coarse.grid = F,
#                       Bprior = "dnorm(0,1)",
#                       block.out = "none",
#                       zero_mean = T,
#                       rm.state = F,
#                       tau = 1)
# 
#   txt <- trimws(readLines("modelfull.r"))
# 
#   # lambda is estimated for each dataset
#   for (d in 1:4) {
#     expect_true(paste0("lambdaD", d, "[j] <- lambda0[Wcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Vcells", d, "[j]] * alpha[", d, "]") %in% txt |
#                   paste0("lambdaD", d, "[j] <- lambda0[Ycells", d, "[j]] * alpha[", d, "]") %in% txt)
#   }
# 
#   # dataset 1
#   expect_true("W1[j] ~ dpois(lambdaD1[j] * E1) # Poisson" %in% txt)
#   expect_true("log(E1) <- 0" %in% txt)
# 
#   # dataset 2
#   expect_true("E2[j] <- eff2[j] * S2[j]" %in% txt)
#   expect_true("W2[j] ~ dpois(lambdaD2[j] * E2[j]) # Poisson" %in% txt)
# 
#   # dataset 3
#   expect_true("V3[j] ~ dbern(1-exp(-lambdaD3[j] * p3[j])) # Bernoulli" %in% txt)
#   expect_true("log(p3[j]) <- D3[1] * Xv3[j,1]" %in% txt)
# 
#   # dataset 4
#   expect_true("Y4[j] ~ dpois(lambdaD4[j] * p4[j]) # Poisson" %in% txt)
#   expect_true("log(p4[j]) <- inprod(C4[1:nCovY4], Xy4[j,1:nCovY4])" %in% txt)
# 
#   inits <- function(x){nimble_inits(data,
#                                     constants,
#                                     sp.auto = T,
#                                     seed = x)}
# 
#   params <- nimble_params(data,
#                           constants,
#                           lambda = F,
#                           XB = F,
#                           sp.auto = F,
#                           effort = F)
# 
#   samples <- nimbleParallel(code = code,
#                             data = data,
#                             constants = constants,
#                             inits = inits,
#                             param = params,
#                             iter = 100,
#                             burnin = 25,
#                             thin = 5)
#   })
