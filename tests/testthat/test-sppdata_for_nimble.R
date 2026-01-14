test_that("sppdata_for_nimble() works", {
  
  # set up ----
  rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
                                        paste0("../../../species-futures/data/species/GPOR/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)
  
  boundary <- rangelist[[1]]
  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))
  
  region <- make_region(rangelist,
                        buffer = 1,
                        sub = F,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = F,
                        clump.size = 2,
                        continuous = F)
  
  covariates <- data.frame(conus.grid.id = region$sp.grid$conus.grid.id,
                           temp = rnorm(nrow(region$sp.grid), 0, 1),
                           prec = rnorm(nrow(region$sp.grid), 0, 1) + runif(nrow(region$sp.grid), 0, 1))
  
  
  # PO data - iNat ----
  
  ## works! ----
  allfiles <- data.frame(file.name = c("iNat_test_PO"),
                         file.label = c("iNaturalist"),
                         covar.mean = c(NA),
                         covar.sum = c(NA),
                         data.type = c("PO"))
  
  species.data <- load_species_data(sp.code = "GPOR",
                                    sp.code.all = "GPOR",
                                    file.info = allfiles,
                                    #file.path = "../../../species-futures/DATA SWAMP/data-ready-testfunctions/",
                                    file.path = "../species-futures/DATA SWAMP/data-ready-testfunctions/",
                                    region = region,
                                    filter.region = T,
                                    year.start = 1800,
                                    year.end = 2025,
                                    coordunc = 1000,
                                    coordunc_na.rm = T,
                                    spat.thin = F,
                                    keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  sp.data <- sppdata_for_nimble(species.data,
                                region,
                                file.info = allfiles,
                                covar = covariates,
                                covs.inat = "prec",
                                covs.PO = NA,
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)
  

  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 1)
  expect_equal(names(sp.data), "PO1")
  expect_equal(names(sp.data$PO1), c("data", "constants"))
  expect_equal(colnames(sp.data$PO1$data$Xw1), c("intercept", "prec"))
  
  ## mess around with covariates ----
  sp.data <- sppdata_for_nimble(species.data,
                                region,
                                file.info = allfiles,
                                covar = covariates,
                                covs.inat = "", # no effort covariate
                                covs.PO = "",
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 1)
  expect_equal(names(sp.data), "PO1")
  expect_equal(names(sp.data$PO1), c("data", "constants"))
  expect_equal(colnames(sp.data$PO1$data$Xw1), c("intercept"))
  
  sp.data <- sppdata_for_nimble(species.data,
                                region,
                                file.info = allfiles,
                                covar = covariates,
                                covs.inat = NA, # no effort covariate
                                covs.PO = "",
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 1)
  expect_equal(names(sp.data), "PO1")
  expect_equal(names(sp.data$PO1), c("data", "constants"))
  expect_equal(colnames(sp.data$PO1$data$Xw1), c("intercept"))
  
  
  expect_warning(sp.data <- sppdata_for_nimble(species.data,
                                region,
                                file.info = allfiles,
                                covar = covariates,
                                covs.inat = "traveltime", # effort covariate that isn't in covariates
                                covs.PO = "",
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id))
  
  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 1)
  expect_equal(names(sp.data), "PO1")
  expect_equal(names(sp.data$PO1), c("data", "constants"))
  expect_equal(colnames(sp.data$PO1$data$Xw1), c("intercept"))
  
  
  # PO data - other ----
  
  ## missing PO.extent ----
  allfiles <- data.frame(file.name = c("iNat_test_PO"),
                         file.label = c("iNat_test"),
                         covar.mean = c(NA),
                         covar.sum = c(NA),
                         data.type = c("PO"))
  
  species.data <- load_species_data(sp.code = "GPOR",
                                    sp.code.all = "GPOR",
                                    file.info = allfiles,
                                    #file.path = "../../../species-futures/DATA SWAMP/data-ready-testfunctions/",
                                    file.path = "../species-futures/DATA SWAMP/data-ready-testfunctions/",
                                    region = region,
                                    filter.region = T,
                                    year.start = 1800,
                                    year.end = 2025,
                                    coordunc = 1000,
                                    coordunc_na.rm = T,
                                    spat.thin = F,
                                    keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  expect_error(sp.data <- sppdata_for_nimble(species.data,
                                region,
                                file.info = allfiles,
                                covar = covariates,
                                covs.inat = "prec",
                                covs.PO = NA,
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id))
  
  ## works! with CONUS extent ----
  allfiles$PO.extent <- "CONUS"
  
  sp.data <- sppdata_for_nimble(species.data,
                                region,
                                file.info = allfiles,
                                covar = covariates,
                                covs.inat = "prec",
                                covs.PO = NA,
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)
  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 1)
  expect_equal(names(sp.data), "PO1")
  expect_equal(names(sp.data$PO1), c("data", "constants"))
  expect_equal(ncol(sp.data$PO1$data$Xw1), 0)
  
  ## mess around with covariates (CONUS extent) ----
  allfiles$PO.extent <- "CONUS"
  
  sp.data <- sppdata_for_nimble(species.data,
                                region,
                                file.info = allfiles,
                                covar = covariates,
                                covs.inat = "prec",
                                covs.PO = "temp",
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 1)
  expect_equal(names(sp.data), "PO1")
  expect_equal(names(sp.data$PO1), c("data", "constants"))
  expect_equal(colnames(sp.data$PO1$data$Xw1), c("temp"))
  
  
  
  ## works! with state extent ----
  allfiles$PO.extent <- "PA"
  
  sp.data <- sppdata_for_nimble(species.data,
                                region,
                                file.info = allfiles,
                                covar = covariates,
                                covs.inat = "prec",
                                covs.PO = "temp",
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)
  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 1)
  expect_equal(names(sp.data), "PO1")
  expect_equal(names(sp.data$PO1), c("data", "constants"))
  expect_equal(colnames(sp.data$PO1$data$Xw1), c("temp"))
  
  
  ## works! with two different state extents ----
  
  allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO"),
                         file.label = c("test1", "test2"),
                         covar.mean = c(NA, NA),
                         covar.sum = c(NA, NA),
                         data.type = c("PO", "PO"),
                         PO.extent = c("PA", "MA"))
  
  species.data <- load_species_data(sp.code = "GPOR",
                                    sp.code.all = "GPOR",
                                    file.info = allfiles,
                                    #file.path = "../../../species-futures/DATA SWAMP/data-ready-testfunctions/",
                                    file.path = "../species-futures/DATA SWAMP/data-ready-testfunctions/",
                                    region = region,
                                    filter.region = T,
                                    year.start = 1800,
                                    year.end = 2025,
                                    coordunc = 1000,
                                    coordunc_na.rm = T,
                                    spat.thin = F,
                                    keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  sp.data <- sppdata_for_nimble(species.data,
                                region,
                                file.info = allfiles,
                                covar = covariates,
                                covs.inat = "prec",
                                covs.PO = NA,
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 1)
  expect_equal(names(sp.data), "PO1")
  expect_equal(names(sp.data$PO1), c("data", "constants"))
  expect_equal(colnames(sp.data$PO1$data$Xw1), c("MA"))
  
  
  # PO data - iNat and other ----
  allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO"),
                         file.label = c("iNaturalist", "test2"),
                         covar.mean = c(NA, NA),
                         covar.sum = c(NA, NA),
                         data.type = c("PO", "PO"),
                         PO.extent = c("CONUS", "MA"))
  
  species.data <- load_species_data(sp.code = "GPOR",
                                    sp.code.all = "GPOR",
                                    file.info = allfiles,
                                    #file.path = "../../../species-futures/DATA SWAMP/data-ready-testfunctions/",
                                    file.path = "../species-futures/DATA SWAMP/data-ready-testfunctions/",
                                    region = region,
                                    filter.region = T,
                                    year.start = 1800,
                                    year.end = 2025,
                                    coordunc = 1000,
                                    coordunc_na.rm = T,
                                    spat.thin = F,
                                    keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  sp.data <- sppdata_for_nimble(species.data,
                                region,
                                file.info = allfiles,
                                covar = covariates,
                                covs.inat = "prec",
                                covs.PO = NA,
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 2)
  expect_equal(names(sp.data), c("PO1", "PO2"))
  expect_equal(names(sp.data$PO1), c("data", "constants"))
  expect_equal(colnames(sp.data$PO1$data$Xw1), c("intercept", "prec"))
  expect_equal(colnames(sp.data$PO1$data$Xw2), NULL)
  
})
