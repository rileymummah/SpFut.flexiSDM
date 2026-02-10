test_that("data_for_nimble() works", {


  # set up ----
  rangelist <- get_range(range.path = c(paste0("~/GitHub/species-futures/data/species/GPOR/GAP/"),
                                        paste0("~/GitHub/species-futures/data/species/GPOR/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)

  boundary <- rangelist[[1]]
  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>%
          st_as_sf() %>%
          mutate(conus.grid.id = 1:nrow(.)) %>%
          rename(geometry = x)

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


  # works! ----
  allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO", "Dodd_test_DND", "NEARMI_test_count"),
                         file.label = c("iNat_test", "iNat_test1", "Dodd_test", "NEARMI_test"),
                         covar.mean = c(NA, NA, NA, NA),
                         covar.sum = c(NA, NA, NA, NA),
                         data.type = c("PO", "PO", "DND", "count"),
                         PO.extent = c("CONUS", "PA", NA, NA))

  species.data <- load_species_data(sp.code = "GPOR",
                                    sp.code.all = "GPOR",
                                    file.info = allfiles,
                                    file.path = "~/GitHub/species-futures/pkg-tests/",
                                    #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
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

  gridkey <- select(region$sp.grid, conus.grid.id) %>%
              st_drop_geometry() %>%
              mutate(grid.id = 1:nrow(.),
                     group = "train")

  tmp <- data_for_nimble(sp.data,
                         covar = covariates,
                         covs.z = c("temp", "prec"),
                         sp.auto = F,
                         coarse.grid = F,
                         region = region,
                         process.intercept = F,
                         gridkey = gridkey,
                         spatRegion= spatRegion)

  data <- tmp$data
  constants <- tmp$constants

  expect_equal(setdiff(allfiles$file.label, constants), character(0))

  expect_equal(length(constants$Wcells1), nrow(data$Xw1))
  expect_equal(length(constants$Wcells2), nrow(data$Xw2))
  expect_equal(length(constants$Vcells3), nrow(data$Xv3))
  expect_equal(length(constants$Ycells4), nrow(data$Xy4))

  expect_equal(length(constants$Wcells1), constants$nW1)
  expect_equal(length(constants$Wcells2), constants$nW2)
  expect_equal(length(constants$Vcells3), constants$nV3)
  expect_equal(length(constants$Ycells4), constants$nY4)

  expect_equal(constants$nCovW1, ncol(data$Xw1))
  expect_equal(constants$nCovW2, ncol(data$Xw2))
  expect_equal(constants$nCovV3, ncol(data$Xv3))
  expect_equal(constants$nCovY4, ncol(data$Xy4))


  # works with sp.auto = T ----
  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000, square = F) %>%
          st_as_sf() %>%
          mutate(conus.grid.id = 1:nrow(.)) %>%
          rename(geometry = x)

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


  allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO", "Dodd_test_DND", "NEARMI_test_count"),
                         file.label = c("iNat_test", "iNat_test1", "Dodd_test", "NEARMI_test"),
                         covar.mean = c(NA, NA, NA, NA),
                         covar.sum = c(NA, NA, NA, NA),
                         data.type = c("PO", "PO", "DND", "count"),
                         PO.extent = c("CONUS", "PA", NA, NA))

  species.data <- load_species_data(sp.code = "GPOR",
                                    sp.code.all = "GPOR",
                                    file.info = allfiles,
                                    file.path = "~/GitHub/species-futures/pkg-tests/",
                                    #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
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

  gridkey <- select(region$sp.grid, conus.grid.id) %>%
    st_drop_geometry() %>%
    mutate(grid.id = 1:nrow(.),
           group = "train")

  ## coarse.grid == F ----
  tmp <- data_for_nimble(sp.data,
                         covar = covariates,
                         covs.z = c("temp", "prec"),
                         sp.auto = T,
                         coarse.grid = F,
                         region = region,
                         process.intercept = F,
                         gridkey = gridkey,
                         spatRegion= spatRegion)

  data <- tmp$data
  constants <- tmp$constants

  expect_equal(setdiff(allfiles$file.label, constants), character(0))

  expect_equal(length(constants$Wcells1), nrow(data$Xw1))
  expect_equal(length(constants$Wcells2), nrow(data$Xw2))
  expect_equal(length(constants$Vcells3), nrow(data$Xv3))
  expect_equal(length(constants$Ycells4), nrow(data$Xy4))

  expect_equal(length(constants$Wcells1), constants$nW1)
  expect_equal(length(constants$Wcells2), constants$nW2)
  expect_equal(length(constants$Vcells3), constants$nV3)
  expect_equal(length(constants$Ycells4), constants$nY4)

  expect_equal(constants$nCovW1, ncol(data$Xw1))
  expect_equal(constants$nCovW2, ncol(data$Xw2))
  expect_equal(constants$nCovV3, ncol(data$Xv3))
  expect_equal(constants$nCovY4, ncol(data$Xy4))

  expect_equal(max(constants$adj), constants$nCell)

  ## coarse.grid == T ----
  spatRegion <- suppressWarnings(make_spatkey(region$sp.grid))

  tmp <- data_for_nimble(sp.data,
                         covar = covariates,
                         covs.z = c("temp", "prec"),
                         sp.auto = T,
                         coarse.grid = T,
                         region = region,
                         process.intercept = F,
                         gridkey = gridkey,
                         spatRegion= spatRegion)

  data <- tmp$data
  constants <- tmp$constants

  expect_equal(setdiff(allfiles$file.label, constants), character(0))

  expect_equal(length(constants$Wcells1), nrow(data$Xw1))
  expect_equal(length(constants$Wcells2), nrow(data$Xw2))
  expect_equal(length(constants$Vcells3), nrow(data$Xv3))
  expect_equal(length(constants$Ycells4), nrow(data$Xy4))

  expect_equal(length(constants$Wcells1), constants$nW1)
  expect_equal(length(constants$Wcells2), constants$nW2)
  expect_equal(length(constants$Vcells3), constants$nV3)
  expect_equal(length(constants$Ycells4), constants$nY4)

  expect_equal(constants$nCovW1, ncol(data$Xw1))
  expect_equal(constants$nCovW2, ncol(data$Xw2))
  expect_equal(constants$nCovV3, ncol(data$Xv3))
  expect_equal(constants$nCovY4, ncol(data$Xy4))

  expect_equal(max(constants$spatCells), constants$nSpatCell)
  expect_equal(max(constants$adj), constants$nSpatCell)
})
