test_that("sppdata_for_nimble() works with PO data", {

  # set up ----
  st.map <- rnaturalearth::ne_states(country = c("Canada", "Mexico", "United States of America"),
                                     returnclass = "sf")


  rangelist <- get_range(range.path = c(paste0("~/GitHub/species-futures/data/species/GPOR/GAP/"),
                                        paste0("~/GitHub/species-futures/data/species/GPOR/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)

  boundary <- rangelist[[1]]
  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.)) %>%
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

  stategrid <- get_state_grid(region, st.map)

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


  sp.data <- sppdata_for_nimble(species.data,
                                region,
                                file.info = allfiles,
                                covar = covariates,
                                covs.inat = NA, # no effort covariate
                                covs.PO = c("temp", "prec"),
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)

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
                                stategrid = stategrid,
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

  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 2)
  expect_equal(names(sp.data), c("PO1", "PO2"))
  expect_equal(names(sp.data$PO1), c("data", "constants"))
  expect_equal(colnames(sp.data$PO1$data$Xw1), c("intercept", "prec"))
  expect_equal(colnames(sp.data$PO1$data$Xw2), NULL)

})

test_that("sppdata_for_nimble() works with DND data", {

  # set up ----
  rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
                                        paste0("../../../species-futures/data/species/GPOR/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)

  boundary <- rangelist[[1]]
  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.)) %>%
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

  stategrid <- get_state_grid(region, st.map)

  # DND data ----

  ## works! ----
  allfiles <- data.frame(file.name = c("Dodd_test1_DND"),
                         file.label = c("Dodd"),
                         covar.mean = c(NA),
                         covar.sum = c(NA),
                         data.type = c("DND"))

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
                                stategrid = stategrid,
                                covs.inat = "prec",
                                covs.PO = NA,
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)


  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 1)
  expect_equal(names(sp.data), "DND1")
  expect_equal(names(sp.data$DND1), c("data", "constants"))
  expect_equal(ncol(sp.data$DND1$data$Xv1), 0)

  ## mess around with covariates ----
  allfiles <- data.frame(file.name = c("Dodd_test1_DND"),
                         file.label = c("Dodd"),
                         covar.mean = c("elevation"),
                         covar.sum = c("time"),
                         data.type = c("DND"))

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
                                stategrid = stategrid,
                                covs.inat = "", # no effort covariate
                                covs.PO = "",
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)

  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 1)
  expect_equal(names(sp.data), "DND1")
  expect_equal(names(sp.data$DND1), c("data", "constants"))
  expect_equal(colnames(sp.data$DND1$data$Xv1), c("elevation", "time"))

  allfiles <- data.frame(file.name = c("Dodd_test1_DND"),
                         file.label = c("Dodd"),
                         covar.mean = c("notacovariate"),
                         covar.sum = c("time"),
                         data.type = c("DND"))

  expect_warning(species.data <- load_species_data(sp.code = "GPOR",
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
                                    keep.conus.grid.id = region$sp.grid$conus.grid.id))

  expect_warning(sp.data <- sppdata_for_nimble(species.data,
                                               region,
                                               file.info = allfiles,
                                               covar = covariates,
                                               stategrid = stategrid,
                                               covs.inat = "", # no effort covariate
                                               covs.PO = "",
                                               DND.maybe = 1,
                                               keep.conus.grid.id = region$sp.grid$conus.grid.id))

  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 1)
  expect_equal(names(sp.data), "DND1")
  expect_equal(names(sp.data$DND1), c("data", "constants"))
  expect_equal(colnames(sp.data$DND1$data$Xv1), c("time"))

})


test_that("sppdata_for_nimble() works with count data", {

  # set up ----
  rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
                                        paste0("../../../species-futures/data/species/GPOR/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)

  boundary <- rangelist[[1]]
  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.)) %>%
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

  stategrid <- get_state_grid(region, st.map)

  # count data ----

  ## works! ----
  allfiles <- data.frame(file.name = c("NEARMI_test_count"),
                         file.label = c("NEARMI"),
                         covar.mean = c(NA),
                         covar.sum = c(NA),
                         data.type = c("count"))

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
                                stategrid = stategrid,
                                covs.inat = "prec",
                                covs.PO = NA,
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)


  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 1)
  expect_equal(names(sp.data), "count1")
  expect_equal(names(sp.data$count1), c("data", "constants"))
  expect_equal(ncol(sp.data$count1$data$Xy1), 0)

  ## mess around with covariates ----
  allfiles <- data.frame(file.name = c("NEARMI_test_count"),
                         file.label = c("NEARMI"),
                         covar.mean = c("depth"),
                         covar.sum = c("EffectValue"),
                         data.type = c("count"))

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
                                stategrid = stategrid,
                                covs.inat = "", # no effort covariate
                                covs.PO = "",
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)

  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 1)
  expect_equal(names(sp.data), "count1")
  expect_equal(names(sp.data$count1), c("data", "constants"))
  expect_equal(colnames(sp.data$count1$data$Xy1), c("depth", "EffectValue"))



})



test_that("sppdata_for_nimble() works with all data", {

  # set up ----
  rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
                                        paste0("../../../species-futures/data/species/GPOR/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)

  boundary <- rangelist[[1]]
  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.)) %>%
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

  stategrid <- get_state_grid(region, st.map)


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
                                stategrid = stategrid,
                                covs.inat = "prec",
                                covs.PO = NA,
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)


  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 4)
  expect_equal(names(sp.data), c("PO1", "PO2", "DND3", "count4"))
  expect_equal(names(sp.data$count4), c("data", "constants"))
  expect_equal(ncol(sp.data$count4$data$Xy4), 0)

  # mess around with file names ----
  allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO", "Dodd_test_DND", "NEARMI_test_count"),
                         file.label = c("iNat_test", "iNat_test", "Dodd_test", "NEARMI_test"),
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
                                stategrid = stategrid,
                                covs.inat = "", # no effort covariate
                                covs.PO = "",
                                DND.maybe = 1,
                                keep.conus.grid.id = region$sp.grid$conus.grid.id)

  expect_type(sp.data, "list")
  expect_equal(length(sp.data), 4)
  expect_equal(names(sp.data), c("PO1", "PO2", "DND3", "count4"))
  expect_equal(names(sp.data$count4), c("data", "constants"))
  expect_equal(ncol(sp.data$count4$data$Xy4), 0)



})




test_that("sppdata_for_nimble() works with CV", {

  # set up ----
  rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
                                        paste0("../../../species-futures/data/species/GPOR/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)

  boundary <- rangelist[[1]]
  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.)) %>%
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

  st.map <- ne_states(country = c("Canada", "Mexico", "United States of America"),
                      returnclass = "sf")

  stategrid <- get_state_grid(region, st.map)


  # make CV blocks
  spatblocks <- make_CV_blocks(region, 5, 5, 3)

  block1 <- filter(spatblocks, folds == 2)

  # find grid.ids for test block, everything else is train
  suppressWarnings(test.i <- st_intersection(region$sp.grid, block1) %>%
                     pull(conus.grid.id) %>%
                     unique())
  train.i <- filter(region$sp.grid, conus.grid.id %in% test.i == F) %>%
    pull(conus.grid.id)

  gridkey <- select(region$sp.grid, conus.grid.id) %>%
    st_drop_geometry() %>%
    mutate(grid.id = 1:nrow(.),
           group = case_when(conus.grid.id %in% train.i ~ "train",
                             conus.grid.id %in% test.i ~ "test"))


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
                                    keep.conus.grid.id = gridkey$conus.grid.id[which(gridkey$group == "train")])

  sp.data <- sppdata_for_nimble(species.data,
                                region,
                                file.info = allfiles,
                                covar = covariates,
                                stategrid = stategrid,
                                covs.inat = "prec",
                                covs.PO = NA,
                                DND.maybe = 1,
                                keep.conus.grid.id = gridkey$conus.grid.id[which(gridkey$group == "train")])

  tmp <- sp.data$PO1$constants$Wcells1[which(sp.data$PO1$constants$Wcells1 %in% gridkey$conus.grid.id[which(gridkey$group == "test")])]
  expect_equal(length(tmp), 0)
  tmp <- sp.data$PO2$constants$Wcells2[which(sp.data$PO2$constants$Wcells2 %in% gridkey$conus.grid.id[which(gridkey$group == "test")])]
  expect_equal(length(tmp), 0)
  tmp <- sp.data$DND3$constants$Vcells3[which(sp.data$PO2$constants$Vcells3 %in% gridkey$conus.grid.id[which(gridkey$group == "test")])]
  expect_equal(length(tmp), 0)
  tmp <- sp.data$count4$constants$Ycells4[which(sp.data$PO2$constants$Ycells4 %in% gridkey$conus.grid.id[which(gridkey$group == "test")])]
  expect_equal(length(tmp), 0)


})
