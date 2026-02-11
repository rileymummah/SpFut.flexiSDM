test_that("PO_for_nimble() works", {

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

  allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO", "Dodd_test_DND", "Dodd_test1_DND", "NEARMI_test_count"),
                         file.label = c("iNat_test", "iNat_test", "Dodd_test", "Dodd_test1", "NEARMI_test"),
                         covar.mean = c(NA, NA, "elevation", "elevation", "depth"),
                         covar.sum = c(NA, NA, "time", "time", "EffectValue"),
                         data.type = c("PO", "PO", "DND", "DND", "count"),
                         PO.extent = c("CONUS", "CONUS", NA, NA, NA))

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
                                    statelines.rm = F,
                                    keep.conus.grid.id = region$sp.grid$conus.grid.id)

  covariates <- data.frame(conus.grid.id = region$sp.grid$conus.grid.id,
                           temp = rnorm(nrow(region$sp.grid), 0, 1),
                           prec = rnorm(nrow(region$sp.grid), 0, 1) + runif(nrow(region$sp.grid), 0, 1))

  # test raw PO ----
  POdata <- species.data$obs$iNat_test
  expect_error(PO_for_nimble(POdata, covariates,
                            rename = 1))

  # test unordered real PO ----
  # Aggregate records to grid.ids
  POdata <- POdata %>%

    # aggregate to grid cell
    group_by(conus.grid.id) %>%
    summarize(count = n()) %>%

    # add in the rest of grid.ids and fill 0s if there were no records (NA)
    full_join(select(covariates, conus.grid.id), by = "conus.grid.id") %>%
    mutate(count = case_when(is.na(count) ~ 0,
                             T ~ count)) %>%
    filter(is.na(conus.grid.id) == F,
           conus.grid.id %in% region$sp.grid$conus.grid.id) # get rid of cells that we're excluding

  expect_error(PO_for_nimble(POdata, covariates,
                             rename = 1))

  # test ordered real PO ----
  POdata <- POdata[order(match(POdata$conus.grid.id, region$sp.grid$conus.grid.id)),]
  covariates <- covariates[order(match(covariates$conus.grid.id, region$sp.grid$conus.grid.id)),]
  PO <- PO_for_nimble(POdata, covariates,
                             rename = 1)

  # structure
  expect_type(PO, "list")
  expect_equal(length(PO), 2)

  # content
  expect_equal(nrow(PO$data$Xw1), length(PO$data$W1))
  expect_equal(nrow(PO$data$Xw1), PO$constants$nW1)
  expect_equal(nrow(PO$data$Xw1), length(PO$constants$Wcells1))
  expect_equal(ncol(PO$data$Xw1), PO$constants$nCovW1)
})
