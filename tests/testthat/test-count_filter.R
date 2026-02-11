test_that("count_filter() works", {


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
                         covar.mean = c(NA, NA, "", "", "depth"),
                         covar.sum = c(NA, NA, "time", "time", "EffectValue"),
                         data.type = c("PO", "PO", "DND", "DND", "count"),
                         PO.extent = c("CONUS", "PA", NA, NA, NA))

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


  # only 1 pass ----
  data <- species.data$obs$NEARMI_test %>%
    filter(pass.id == 2)
  countstart <- count_filter(data,
                             covs.mean = "",
                             covs.sum = "EffectValue",
                             offset.area = NULL)

  expect_s3_class(countstart, "data.frame")
  expect_equal(nrow(countstart), 7)
  expect_equal(max(countstart$npass), 1)
  expect_equal(max(countstart$count), 4)



  # 3 passes ----
  data <- species.data$obs$NEARMI_test
  countstart <- count_filter(data,
                             covs.mean = "",
                             covs.sum = "EffectValue",
                             offset.area = NULL)

  expect_s3_class(countstart, "data.frame")
  expect_equal(nrow(countstart), 7)
  expect_equal(max(countstart$npass), 3)
  expect_equal(max(countstart$count), 18)


  # filter age classes ----
  data <- species.data$obs$NEARMI_test
  countstart <- count_filter(data,
                             covs.mean = "",
                             covs.sum = "EffectValue",
                             offset.area = NULL,
                             age.use = "adult")

  expect_s3_class(countstart, "data.frame")
  expect_equal(nrow(countstart), 7)
  expect_equal(max(countstart$npass), 3)
  expect_equal(max(countstart$count), 3)

  data <- species.data$obs$NEARMI_test
  countstart <- count_filter(data,
                             covs.mean = "",
                             covs.sum = "EffectValue",
                             offset.area = NULL,
                             age.use = "notanageclass")

  expect_s3_class(countstart, "data.frame")
  expect_equal(nrow(countstart), 0)


  # different covars ----
  data <- species.data$obs$NEARMI_test
  countstart <- count_filter(data,
                             covs.mean = "depth",
                             covs.sum = "EffectValue",
                             offset.area = NULL)

  expect_s3_class(countstart, "data.frame")
  expect_equal(nrow(countstart), 7)
  expect_equal(max(countstart$npass), 3)
  expect_equal(max(countstart$count), 18)
  expect_true("depth" %in% colnames(countstart))
  expect_true("EffectValue" %in% colnames(countstart))

})
