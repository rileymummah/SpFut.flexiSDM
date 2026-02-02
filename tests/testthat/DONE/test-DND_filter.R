test_that("DND_filter() works", {


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
                         covar.mean = c(NA, NA, NA, "elevation", NA),
                         covar.sum = c(NA, NA, "time", "time", NA),
                         data.type = c("PO", "PO", "DND", "DND", "count"))

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

  # only 1 pass ----
  data <- species.data$obs$Dodd_test
  dndstart <- DND_filter(data,
                         covs.mean = "",
                         covs.sum = "time",
                         DND.maybe = 1,
                         offset.area = NULL)

  expect_s3_class(dndstart, "data.frame")
  expect_equal(nrow(dndstart), 5)
  expect_equal(max(dndstart$npass), 1)

  # 2 passes in 1 survey ----
  data <- species.data$obs$Dodd_test1
  dndstart1 <- DND_filter(data,
                          covs.mean = "",
                          covs.sum = "time",
                          DND.maybe = 1,
                          offset.area = NULL)

  expect_s3_class(dndstart1, "data.frame")
  expect_equal(nrow(dndstart1), 5)
  expect_equal(max(dndstart1$npass), 2)


  # filter age classes ----
  data <- species.data$obs$Dodd_test1
  dndstart1 <- DND_filter(data,
                          covs.mean = "",
                          covs.sum = "time",
                          DND.maybe = 1,
                          offset.area = NULL,
                          age.use = "adult")

  expect_s3_class(dndstart1, "data.frame")
  expect_equal(nrow(dndstart1), 5)
  expect_equal(max(dndstart1$count), 1)

  dndstart1 <- DND_filter(data,
                          covs.mean = "",
                          covs.sum = "time",
                          DND.maybe = 1,
                          offset.area = NULL,
                          age.use = "larva")

  expect_s3_class(dndstart1, "data.frame")
  expect_equal(nrow(dndstart1), 5)
  expect_equal(max(dndstart1$count), 0)


  dndstart1 <- DND_filter(data,
                          covs.mean = "",
                          covs.sum = "time",
                          DND.maybe = 1,
                          offset.area = NULL,
                          age.use = "notanageclass")

  expect_s3_class(dndstart1, "data.frame")
  expect_equal(nrow(dndstart1), 0)


  # maybe detections ----
  data <- species.data$obs$Dodd_test1
  dndstart <- DND_filter(data,
                         covs.mean = "",
                         covs.sum = "time",
                         DND.maybe = 0,
                         offset.area = NULL)

  expect_s3_class(dndstart, "data.frame")
  expect_equal(nrow(dndstart), 5)
  expect_equal(sum(dndstart$count), 1)

  data <- species.data$obs$Dodd_test1
  dndstart1 <- DND_filter(data,
                          covs.mean = "",
                          covs.sum = "time",
                          DND.maybe = 1,
                          offset.area = NULL)

  expect_s3_class(dndstart1, "data.frame")
  expect_equal(nrow(dndstart1), 5)
  expect_equal(sum(dndstart1$count), 3)


  # different covars ----
  data <- species.data$obs$Dodd_test1
  dndstart <- DND_filter(data,
                         covs.mean = "elevation",
                         covs.sum = "time",
                         offset.area = NULL,
                         DND.maybe = 1)

  expect_s3_class(dndstart, "data.frame")
  expect_equal(nrow(dndstart), 5)
  expect_equal(max(dndstart$npass), 2)
  expect_equal(max(dndstart$count), 1)
  expect_true("elevation" %in% colnames(dndstart))
  expect_true("time" %in% colnames(dndstart))
})
