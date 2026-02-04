
test_that("map_species_data() works with plot = samples", {

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

  allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO", "Dodd_test_DND", "NEARMI_test_count"),
                         file.label = c("iNat_test", "iNat_test", "Dodd_test", "NEARMI_test"),
                         covar.mean = c(NA, NA, NA, NA),
                         covar.sum = c(NA, NA, NA, NA),
                         data.type = c("PO", "PO", "DND", "count"))

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

  # plot samples only, with details ----
  out <- map_species_data(region = region,
                          species.data = species.data,
                          year.start = 1800,
                          year.end = 2025,
                          plot = "samples",
                          plot.region = T,
                          details = T,
                          title = "TEST MAP")

  # output format
  expect_type(out, "list")
  expect_equal(length(out), 2)
  expect_true(is_ggplot(out$plot))
  expect_s3_class(out$dat, "sf")

  # output content
  expect_equal(nrow(out$dat), 35)
  expect_equal(ncol(out$dat), 15)
  vdiffr::expect_doppelganger("samples with details", out$plot)


  # plot samples only, without details ----
  out <- map_species_data(region = region,
                          species.data = species.data,
                          year.start = 1800,
                          year.end = 2025,
                          plot = "samples",
                          plot.region = T,
                          details = F,
                          title = "TEST MAP")

  # output format
  expect_type(out, "list")
  expect_equal(length(out), 2)
  expect_true(is_ggplot(out$plot))
  expect_s3_class(out$dat, "sf")

  # output content
  expect_equal(nrow(out$dat), 35)
  expect_equal(ncol(out$dat), 12)
  vdiffr::expect_doppelganger("samples without details", out$plot)


  # plot samples with blocks ----
  spatblocks <- make_CV_blocks(region, rows = 4, cols = 3, k = 3)
  out <- map_species_data(region = region,
                          species.data = species.data,
                          year.start = 1800,
                          year.end = 2025,
                          plot = "samples",
                          plot.region = T,
                          details = F,
                          title = "TEST MAP",
                          blocks = spatblocks)
  # output format
  expect_type(out, "list")
  expect_equal(length(out), 2)
  expect_true(is_ggplot(out$plot))
  expect_s3_class(out$dat, "sf")

  # output content
  expect_equal(nrow(out$dat), 35)
  expect_equal(ncol(out$dat), 12)
  vdiffr::expect_doppelganger("samples with blocks", out$plot)


  # leave out data from one block ----
  spatblocks <- make_CV_blocks(region, rows = 4, cols = 3, k = 3)

  # Set up training and test sets based on cross validation blocks
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

  out <- map_species_data(region = region,
                          species.data = species.data,
                          year.start = 1800,
                          year.end = 2025,
                          plot = "samples",
                          plot.region = T,
                          details = F,
                          title = "TEST MAP",
                          blocks = spatblocks)
  vdiffr::expect_doppelganger("cv samples with blocks", out$plot)

})

test_that("map_species_data() works with plot = lambda", {

  mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')

  # everything normal ----
  out <- map_species_data(title = "Lambda test", region = mod1$region, plot = "lambda",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = F,
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("lambda", out$plot)
  expect_equal(nrow(out$dat), 95)

  # add range and region ----
  out <- map_species_data(title = "Lambda test", region = mod1$region, plot = "lambda",
                          out = mod1$out,
                          plot.range = T, plot.region = T, plot.cells = F,
                          plot.current = T, plot.uncertainty = F,
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("lambda with region", out$plot)
  expect_equal(nrow(out$dat), 95)
  expect_equal(out$dat$value[c(1:41, 43:95)], mod1$out$lambda0$mean[c(1:41, 43:95)]) # because highest value is truncated

  # uncertainty ----
  out <- map_species_data(title = "Lambda test", region = mod1$region, plot = "lambda",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = "unc.rel",
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("lambda, relative uncertainty", out$plot)
  expect_equal(nrow(out$dat), 95)
  expect_equal(out$dat$value[c(1:15, 17:95)], mod1$out$lambda0$unc.rel[c(1:15, 17:95)]) # because highest value is truncated

  out <- map_species_data(title = "Lambda test", region = mod1$region, plot = "lambda",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = "unc.range",
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("lambda, absolute uncertainty", out$plot)
  expect_equal(nrow(out$dat), 95)
  expect_equal(out$dat$value[c(1:41, 43:95)], mod1$out$lambda0$unc.range[c(1:41, 43:95)]) # because highest value is truncated

  # transform ----
  out <- map_species_data(title = "Lambda test", region = mod1$region, plot = "lambda",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = "unc.rel",
                          transform = "log", plot.change = F)
  vdiffr::expect_doppelganger("lambda, relative uncertainty log", out$plot)
  expect_equal(nrow(out$dat), 95)
  expect_equal(out$dat$value[c(1:15, 17:95)], log(mod1$out$lambda0$unc.rel[c(1:15, 17:95)])) # because highest value is truncated

  out <- map_species_data(title = "Lambda test", region = mod1$region, plot = "lambda",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = "unc.range",
                          transform = "exp", plot.change = F)
  vdiffr::expect_doppelganger("lambda, absolute uncertainty exp", out$plot)
  expect_equal(nrow(out$dat), 95)
  expect_equal(out$dat$value[c(1:41, 43:95)], exp(mod1$out$lambda0$unc.range[c(1:41, 43:95)])) # because highest value is truncated

  out <- map_species_data(title = "Lambda test", region = mod1$region, plot = "lambda",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = F,
                          transform = "exp", plot.change = F)
  vdiffr::expect_doppelganger("lambda, mean exp", out$plot)
  expect_equal(nrow(out$dat), 95)
  expect_equal(out$dat$value[c(1:41, 43:95)], exp(mod1$out$lambda0$mean[c(1:41, 43:95)])) # because highest value is truncated


  mod4 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4.rds')

  # coarse grid ----
  out <- map_species_data(title = "Lambda test", region = mod4$region, plot = "lambda",
                          out = mod4$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = F,
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("lambda coarse", out$plot)
  expect_equal(nrow(out$dat), 473)
})



test_that("map_species_data() works with plot = psi and plot = threshold", {

  mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')

  # everything normal ----
  out <- map_species_data(title = "Psi test", region = mod1$region, plot = "psi",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = F,
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("psi", out$plot)
  expect_equal(nrow(out$dat), 95)
  expect_equal(out$dat$value[c(1:47, 49:95)], mod1$out$psi0$mean[c(1:47, 49:95)]) # because highest value is truncated
  expect_lte(max(out$dat$value), 1)
  expect_lte(0, min(out$dat$value))

  # uncertainty ----
  out <- map_species_data(title = "Psi test", region = mod1$region, plot = "psi",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = "unc.rel",
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("psi, relative uncertainty", out$plot)
  expect_equal(nrow(out$dat), 95)
  expect_equal(out$dat$value[c(1:83, 85:95)], mod1$out$psi0$unc.rel[c(1:83, 85:95)]) # because highest value is truncated

  out <- map_species_data(title = "Psi test", region = mod1$region, plot = "psi",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = "unc.range",
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("psi, absolute uncertainty", out$plot)
  expect_equal(nrow(out$dat), 95)
  expect_equal(out$dat$value[c(1:15, 17:95)], mod1$out$psi0$unc.range[c(1:15, 17:95)]) # because highest value is truncated

  # coarse grid ----
  out <- map_species_data(title = "Psi test", region = mod4$region, plot = "psi",
                          out = mod4$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = F,
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("psi coarse", out$plot)
  expect_equal(nrow(out$dat), 473)

  # boundary ----
  out <- map_species_data(title = "Boundary test", region = mod1$region, plot = "boundary",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = F,
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("threshold 0.5", out$plot)
  expect_equal(nrow(out$dat), 95)

  test <- nrow(out$dat[which(out$dat$value == "Present"),]) + nrow(out$dat[which(out$dat$value == "Absent"),])
  expect_equal(test, 95)


  out <- map_species_data(title = "Boundary test", region = mod1$region, plot = "boundary",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = F,
                          transform = "none", plot.change = F,
                          threshold = 0.90)
  vdiffr::expect_doppelganger("threshold 0.9", out$plot)
  expect_equal(nrow(out$dat), 95)

  test <- nrow(out$dat[which(out$dat$value == "Present"),]) + nrow(out$dat[which(out$dat$value == "Absent"),])
  expect_equal(test, 95)

  # coarse grid ----

  mod4 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4.rds')

  out <- map_species_data(title = "Boundary test", region = mod4$region, plot = "boundary",
                          out = mod4$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = F,
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("threshold coarse", out$plot)
  expect_equal(nrow(out$dat), 473)
})



test_that("map_species_data() works with plot = spat", {

  mod4 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4.rds')

  # everything normal ----
  out <- map_species_data(title = "Spatial test", region = mod4$region, plot = "spat",
                          out = mod4$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = F,
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("spatial", out$plot)
  expect_equal(nrow(out$dat), 90)


  # no spatial effect ----
  mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')
  expect_error(out <- map_species_data(title = "Spatial test", region = mod1$region, plot = "spat",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = F,
                          transform = "none", plot.change = F))
})


test_that("map_species_data() works with plot = XB", {

  mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')

  # everything normal ----
  out <- map_species_data(title = "XB test", region = mod1$region, plot = "XB",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = F,
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("xb", out$plot)
  expect_equal(nrow(out$dat), 95)


  # uncertainty ----
  out <- map_species_data(title = "XB test", region = mod1$region, plot = "XB",
                          out = mod1$out,
                          plot.range = F, plot.region = F, plot.cells = F,
                          plot.current = T, plot.uncertainty = "unc.rel",
                          transform = "none", plot.change = F)
  vdiffr::expect_doppelganger("xb relative uncertainty", out$plot)
  expect_equal(nrow(out$dat), 95)


})


test_that("map_species_data() works with plot = effort", {




})
