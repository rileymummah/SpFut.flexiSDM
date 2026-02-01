
test_that("map_species_data() works with samples", {
  
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
  expect_doppelganger("samples with details", out$plot)
  
  
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
  expect_doppelganger("samples without details", out$plot)
  
  
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
  expect_doppelganger("samples with blocks", out$plot)
  
  
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
  expect_doppelganger("cv samples with blocks", out$plot)
  
})

