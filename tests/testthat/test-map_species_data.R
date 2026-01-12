test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


test_that("load_species_data() works with iNat format data", {

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

    allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO", "Dodd_test_DND", "NEARMI_test_count"),
                           file.label = c("iNat_test", "iNat_test", "Dodd_test", "NEARMI_test"),
                           covar.mean = c(NA, NA, NA, NA),
                           covar.sum = c(NA, NA, NA, NA),
                           data.type = c("PO", "PO", "DND", "count"))

    species.data <- load_species_data(sp.code = "GPOR",
                                      sp.code.all = "GPOR",
                                      file.info = allfiles,
                                      file.path = "../../../species-futures/DATA SWAMP/data-ready-testfunctions/",
                                      #file.path = "../species-futures/DATA SWAMP/data-ready-testfunctions/",
                                      region = region,
                                      filter.region = T,
                                      year.start = 1800,
                                      year.end = 2025,
                                      coordunc = 1000,
                                      coordunc_na.rm = T,
                                      spat.thin = F,
                                      keep.conus.grid.id = region$sp.grid$conus.grid.id)

    # plot samples only ----
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
    
    
    
    
    
})

