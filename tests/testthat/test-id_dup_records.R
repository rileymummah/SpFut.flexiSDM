test_that("id_dup_records() works", {


    ## set up ----

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


    # one PO, nothing happens ----
    allfiles <- data.frame(file.name = c("iNat_test_PO"),
                           file.label = c("iNaturalist"),
                           covar.mean = c(NA),
                           covar.sum = c(NA),
                           data.type = c("PO"),
                           PO.extent = "CONUS")

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

  out <- id_dup_records(species.data)
  expect_equal(length(out), 0)


  # two POs without overlapping data, nothing happens ----
  allfiles <- data.frame(file.name = c("iNat_test2_PO", "iNat_test3_PO"),
                         file.label = c("iNaturalist", "iNat-other"),
                         covar.mean = c(NA),
                         covar.sum = c(NA),
                         data.type = c("PO"),
                         PO.extent = c("CONUS", "CONUS"))

  species.data <- load_species_data(sp.code = "GPOR",
                                    sp.code.all = c("GPOR", "GPORPO"),
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

  out <- id_dup_records(species.data)
  expect_equal(length(out), 0)


  # two POs with overlapping data, they are identified ----
  allfiles <- data.frame(file.name = c("iNat_test1_PO", "iNat_test2_PO"),
                         file.label = c("iNaturalist", "iNat-other"),
                         covar.mean = c(NA, NA),
                         covar.sum = c(NA, NA),
                         data.type = c("PO", "PO"),
                         PO.extent = c("CONUS", "CONUS"))

  species.data <- load_species_data(sp.code = "GPOR",
                                    sp.code.all = c("GPOR", "GPORPO"),
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

  out <- id_dup_records(species.data)
  expect_equal(length(out), 10)

})
