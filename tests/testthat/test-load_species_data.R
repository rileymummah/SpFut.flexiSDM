
test_that("load_species_data() works", {

    rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/RACA/GAP/"),
                                    paste0("../../../species-futures/data/species/RACA/IUCN/")),
                     range.name = c("GAP", "IUCN"), crs = 4326)

    boundary <- rangelist[[1]]
    grid <- st_make_grid(boundary) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))

    region <- make_region(rangelist,
                          buffer = 50,
                          sub = F,
                          boundary = boundary,
                          grid = grid,
                          rm.clumps = T,
                          clump.size = 2,
                          continuous = F)

    allfiles <- data.frame(file.name = c("RACA_NRIS_PO_1901_2022"),
                           file.label = c("RACA_NRIS"),
                           covar.mean = "",
                           covar.sum = "NA",
                           data.type = c("PO"))

    expect_true(file.exists('../../../species-futures/DATA SWAMP/data-ready/'))

    species.data <- load_species_data(sp.code = "RACA",
                                      sp.code.all = "RACA",
                                      file.info = allfiles,
                                      file.path = "../../../species-futures/DATA SWAMP/data-ready/",
                                      region = region,
                                      filter.region = T,
                                      year.start = 1900,
                                      year.end = 2025,
                                      coordunc = 1000,
                                      coordunc_na.rm = T,
                                      spat.thin = F,
                                      keep.conus.grid.id = region$sp.grid$conus.grid.id)

    expect_type(species.data, "list")
    expect_equal(length(species.data), 2)
    expect_equal(names(species.data), c("locs", "obs"))
    expect_type(species.data$locs, "list")
    expect_type(species.data$obs, "list")
    
})



test_that("load_species_data() works with two species codes", {
  
  rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/RACA/GAP/"),
                                        paste0("../../../species-futures/data/species/RACA/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)
  
  boundary <- rangelist[[1]]
  grid <- st_make_grid(boundary) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))
  
  region <- make_region(rangelist,
                        buffer = 50,
                        sub = F,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = T,
                        clump.size = 2,
                        continuous = F)
  
  allfiles <- data.frame(file.name = c("RACA_USGS_RHCA_count_2022_2022"),
                         file.label = c("RCHA"),
                         covar.mean = "",
                         covar.sum = "",
                         data.type = c("PO"))
  
  expect_true(file.exists('../../../species-futures/DATA SWAMP/data-ready/'))
  
  species.data <- load_species_data(sp.code = "RACA",
                                    sp.code.all = c("RACA", "RHCA"),
                                    file.info = allfiles,
                                    file.path = "../../../species-futures/DATA SWAMP/data-ready/",
                                    region = region,
                                    filter.region = T,
                                    year.start = 1900,
                                    year.end = 2025,
                                    coordunc = 1000,
                                    coordunc_na.rm = T,
                                    spat.thin = F,
                                    keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  expect_type(species.data, "list")
  expect_equal(length(species.data), 2)
  expect_equal(names(species.data), c("locs", "obs"))
  expect_type(species.data$locs, "list")
  expect_type(species.data$obs, "list")
  
  
  species.data <- load_species_data(sp.code = "RACA",
                                    sp.code.all = "RACA|RHCA",
                                    file.info = allfiles,
                                    file.path = "../../../species-futures/DATA SWAMP/data-ready/",
                                    region = region,
                                    filter.region = T,
                                    year.start = 1900,
                                    year.end = 2025,
                                    coordunc = 1000,
                                    coordunc_na.rm = T,
                                    spat.thin = F,
                                    keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  expect_type(species.data, "list")
  expect_equal(length(species.data), 2)
  expect_equal(names(species.data), c("locs", "obs"))
  expect_type(species.data$locs, "list")
  expect_type(species.data$obs, "list")
  
})


test_that("load_species_data() works with weird years", {
  
  rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/RACA/GAP/"),
                                        paste0("../../../species-futures/data/species/RACA/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)
  
  boundary <- rangelist[[1]]
  grid <- st_make_grid(boundary) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))
  
  region <- make_region(rangelist,
                        buffer = 50,
                        sub = F,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = T,
                        clump.size = 2,
                        continuous = F)
  
  allfiles <- data.frame(file.name = c("RACA_NRIS_PO_1901_2022"),
                         file.label = c("RACA_NRIS"),
                         covar.mean = "",
                         covar.sum = "",
                         data.type = c("PO"))
  
  expect_true(file.exists('../../../species-futures/DATA SWAMP/data-ready/'))
  
  expect_error(load_species_data(sp.code = "RACA",
                                    sp.code.all = "RACA",
                                    file.info = allfiles,
                                    file.path = "../../../species-futures/DATA SWAMP/data-ready/",
                                    region = region,
                                    filter.region = T,
                                    year.start = 2020,
                                    year.end = 2000,
                                    coordunc = 1000,
                                    coordunc_na.rm = T,
                                    spat.thin = F,
                                    keep.conus.grid.id = region$sp.grid$conus.grid.id))
  
  
  species.data <- load_species_data(sp.code = "RACA",
                                    sp.code.all = "RACA",
                                    file.info = allfiles,
                                    file.path = "../../../species-futures/DATA SWAMP/data-ready/",
                                    region = region,
                                    filter.region = T,
                                    year.start = 2024,
                                    year.end = 2024,
                                    coordunc = 1000,
                                    coordunc_na.rm = T,
                                    spat.thin = F,
                                    keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  expect_type(species.data, "list")
  expect_equal(length(species.data), 2)
  expect_equal(names(species.data), c("locs", "obs"))
  expect_type(species.data$locs, "list")
  expect_type(species.data$obs, "list")
  
})



test_that("load_species_data() works with multiple rows in file.info", {
  
  rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/RACA/GAP/"),
                                        paste0("../../../species-futures/data/species/RACA/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)
  
  boundary <- rangelist[[1]]
  grid <- st_make_grid(boundary) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))
  
  region <- make_region(rangelist,
                        buffer = 50,
                        sub = F,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = T,
                        clump.size = 2,
                        continuous = F)
  
  allfiles <- data.frame(file.name = c("RACA_NRIS_PO_1901_2022", "RACA_USGS_RHCA_count_2022_2022"),
                         file.label = c("RACA_NRIS", "RHCA"),
                         covar.mean = c("", ""),
                         covar.sum = c("", ""),
                         data.type = c("PO", "count"))
  
  expect_true(file.exists('../../../species-futures/DATA SWAMP/data-ready/'))
  
  species.data <- load_species_data(sp.code = "RACA",
                                    sp.code.all = "RACA",
                                    file.info = allfiles,
                                    file.path = "../../../species-futures/DATA SWAMP/data-ready/",
                                    region = region,
                                    filter.region = T,
                                    year.start = 1900,
                                    year.end = 2025,
                                    coordunc = 1000,
                                    coordunc_na.rm = T,
                                    spat.thin = F,
                                    keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  expect_type(species.data, "list")
  expect_equal(length(species.data), 2)
  expect_equal(names(species.data), c("locs", "obs"))
  expect_type(species.data$locs, "list")
  expect_type(species.data$obs, "list")
  
})


test_that("load_species_data() filter.region = F works", {
  
  rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/RACA/GAP/"),
                                        paste0("../../../species-futures/data/species/RACA/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)
  
  boundary <- rangelist[[1]]
  grid <- st_make_grid(boundary) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))
  
  region <- make_region(rangelist,
                        buffer = 50,
                        sub = F,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = T,
                        clump.size = 2,
                        continuous = F)
  
  allfiles <- data.frame(file.name = c("RACA_NRIS_PO_1901_2022", "RACA_USGS_RHCA_count_2022_2022"),
                         file.label = c("RACA_NRIS", "RHCA"),
                         covar.mean = c("", ""),
                         covar.sum = c("", ""),
                         data.type = c("PO", "count"))
  
  expect_true(file.exists('../../../species-futures/DATA SWAMP/data-ready/'))
  
  species.data <- load_species_data(sp.code = "RACA",
                                    sp.code.all = "RACA",
                                    file.info = allfiles,
                                    file.path = "../../../species-futures/DATA SWAMP/data-ready/",
                                    region = region,
                                    filter.region = F,
                                    year.start = 1900,
                                    year.end = 2025,
                                    coordunc = 1000,
                                    coordunc_na.rm = T,
                                    spat.thin = F,
                                    keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  expect_type(species.data, "list")
  expect_equal(length(species.data), 2)
  expect_equal(names(species.data), c("locs", "obs"))
  expect_type(species.data$locs, "list")
  expect_type(species.data$obs, "list")
  
})



test_that("load_species_data() coordunc = 10", {
  
  rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/RACA/GAP/"),
                                        paste0("../../../species-futures/data/species/RACA/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)
  
  boundary <- rangelist[[1]]
  grid <- st_make_grid(boundary) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))
  
  region <- make_region(rangelist,
                        buffer = 50,
                        sub = F,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = T,
                        clump.size = 2,
                        continuous = F)
  
  allfiles <- data.frame(file.name = c("RACA_NRIS_PO_1901_2022", "RACA_USGS_RHCA_count_2022_2022"),
                         file.label = c("RACA_NRIS", "RHCA"),
                         covar.mean = c("", ""),
                         covar.sum = c("", ""),
                         data.type = c("PO", "count"))
  
  expect_true(file.exists('../../../species-futures/DATA SWAMP/data-ready/'))
  
  species.data <- load_species_data(sp.code = "RACA",
                                    sp.code.all = "RACA",
                                    file.info = allfiles,
                                    file.path = "../../../species-futures/DATA SWAMP/data-ready/",
                                    region = region,
                                    filter.region = F,
                                    year.start = 1900,
                                    year.end = 2025,
                                    coordunc = 10,
                                    coordunc_na.rm = T,
                                    spat.thin = F,
                                    keep.conus.grid.id = region$sp.grid$conus.grid.id)
  
  expect_type(species.data, "list")
  expect_equal(length(species.data), 2)
  expect_equal(names(species.data), c("locs", "obs"))
  expect_type(species.data$locs, "list")
  expect_type(species.data$obs, "list")
  
  tmp <- bind_rows(species.data$obs) %>%
    filter(coordunc > 10)
  
  expect_equal(nrow(tmp), 0)
  
})
