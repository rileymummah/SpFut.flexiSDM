
test_that("make_region() works", {

  rangelist <- get_range(range.path = c(paste0("~/GitHub/species-futures/data/species/GPOR/GAP/"),
                                  paste0("~/GitHub/species-futures/data/species/GPOR/IUCN/")),
                   range.name = c("GAP", "IUCN"), crs = 4326)

  boundary <- rangelist[[1]]


  grid <- st_make_grid(boundary, cellsize = 100000) %>%
          st_as_sf() %>%
          mutate(conus.grid.id = 1:nrow(.)) %>%
          rename('geometry' = 'x')

  expect_error(make_region(rangelist,
                        buffer = 50,
                        sub = F,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = T,
                        clump.size = 2,
                        continuous = F))


  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>%
          st_as_sf() %>%
          mutate(conus.grid.id = 1:nrow(.)) %>%
          rename('geometry' = 'x')

  region <- make_region(rangelist,
                        buffer = 50,
                        sub = F,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = T,
                        clump.size = 2,
                        continuous = F)

  expect_type(region, "list")

  expect_equal(length(region), 4)

  expect_equal(names(region), c("range", "region", "sp.grid", "boundary"))

  expect_s3_class(region$range, "sf")
  expect_s3_class(region$region, "sf")
  expect_s3_class(region$sp.grid, "sf")
  expect_s3_class(region$boundary, "sf")

  expect_equal(nrow(region$sp.grid), 79)


  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000, square = F) %>%
            st_as_sf() %>%
            mutate(conus.grid.id = 1:nrow(.)) %>%
            rename('geometry' = 'x')

  tmp <- grid %>% mutate(area = as.numeric(st_area(.)))

  region <- make_region(rangelist,
                        buffer = 50,
                        sub = F,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = T,
                        clump.size = 2,
                        continuous = F)

  expect_type(region, "list")

  expect_equal(length(region), 4)

  expect_equal(names(region), c("range", "region", "sp.grid", "boundary"))

  expect_s3_class(region$range, "sf")
  expect_s3_class(region$region, "sf")
  expect_s3_class(region$sp.grid, "sf")
  expect_s3_class(region$boundary, "sf")

  expect_equal(nrow(region$sp.grid), 95)

})



# test_that("sub = T works", {
#
#   rangelist <- get_range(range.path = c(paste0("~/GitHub/species-futures/data/species/GPOR/GAP/"),
#                                         paste0("~/GitHub/species-futures/data/species/GPOR/IUCN/")),
#                          range.name = c("GAP", "IUCN"), crs = 4326)
#
#   boundary <- rangelist[[1]]
#
#
#   grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>%
#           st_as_sf() %>%
#           mutate(conus.grid.id = 1:nrow(.))  %>%
#           rename('geometry' = 'x')
#
#   expect_error(make_region(rangelist,
#                            buffer = 50,
#                            sub = T,
#                            boundary = boundary,
#                            grid = grid,
#                            rm.clumps = T,
#                            clump.size = 2,
#                            continuous = F))
#
#   grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>%
#           st_as_sf() %>%
#           mutate(conus.grid.id = 1:nrow(.)) %>%
#           rename('geometry' = 'x')
#
#   region <- make_region(rangelist,
#                         buffer = 500000,
#                         sub = T,
#                         boundary = boundary,
#                         grid = grid,
#                         rm.clumps = F,
#                         clump.size = 2,
#                         continuous = F)
#
#   expect_type(region, "list")
#
#   expect_equal(length(region), 4)
#
#   expect_equal(names(region), c("range", "region", "sp.grid", "boundary"))
#
#   expect_s3_class(region$range, "sf")
#   expect_s3_class(region$region, "sf")
#   expect_s3_class(region$sp.grid, "sf")
#   expect_s3_class(region$boundary, "sf")
#
#   expect_equal(nrow(region$sp.grid), 34)
#
#
# })


test_that("rm.clumps works", {

  rangelist <- get_range(range.path = c(paste0("~/GitHub/species-futures/data/species/GPOR/GAP/"),
                                        paste0("~/GitHub/species-futures/data/species/GPOR/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)

  boundary <- rangelist[[1]]
  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 10000) %>%
          st_as_sf() %>%
          mutate(conus.grid.id = 1:nrow(.)) %>%
          rename('geometry' = 'x')

  region <- make_region(rangelist,
                        buffer = 1,
                        sub = F,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = F,
                        clump.size = 50,
                        continuous = F)
  expect_equal(nrow(region$sp.grid), 12162)

  region <- make_region(rangelist,
                        buffer = 1,
                        sub = F,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = T,
                        clump.size = 50,
                        continuous = F)
  expect_equal(nrow(region$sp.grid), 12130)


  region <- make_region(rangelist,
                        buffer = 1,
                        sub = F,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = T,
                        clump.size = 10,
                        continuous = F)
  expect_equal(nrow(region$sp.grid), 12159)


})

# test_that("continuous = T works", {
#
#   rangelist <- get_range(range.path = c(paste0("~/GitHub/species-futures/data/species/GPOR/GAP/"),
#                                         paste0("~/GitHub/species-futures/data/species/GPOR/IUCN/")),
#                          range.name = c("GAP", "IUCN"), crs = 4326)
#
#   boundary <- rangelist[[1]]
#   grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 10000) %>%
#           st_as_sf() %>%
#           mutate(conus.grid.id = 1:nrow(.)) %>%
#           rename('geometry' = 'x')
#
#   region <- make_region(rangelist,
#                         buffer = 50000,
#                         sub = F,
#                         boundary = boundary,
#                         grid = grid,
#                         rm.clumps = F,
#                         clump.size = 50,
#                         continuous = T)
#
#   expect_equal(nrow(region$sp.grid), 12130)
#
#
# })
#
#
# test_that("no boundary works", {
#
#   rangelist <- get_range(range.path = c(paste0("~/GitHub/species-futures/data/species/RACA/GAP/"),
#                                         paste0("~/GitHub/species-futures/data/species/RACA/IUCN/")),
#                          range.name = c("GAP", "IUCN"), crs = 4326)
#
#   boundary <- rangelist[[1]]
#   grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 10000) %>%
#           st_as_sf() %>%
#           mutate(conus.grid.id = 1:nrow(.)) %>%
#           rename('geometry' = 'x')
#
#   region <- make_region(rangelist,
#                         buffer = 50000,
#                         sub = F,
#                         #boundary = boundary,
#                         grid = grid,
#                         rm.clumps = F,
#                         clump.size = 50,
#                         continuous = T)
#
#
#   expect_type(region, "list")
#
#   expect_equal(length(region), 4)
#
#   expect_equal(names(region), c("range", "region", "sp.grid", "boundary"))
#
#   expect_s3_class(region$range, "sf")
#   expect_s3_class(region$region, "sf")
#   expect_s3_class(region$sp.grid, "sf")
#
#
#   expect_equal(nrow(region$sp.grid), 5573)
#
#
# })
#
#
#
# test_that("lat/lon cropping works", {
#
#   rangelist <- get_range(range.path = c(paste0("~/GitHub/species-futures/data/species/GPOR/GAP/"),
#                                         paste0("~/GitHub/species-futures/data/species/GPOR/IUCN/")),
#                          range.name = c("GAP", "IUCN"), crs = 4326)
#
#   boundary <- rangelist[[1]]
#   grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>%
#           st_as_sf() %>%
#           mutate(conus.grid.id = 1:nrow(.)) %>%
#           rename('geometry' = 'x')
#
#   region <- make_region(rangelist,
#                         buffer = 50000,
#                         sub = F,
#                         lat.lo = 40,
#                         lat.hi = 44,
#                         lon.lo = -80,
#                         lon.hi = -75,
#                         boundary = boundary,
#                         grid = grid,
#                         rm.clumps = F,
#                         clump.size = 50,
#                         continuous = T)
#
#
#   expect_type(region, "list")
#
#   expect_equal(length(region), 4)
#
#   expect_equal(names(region), c("range", "region", "sp.grid", "boundary"))
#
#   expect_s3_class(region$range, "sf")
#   expect_s3_class(region$region, "sf")
#   expect_s3_class(region$sp.grid, "sf")
#   expect_s3_class(region$boundary, "sf")
#
#
#   expect_equal(nrow(region$sp.grid), 20)
#
#
# })


