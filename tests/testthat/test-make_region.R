

test_that("make_region() works", {
  
  rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
                                  paste0("../../../species-futures/data/species/GPOR/IUCN/")),
                   range.name = c("GAP", "IUCN"), crs = 4326)
  
  boundary <- rangelist[[1]]
  grid <- st_make_grid(boundary, cellsize = 1) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))
  
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
  
  
  # dots <- st_make_grid(st_transform(region$boundary, crs = 3857), what = "centers", cellsize = c(10000, 10000)) %>% st_as_sf()
  # dots1 <- st_filter(dots, st_transform(region$boundary, crs = 3857)) %>%
  #   mutate(id = 1:nrow(.))
  # 
  # suppressWarnings(dots2 <- st_intersection(dots1, region$sp.grid))
  # missing <- setdiff(dots1$id, dots2$id)
  # 
  # expect_equal(length(missing), 0)

})



test_that("sub = T works", {

  rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
                                        paste0("../../../species-futures/data/species/GPOR/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)

  boundary <- rangelist[[1]]


  grid <- st_make_grid(boundary, cellsize = 1) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))
  expect_error(make_region(rangelist,
                           buffer = 50,
                           sub = T,
                           boundary = boundary,
                           grid = grid,
                           rm.clumps = T,
                           clump.size = 2,
                           continuous = F))
  
  grid <- st_make_grid(boundary, cellsize = 0.1) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))
  region <- make_region(rangelist,
                        buffer = 500000,
                        sub = T,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = F,
                        clump.size = 2,
                        continuous = F)

  expect_type(region, "list")

  expect_equal(length(region), 4)

  expect_equal(names(region), c("range", "region", "sp.grid", "boundary"))

  expect_s3_class(region$range, "sf")
  expect_s3_class(region$region, "sf")
  expect_s3_class(region$sp.grid, "sf")
  expect_s3_class(region$boundary, "sf")

})

# 
# test_that("rm.clumps = F works", {
#   
#   rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
#                                         paste0("../../../species-futures/data/species/GPOR/IUCN/")),
#                          range.name = c("GAP", "IUCN"), crs = 4326)
#   
#   boundary <- rangelist[[1]]
#   grid <- st_make_grid(boundary) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))
#   
#   region <- make_region(rangelist,
#                         buffer = 1,
#                         crs = 4326,
#                         sub = F,
#                         boundary = boundary,
#                         grid = grid,
#                         rm.clumps = F,
#                         clump.size = 50, 
#                         continuous = F)
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
# })
# 
# test_that("continuous = T works", {
#   
#   rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
#                                         paste0("../../../species-futures/data/species/GPOR/IUCN/")),
#                          range.name = c("GAP", "IUCN"), crs = 4326)
#   
#   boundary <- rangelist[[1]]
#   grid <- st_make_grid(boundary) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))
#   
#   region <- make_region(rangelist,
#                         buffer = 1,
#                         crs = 4326,
#                         sub = T,
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
# })
