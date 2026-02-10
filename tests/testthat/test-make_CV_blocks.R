
test_that("make_CV_blocks() works", {

  rangelist <- get_range(range.path = c(paste0("~/GitHub/species-futures/data/species/GPOR/GAP/"),
                                        paste0("~/GitHub/species-futures/data/species/GPOR/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)

  boundary <- rangelist[[1]]
  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>%
          st_as_sf() %>%
          mutate(conus.grid.id = 1:nrow(.)) %>%
          rename(geometry = x)

  region <- make_region(rangelist,
                        buffer = 50,
                        sub = F,
                        boundary = boundary,
                        grid = grid,
                        rm.clumps = T,
                        clump.size = 50,
                        continuous = F)


  expect_s3_class(make_CV_blocks(region, 5, 5, 3), "sf")

  expect_s3_class(make_CV_blocks(region, 3, 5, 2), "sf")

})
