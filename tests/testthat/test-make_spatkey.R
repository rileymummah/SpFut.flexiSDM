test_that("make_spatkey() works", {

  rangelist <- get_range(range.path = c(paste0("~/GitHub/species-futures/data/species/GPOR/GAP/"),
                                        paste0("~/GitHub/species-futures/data/species/GPOR/IUCN/")),
                         range.name = c("GAP", "IUCN"), crs = 4326)

  boundary <- rangelist[[1]]

  apothem = 100000 # distance from centroid to edge of hexagon

  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = apothem,
                       square = F, flat_topped = T) %>%
            st_as_sf() %>%
            mutate(conus.grid.id = 1:nrow(.)) %>%
            rename(geometry = x)

  spatkey <- make_spatkey(grid, apothem)

  expect_equal(class(spatkey), "list")
})
