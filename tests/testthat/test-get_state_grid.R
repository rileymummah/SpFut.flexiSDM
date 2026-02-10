test_that("get_state_grid() works", {


  range.path <- c("~/GitHub/species-futures/data/species/GPOR/GAP/",
                  "~/GitHub/species-futures/data/species/GPOR/IUCN")
  range.name <- c("GAP", "IUCN")
  rangelist <- get_range(range.path,
                         range.name,
                         crs = 4326)

  # load("data/USA/grid-covar.rdata")

  boundary <- rangelist[[1]]
  grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>%
          st_as_sf() %>%
          mutate(conus.grid.id = 1:nrow(.)) %>%
          rename(geometry = x)


  # Make region
  region <- make_region(rangelist,
                        buffer = 50000,
                        sub = F,
                        boundary = boundary,
                        grid = grid)
  states <- ne_states(country = c("Canada", "Mexico", "United States of America"),
                      returnclass = "sf")


  # test
  stategrid <- get_state_grid(region, states)

  # tmp <- region$sp.grid %>% full_join(stategrid, by = "conus.grid.id")
  # states1 <- st_crop(st_transform(states, st_crs(tmp)), st_buffer(tmp, 100000))
  #
  # ggplot(tmp) + geom_sf(aes(fill = name), alpha = 0.5) + geom_sf(data = states1, fill = NA)

  expect_equal(nrow(stategrid), 149)
  expect_equal(length(unique(stategrid$conus.grid.id)), 79)


})
