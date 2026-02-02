test_that("z_for_nimble() works", {

    # set up ----
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

  covariates <- data.frame(conus.grid.id = region$sp.grid$conus.grid.id,
                           temp = rnorm(nrow(region$sp.grid), 0, 1),
                           prec = rnorm(nrow(region$sp.grid), 0, 1) + runif(nrow(region$sp.grid), 0, 1))


  # works! ----
  out <- z_for_nimble(covariates)

  expect_type(out, "list")
  expect_equal(length(out), 2)
  expect_equal(names(out), c("data", "constants"))
  expect_equal(nrow(out$data$Xz), out$constants$nCell)
  expect_equal(ncol(out$data$Xz), out$constants$nCovZ)


  # throw in some NAs ----
  covariates[4, 2] <- NA
  expect_error(out <- z_for_nimble(covariates))


  # remove conus.grid.id col name ----
  covariates <- data.frame(test = region$sp.grid$conus.grid.id,
                           temp = rnorm(nrow(region$sp.grid), 0, 1),
                           prec = rnorm(nrow(region$sp.grid), 0, 1) + runif(nrow(region$sp.grid), 0, 1))
  expect_error(out <- z_for_nimble(covariates))

})
