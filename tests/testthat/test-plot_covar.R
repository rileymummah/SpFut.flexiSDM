test_that("plot_covar() works", {

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

  ncell <- nrow(region$sp.grid)
  set.seed(1)
  covar <- data.frame(conus.grid.id = region$sp.grid$conus.grid.id,
                      temp = rnorm(ncell, 0, 1) + rnorm(ncell, 0, 1),
                      prec = runif(ncell, 0, 1) + rnorm(ncell, 0, 1),
                      elev = runif(ncell, 0, 1) + runif(ncell, 0, 1))

  covlabs <- data.frame(covariate = c("temp", "prec", "elev"),
                        Label = c("Temperature", "Precipitation", "Elevation"))

  out <- plot_covar(covar,
             region,
             cov.labs = covlabs,
             scaled = F)


  # check format
  expect_s3_class(out$dat, "sf")
  expect_equal(nrow(out$dat), 237)
  expect_equal(unique(table(out$dat$conus.grid.id)), 3) # each conus.grid.id has 3 values
  vdiffr::expect_doppelganger("Covariate values", out$plot)

  
  # Scale covariates 
  covar_unscaled <- covar
  numcols <- sapply(covar, is.numeric)
  numcols <- which(numcols)
  covar[,numcols] <- sapply(covar[,numcols], scale_this)
  
  out <- plot_covar(covar,
                    region,
                    cov.labs = covlabs,
                    scaled = T)
  
  
  # check format
  expect_s3_class(out$dat, "sf")
  expect_equal(nrow(out$dat), 237)
  expect_equal(unique(table(out$dat$conus.grid.id)), 3) # each conus.grid.id has 3 values
  vdiffr::expect_doppelganger("Covariate values, scaled", out$plot)
  
})
