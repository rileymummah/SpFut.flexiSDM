test_that("cor_covar() works", {

  set.seed(1)
  ncell <- 79
  covar <- data.frame(conus.grid.id = 1:ncell,
                      temp = rnorm(ncell, 0, 1) + rnorm(ncell, 0, 1),
                      prec = runif(ncell, 0, 1) + rnorm(ncell, 0, 1),
                      elev = runif(ncell, 0, 1) + runif(ncell, 0, 1))

  covlabs <- data.frame(covariate = c("temp", "prec", "elev"),
                        Label = c("Temperature", "Precipitation", "Elevation"))

  out <- cor_covar(covar,
                   cov.labs = covlabs,
                   color.threshold = 0.04)

    # check format
    expect_equal(nrow(out$dat), 6)
    expect_equal(unique(table(out$dat$cov2)), 2) # each cov2 has 2 values
    vdiffr::expect_doppelganger("Covariate correlations", out$plot)

})
