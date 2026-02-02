test_that("plot_posteriors() works", {

  # mod1 <- fit_test_model(path = "~/GitHub/species-futures/",
  #                       sp.auto = F,
  #                       iter = 1000)

  mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')

  cov.labs <- data.frame(covariate = c("covA", "covB", "covC"),
                         Label = c("Precipitation", "Elevation", "Temperature"))

  pl <- plot_posteriors(mod1$samples, data = mod1$data, constants = mod1$constants,
                        cov.labs = cov.labs, plot = "B", cutoff = 0)
  vdiffr::expect_doppelganger("B posterior", pl)

  pl <- plot_posteriors(mod1$samples, data = mod1$data, constants = mod1$constants,
                        cov.labs = cov.labs, plot = "alpha", cutoff = 0)
  vdiffr::expect_doppelganger("alpha posterior", pl)




  cov.labs <- data.frame(covs = c("covA", "covB", "covC"),
                         Label = c("Precipitation", "Elevation", "Temperature"))
  expect_error(plot_posteriors(mod1$samples, data = mod1$data, constants = mod1$constants,
                               cov.labs = cov.labs, plot = "B", cutoff = 0))


  cov.labs <- data.frame(covariate = c("covA", "covB", "covC"),
                         lab = c("Precipitation", "Elevation", "Temperature"))
  expect_error(plot_posteriors(mod1$samples, data = mod1$data, constants = mod1$constants,
                               cov.labs = cov.labs, plot = "B", cutoff = 0))
})
