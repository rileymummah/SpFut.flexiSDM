test_that("plot_chains() works", {

  mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')



  cov.labs <- data.frame(covar = c("covA", "covB", "covC"),
                         lab = c("CovA", "CovB", "CovC"))
  expect_error(plot_chains(mod1$samples, data = mod1$data, constants = mod1$constants, cov.labs = cov.labs, plot = "B", cutoff = 0))

  cov.labs <- data.frame(covariate = c("covA", "covB", "covC"),
                         lab = c("CovA", "CovB", "CovC"))
  expect_error(plot_chains(mod1$samples, data = mod1$data, constants = mod1$constants, cov.labs = cov.labs, plot = "B", cutoff = 0))



  cov.labs <- data.frame(covariate = c("covA", "covB", "covC"),
                         Label = c("CovA", "CovB", "CovC"))
  out1 <- plot_chains(samples = mod1$samples, data = mod1$data, constants = mod1$constants, cov.labs = cov.labs, plot = "B", cutoff = 0)
  vdiffr::expect_doppelganger("B chains1", out1$plot)

  out2 <- plot_chains(samples = mod1$samples, data = mod1$data, constants = mod1$constants, cov.labs = cov.labs, plot = "alpha", cutoff = 0)
  vdiffr::expect_doppelganger("alpha chains1", out2$plot)

  out3 <- plot_chains(mod1$samples, data = mod1$data, constants = mod1$constants, cov.labs = cov.labs, plot = "observation", cutoff = 0)
  vdiffr::expect_doppelganger("observation chains1", out3$plot)

  expect_error(out4 <- plot_chains(mod1$samples, data = mod1$data, constants = mod1$constants, cov.labs = cov.labs, plot = "tau", cutoff = 0))


  mod3 <- readRDS('~/GitHub/species-futures/pkg-tests/mod3.rds')
  cov.labs <- data.frame(covariate = c("covA", "covB", "covC"),
                         Label = c("CovA", "CovB", "CovC"))
  out1 <- plot_chains(samples = mod3$samples, data = mod3$data, constants = mod3$constants, cov.labs = cov.labs, plot = "B", cutoff = 0)
  vdiffr::expect_doppelganger("B chains3", out1$plot)

  out2 <- plot_chains(samples = mod3$samples, data = mod3$data, constants = mod3$constants, cov.labs = cov.labs, plot = "alpha", cutoff = 0)
  vdiffr::expect_doppelganger("alpha chains3", out2$plot)

  out3 <- plot_chains(mod3$samples, data = mod3$data, constants = mod3$constants, cov.labs = cov.labs, plot = "observation", cutoff = 0)
  vdiffr::expect_doppelganger("observation chains3", out3$plot)

  out4 <- plot_chains(mod3$samples, data = mod3$data, constants = mod3$constants, cov.labs = cov.labs, plot = "tau", cutoff = 0)
  vdiffr::expect_doppelganger("tau chains3", out4$plot)


})
