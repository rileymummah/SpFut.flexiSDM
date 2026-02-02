test_that("plot_pars() works", {

  cov.labs <- data.frame(covariate = c("covA", "covB", "covC"),
                         Label = c("CovA", "CovB", "CovC"))

  # CV 1
  mod4cv1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4cv1.rds')

  # CV 2
  mod4cv2 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4cv2.rds')

  # CV 3
  mod4cv3 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4cv3.rds')

  # full
  mod4 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4.rds')



  proc <- bind_rows(mod4cv1$out$process.coef, mod4cv2$out$process.coef, mod4cv3$out$process.coef, mod4$out$process.coef)
  obs <- bind_rows(mod4cv1$out$obs.coef, mod4cv2$out$obs.coef, mod4cv3$out$obs.coef, mod4$out$obs.coef)
  alpha <- bind_rows(mod4cv1$out$alpha, mod4cv2$out$alpha, mod4cv3$out$alpha, mod4$out$alpha)
  tau <- bind_rows(mod4cv1$out$tau, mod4cv2$out$tau, mod4cv3$out$tau, mod4$out$tau)


  vdiffr::expect_doppelganger("proc-full", plot_pars(proc, plot.type = "full", cov.labs = cov.labs))
  vdiffr::expect_doppelganger("proc-cv", plot_pars(proc, plot.type = "cv", cov.labs = cov.labs))


  vdiffr::expect_doppelganger("obs-full", plot_pars(obs, plot.type = "full", cov.labs = cov.labs))
  vdiffr::expect_doppelganger("obs-cv", plot_pars(obs, plot.type = "cv", cov.labs = cov.labs))


  vdiffr::expect_doppelganger("alpha-full", plot_pars(alpha, plot.type = "full", cov.labs = cov.labs))
  vdiffr::expect_doppelganger("alpha-cv", plot_pars(alpha, plot.type = "cv", cov.labs = cov.labs))


  vdiffr::expect_doppelganger("tau-full", plot_pars(tau, plot.type = "full", cov.labs = cov.labs))
  vdiffr::expect_doppelganger("tau-cv", plot_pars(tau, plot.type = "cv", cov.labs = cov.labs))


  # test no spatial auto
  mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')
  expect_error(plot_pars(mod1$out$tau, plot.type = "full", cov.labs = cov.labs))
})
