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


  out <- plot_pars(proc, plot.type = "full", cov.labs = cov.labs)
  vdiffr::expect_doppelganger("proc-full", out$plot)
  expect_equal(nrow(out$dat), 3)
  expect_equal(ncol(out$dat), 12)
  
  out <- plot_pars(proc, plot.type = "cv", cov.labs = cov.labs)
  vdiffr::expect_doppelganger("proc-cv", out$plot)
  expect_equal(nrow(out$dat), 12)
  expect_equal(ncol(out$dat), 12)
  

  out <- plot_pars(obs, plot.type = "full", cov.labs = cov.labs)
  vdiffr::expect_doppelganger("obs-full", out$plot)
  expect_equal(nrow(out$dat), 7)
  expect_equal(ncol(out$dat), 15)
  
  out <- plot_pars(obs, plot.type = "cv", cov.labs = cov.labs)
  vdiffr::expect_doppelganger("obs-cv", out$plot)
  expect_equal(nrow(out$dat), 26)
  expect_equal(ncol(out$dat), 15)
  

  out <- plot_pars(alpha, plot.type = "full", cov.labs = cov.labs)
  vdiffr::expect_doppelganger("alpha-full", out$plot)
  expect_equal(nrow(out$dat), 4)
  expect_equal(ncol(out$dat), 12)
  
  out <- plot_pars(alpha, plot.type = "cv", cov.labs = cov.labs)
  vdiffr::expect_doppelganger("alpha-cv", out$plot)
  expect_equal(nrow(out$dat), 16)
  expect_equal(ncol(out$dat), 12)
  

  out <- plot_pars(tau, plot.type = "full", cov.labs = cov.labs)
  vdiffr::expect_doppelganger("tau-full", out$plot)
  expect_equal(nrow(out$dat), 1)
  expect_equal(ncol(out$dat), 11)
  
  out <- plot_pars(tau, plot.type = "cv", cov.labs = cov.labs)
  vdiffr::expect_doppelganger("tau-cv", out$plot)
  expect_equal(nrow(out$dat), 4)
  expect_equal(ncol(out$dat), 11)
  

  # test no spatial auto
  mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')
  expect_error(plot_pars(mod1$out$tau, plot.type = "full", cov.labs = cov.labs))
})
