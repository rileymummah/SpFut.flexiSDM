test_that("plot_effects() works", {

  mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')

  cov.labs <- data.frame(covariate = c("covA", "covB", "covC"),
                         Label = c("CovA", "CovB", "CovC"))
  out1 <- plot_effects(data = mod1$data, out = mod1$out, cov.labs = cov.labs)
  
  vdiffr::expect_doppelganger("spatial", out1$plot)
  expect_equal(nrow(out1$dat), 946)
  
  
  
  
  mod4 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4.rds')
  
  cov.labs <- data.frame(covariate = c("covA", "covB", "covC"),
                         Label = c("CovA", "CovB", "CovC"))
  out4 <- plot_effects(data = mod4$data, out = mod4$out, cov.labs = cov.labs)
  
  vdiffr::expect_doppelganger("spatial", out4$plot)
  expect_equal(nrow(out4$dat), 1036)
  

  
})
