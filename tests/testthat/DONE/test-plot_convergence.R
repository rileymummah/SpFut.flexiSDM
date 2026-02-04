test_that("plot_convergence() works", {

  # full
  mod4 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4.rds')

  out <- plot_convergence(mod4$out)

  vdiffr::expect_doppelganger("convergence metrics", out$plot)
  expect_true(is.data.frame(out$dat))
  expect_equal(nrow(out$dat), 14)
  expect_equal(colnames(out$dat), c("source", "data.type", "type", "covariate", "Rhat", 'ESS'))

})
