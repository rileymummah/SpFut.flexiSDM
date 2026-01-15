# test_that("plot_posteriors() works", {
#   
#   out <- fit_test_model(path = "../../../species-futures/",
#                         sp.auto = F,
#                         iter = 1000)
#   
#   cov.labs <- data.frame(covariate = c("prec", "elev", "temp"),
#                          Label = c("Precipitation", "Elevation", "Temperature"))
#   
#   pl <- plot_posteriors(out$samples, data = out$data, cov.labs = cov.labs, plot = "B", cutoff = 0)
#   expect_doppelganger("B posterior", pl)
#   
#   pl <- plot_posteriors(out$samples, data = out$data, cov.labs = cov.labs, plot = "alpha", cutoff = 0)
#   expect_doppelganger("alpha posterior", pl)
#   
#   
#   
#   
#   cov.labs <- data.frame(covs = c("prec", "elev", "temp"),
#                          Label = c("Precipitation", "Elevation", "Temperature"))
#   expect_error(plot_posteriors(out$samples, data = out$data, cov.labs = cov.labs, plot = "B", cutoff = 0))
#   
#   
#   cov.labs <- data.frame(covariate = c("prec", "elev", "temp"),
#                          lab = c("Precipitation", "Elevation", "Temperature"))
#   expect_error(plot_posteriors(out$samples, data = out$data, cov.labs = cov.labs, plot = "B", cutoff = 0))
# })
