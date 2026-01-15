# test_that("plot_chains() works", {
#   
#   out <- fit_test_model(path = "../../../species-futures/",
#                         sp.auto = F,
#                         iter = 1000)
#   
#   
#   cov.labs <- data.frame(covar = c("prec", "elev", "temp"),
#                          lab = c("Precipitation", "Elevation", "Temperature"))
#   expect_error(plot_chains(out$samples, data = out$data, constants = out$constants, cov.labs = cov.labs, plot = "B", cutoff = 0))
#   
#   cov.labs <- data.frame(covariate = c("prec", "elev", "temp"),
#                          lab = c("Precipitation", "Elevation", "Temperature"))
#   expect_error(plot_chains(out$samples, data = out$data, constants = out$constants, cov.labs = cov.labs, plot = "B", cutoff = 0))
#   
#   
#   
#   cov.labs <- data.frame(covariate = c("prec", "elev", "temp"),
#                          Label = c("Precipitation", "Elevation", "Temperature"))
#   pl <- plot_chains(out$samples, data = out$data, constants = out$constants, cov.labs = cov.labs, plot = "B", cutoff = 0)
#   expect_doppelganger("B chains", pl)
#   
#   pl <- plot_chains(out$samples, data = out$data, constants = out$constants, cov.labs = cov.labs, plot = "alpha", cutoff = 0)
#   expect_doppelganger("alpha chains", pl)
#   
#   
#   
# })
