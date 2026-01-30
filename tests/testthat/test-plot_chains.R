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
#   out1 <- plot_chains(out$samples, data = out$data, constants = out$constants, cov.labs = cov.labs, plot = "B", cutoff = 0)
#   expect_doppelganger("B chains", out1$plot)
# 
#   out2 <- plot_chains(samples = out$samples, data = out$data, constants = out$constants, cov.labs = cov.labs, plot = "alpha", cutoff = 0)
#   expect_doppelganger("alpha chains", out2$plot)
#   
#   out3 <- plot_chains(out$samples, data = out$data, constants = out$constants, cov.labs = cov.labs, plot = "observation", cutoff = 0)
#   expect_doppelganger("observation chains", out3$plot)
#   
# 
# 
# })
