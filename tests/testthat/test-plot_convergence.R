# test_that("plot_convergence() works", {
#   
#   # full
#   mod4 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4.rds')
#   
#   mod4$samples <- lapply(mod4$samples,
#                          get_derived,
#                          data = mod4$data,
#                          project = 0)
#   
#   summarize_samples(samples = mod4$samples,
#                     data = mod4$data,
#                     constants = mod4$constants,
#                     project = 0,
#                     coarse.grid = F,
#                     cutoff = 0,
#                     block.out = "none",
#                     gridkey = mod4$gridkey,
#                     spatkey = NULL,
#                     effort = F,
#                     SLURM = F) -> out0
#   
#   out <- plot_convergence(out0)
#   
#   expect_doppelganger("convergence metrics", out$plot)
#   expect_true(is.data.frame(out$dat))
#   expect_equal(nrow(out$dat), 14)
#   expect_equal(colnames(out$dat), c("source", "data.type", "type", "covariate", "Rhat", 'ESS'))
#   
# })
