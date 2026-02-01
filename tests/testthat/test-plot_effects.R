# test_that("plot_effects() works", {
#   
#   mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')
#   
#   mod1$samples <- lapply(mod1$samples,
#                          get_derived,
#                          data = mod1$data,
#                          project = 0)
#   
#   summarize_samples(samples = mod1$samples,
#                     data = mod1$data,
#                     constants = mod1$constants,
#                     project = 0,
#                     coarse.grid = F,
#                     cutoff = 0,
#                     block.out = ,
#                     gridkey = mod1$gridkey,
#                     spatkey = NULL,
#                     effort = F,
#                     SLURM = F) -> out
#   
#   cov.labs <- data.frame(covariate = c("covA", "covB", "covC"),
#                          Label = c("CovA", "CovB", "CovC"))
#   out1 <- plot_effects(mod1$data, out, cov.labs = cov.labs)
#   out1$dat %>% filter(lo > mean | hi < mean) -> tmp
#   
#   ggplot(tmp) +
#     geom_hline(yintercept = 0) +
#     geom_ribbon(aes(x = .data$xplot, ymax = .data$hi, ymin = .data$lo),
#                 alpha = 0.5) +
#     geom_line(aes(x = .data$xplot, y = .data$mean)) +
#     facet_wrap(~ cov, scales = "free") +
#     theme_bw() +
#     labs(x = xlab, y = "Exp(Estimate)",
#          title = "Marginal effects of process covariates",
#          subtitle = "Ribbon indicates 95% credible interval")
# })
