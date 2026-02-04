test_that("plot_effects() works", {

  mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')

  cov.labs <- data.frame(covariate = c("covA", "covB", "covC"),
                         Label = c("CovA", "CovB", "CovC"))
  out1 <- plot_effects(data = mod1$data, out = mod1$out, cov.labs = cov.labs)
  
  out1$plot
  
  out1$dat %>% filter(lo > mean | hi < mean, cov == "CovB") -> tmp

  ggplot(tmp) +
    geom_hline(yintercept = 0) +
    geom_ribbon(aes(x = .data$xplot, ymax = .data$hi, ymin = .data$lo),
                alpha = 0.5) +
    geom_line(aes(x = .data$xplot, y = .data$mean)) +
    facet_wrap(~ cov, scales = "free") +
    theme_bw() +
    labs(x = xlab, y = "Exp(Estimate)",
         title = "Marginal effects of process covariates",
         subtitle = "Ribbon indicates 95% credible interval")
})
