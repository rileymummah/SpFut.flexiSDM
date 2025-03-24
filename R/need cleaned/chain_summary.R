# chain_summary <- function(i, samples, chains = 1:3, cutoff = 0) {
#   library(magrittr)
#   library(tidyverse)
#
#   s <- lapply(chains,
#               FUN = function(x, index, cutoff) {
#                 as.data.frame(samples[[x]][,index]) %>%
#                   mutate(chain = x) %>%
#                   slice((cutoff+1):nrow(.)) -> tmp
#
#                 colnames(tmp)[1] <- 'var1'
#                 return(tmp)
#               },
#               index = i,
#               cutoff = cutoff)
#
#   # summarize
#   tmp <- bind_rows(s) %>%
#     filter(chain %in% chains) %>%
#     dplyr::summarize(mean = mean(var1, na.rm = T),
#                      lo = quantile(var1, probs = 0.025, na.rm = T),
#                      hi = quantile(var1, probs = 0.975, na.rm = T),
#                      lotail = mean(var1) - sd(var1),
#                      hitail = mean(var1) + sd(var1)) %>%
#     mutate(param = colnames(samples[[1]])[i],
#            param = gsub("[",'',param, fixed=T),
#            param = gsub("]",'',param, fixed=T))
#
#
#   # get uncertainty
#   denstails <- bind_rows(s) %>%
#     filter(chain %in% chains) %>%
#     mutate(hi = case_when(var1 > tmp$hitail ~ 1,
#                           T ~ 0),
#            lo = case_when(var1 < tmp$lotail ~ 1,
#                           T ~ 0)) %>%
#     summarize(hitail = mean(hi),
#               lotail = mean(lo))
#
#   tmp$hitail <- denstails$hitail
#   tmp$lotail <- denstails$lotail
#
#   tmp$unc.range <- tmp$hi - tmp$lo
#   tmp$unc.rel <- (tmp$hi - tmp$lo)/tmp$mean
#
#   # get rhat
#   tmp1 <- bind_rows(s) %>%
#     mutate(var1 = as.numeric(var1)) %>%
#     pivot_wider(.,
#                 names_from = 'chain',
#                 names_prefix = 's',
#                 values_from = 'var1',
#                 values_fn = list) %>%
#     unnest(cols=everything())
#   tmp1 <- tmp1[,chains]
#   rhat <- rstan::Rhat(as.matrix(tmp1))
#
#   # # get gelman
#   # tmp2 <- list(coda::as.mcmc(tmp1[,1]), coda::as.mcmc(tmp1[,2]), coda::as.mcmc(tmp1[,2]))
#   # tmp2 <- coda::as.mcmc.list(tmp2)
#   # gelman <- as.numeric(coda::gelman.diag(tmp2)[[1]])[1]
#
#   # get ESS
#   bind_rows(s) %>%
#     mutate(var1 = as.numeric(var1)) %>%
#     pull(var1) -> tmp.ess
#
#   # If there is missing data in the projections, ESS is NaN and breaks the loop here.
#   if (all(is.nan(tmp.ess))) {
#     ESS <- NA
#   } else {
#     coda::as.mcmc(tmp.ess) %>%
#       coda::effectiveSize() %>%
#       as.numeric() -> ESS
#   }
#
#
#   # tmp2 <- list(coda::as.mcmc(tmp1[,1]),
#   #              coda::as.mcmc(tmp1[,2]),
#   #              coda::as.mcmc(tmp1[,3]))
#   # tmp2 <- coda::as.mcmc.list(tmp2)
#   # if (is.na(sum(tmp1))) ESS <- NA
#   # if (is.na(sum(tmp1)) == F) ESS <- as.numeric(coda::effectiveSize(tmp2))
#
#
#
#   tmp <- tmp %>%
#     select(param, everything()) %>%
#     mutate(rhat = rhat,
#            ESS = ESS)
#   #gelman = gelman)
#
#   # out <- bind_rows(out, tmp)
#
#   # setTxtProgressBar(pb,i)
#
#   return(tmp)
# }
