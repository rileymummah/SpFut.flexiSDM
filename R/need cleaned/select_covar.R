# select_covar <- function(covs,
#                          threshold = 0.4) {
#   # selectively remove covariates until all high correlations are gone
#   cors <- cor(as.data.frame(covar[,c(covs)])) %>%
#     as.data.frame() %>%
#     mutate(cov1 = row.names(.)) %>%
#     pivot_longer(!cov1) %>%
#     filter(value != 1) %>%
#     rename(cov2 = name)
#
#
#   # get the max correlation
#   rm.vals <- max(abs(cors$value))
#
#   covs.rm <- c()
#   while (rm.vals > threshold) {
#     rm.var <- cors$cov1[which(abs(cors$value) %in% rm.vals)]
#
#     tmp <- cors %>%
#       filter(cov1 %in% rm.var) %>%
#       group_by(cov1) %>%
#       summarize(avg = mean(abs(value))) %>%
#       filter(avg == max(avg)) %>%
#       pull(cov1)
#
#     covs.rm <- c(covs.rm, tmp)
#     #cat("removing ", tmp, "\n")
#
#     cors <- cors %>%
#       filter(cov1 != tmp,
#              cov2 != tmp)
#
#     # get the new max correlation
#     rm.vals <- max(abs(cors$value))
#   }
#
#   return(covs.rm)
# }
