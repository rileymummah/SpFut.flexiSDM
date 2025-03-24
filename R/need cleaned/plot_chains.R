# plot_chains <- function(samples,
#                         path,
#                         data,
#                         all.chains = T,
#                         save = T,
#                         cutoff = 0,
#                         B = T,
#                         alpha = F,
#                         tau = F,
#                         Bpriordist = "dnorm",
#                         Bpriorvar1 = 0,
#                         Bpriorvar2 = 1) {
#
#   do <- c()
#   if (B == T) do <- c(do, "B")
#   if (alpha == T) do <- c(do, "alpha")
#   if (tau == T) do <- c(do, "tau")
#
#
#   for (d in do) {
#
#     if (d == "B") {
#       bnames <- data.frame(name = colnames(data$Xz),
#                            param = paste0("B[", 1:ncol(data$Xz), "]"))
#       bind <- grep("B", colnames(samples[[1]]))
#       rm <- grep("XB", colnames(samples[[1]]))
#       bind <- bind[-which(bind %in% rm)]
#
#     } else if (d == "alpha") {
#       bind <- grep("alpha", colnames(samples[[1]]))
#       bnames <- data.frame(name = unlist(constants[grep("name", names(constants))]),
#                            param = paste0("alpha[", 1:length(bind), "]"))
#     } else if (d == "tau") {
#       bind <- grep("tau", colnames(samples[[1]]))
#       bnames <- data.frame(name = "tau",
#                            param = paste0("tau[", 1:length(bind), "]"))
#     }
#
#
#
#     # select which chains to use
#     if (all.chains == T) {
#       chains <- 1:length(samples)
#     } else {
#       chains <- c()
#       tmp1 <- as.data.frame(samples[[1]][,bind])
#
#       if (d == "B") {
#         startcol <- 2
#         endcol <- ncol(tmp1)
#       } else if (d == "alpha") {
#         startcol <- 1
#         endcol <- ncol(tmp1)
#       }
#
#       if (F %in% (colMeans(tmp1[,startcol:endcol]) < 4) == F &
#           F %in% (colMeans(tmp1[,startcol:endcol]) > -4) == F) {
#         chains <- c(chains, 1)
#       }
#       tmp2 <- as.data.frame(samples[[2]][,bind])
#       if (F %in% (colMeans(tmp2[,startcol:endcol]) < 4) == F &
#           F %in% (colMeans(tmp2[,startcol:endcol]) > -4) == F) {
#         chains <- c(chains, 2)
#       }
#       tmp3 <- as.data.frame(samples[[3]][,bind])
#       if (F %in% (colMeans(tmp3[,startcol:endcol]) < 4) == F &
#           F %in% (colMeans(tmp3[,startcol:endcol]) > -4) == F) {
#         chains <- c(chains, 3)
#       }
#     }
#
#     if (length(chains) == 0) chains <- 1:3
#
#     # calculate rhat
#     all <- list()
#     for (i in 1:length(bind)) {
#       df <- as.matrix(data.frame(chain1 = as.vector(samples[[1]][,bind[i]]),
#                                  chain2 = as.vector(samples[[2]][,bind[i]]),
#                                  chain3 = as.vector(samples[[3]][,bind[i]])))
#       df <- df[cutoff:nrow(df),chains]
#       all[[i]] <- df
#     }
#
#
#     rhat <- unlist(lapply(all, rstan::Rhat))
#     bnames$rhat <- round(rhat, 2)
#
#
#     # plot chains
#     call <- c()
#     for (c in chains) {
#       c1 <- as.data.frame(samples[[c]][,bind]) %>%
#         mutate(n = 1:nrow(.)) %>%
#         pivot_longer(!n) %>%
#         mutate(chain = c)
#       call <- bind_rows(call, c1)
#     }
#
#     # If there's only one, need to rename it
#     if (length(bind) == 1) {
#       call$name <- paste0(d, "[", 1, "]")
#     }
#
#     call <- call %>%
#       full_join(bnames, by = c("name" = "param"))
#
#     if (d == "B") {
#       cov.labs <- read.csv("data/covariate-labels.csv")
#
#       call <- call %>%
#         mutate(cov1 = gsub("2", "", name.y),
#                tmp = gsub("_x_.*", "", name.y),
#                quad = case_when(substr(tmp, nchar(tmp), nchar(tmp)) == 2 ~ "^2",
#                                 T ~ "")) %>%
#         left_join(cov.labs, by = c("cov1" = "covariate")) %>%
#         mutate(name.y = paste0(Label, quad)) %>%
#         select(!tmp)
#     }
#
#
#     call <- call %>%
#       mutate(lab = paste0(name.y, "\nRhat = ", rhat))
#
#
#     pl <- ggplot(call) +
#       geom_line(aes(x = n, y = value, color = as.factor(chain)), linewidth = 0.1) +
#       geom_vline(xintercept = cutoff) +
#       facet_wrap(~lab, scales = "free_y") +
#       labs(x = "Iteration", y = "Parameter value", color = "Chain", subtitle = "Black vertical line indicates burnin") +
#       scale_color_manual(values = chaincols) +
#       theme_bw()
#
#     sub <- paste0(chains, collapse = "")
#
#
#     if (d == "B") letter <- "a"
#     if (d == "alpha") letter <- "b"
#     if (d == "tau") letter <- "c"
#
#     if (save == T) {
#       ggsave(pl, file = paste0(path, "/3_parameters-", letter, "1_chains-", sub, "-", d, ".jpg"), height = 6, width = 8)
#     } else {return(pl)}
#
#
#
#
#     pl <- ggplot(data = filter(call, n > cutoff)) +
#       geom_density(aes(x = value, color = as.factor(chain), fill = as.factor(chain)), alpha = 0.1) +
#       facet_wrap(~lab, scales = "free") +
#       labs(y = "Density", x = "Parameter value", color = "Chain", fill = "Chain", subtitle = "Posterior distribution after burnin") +
#       scale_fill_manual(values = chaincols) +
#       scale_color_manual(values = chaincols) +
#       theme_bw()
#
#     if (d %in% c("B", "alpha")) {
#
#       if (d == "B") {
#         if (Bpriordist == "dnorm") {
#           prior <- rnorm(nrow(call), Bpriorvar1, Bpriorvar2)
#         } else if (Bpriordist == "dunif") {
#           prior <- runif(nrow(call), Bpriorvar1, Bpriorvar2)
#         }
#       } else if (d == "alpha") {
#         prior <- rnorm(nrow(call), 0, 1)
#         prior <- exp(prior)
#       }
#
#
#
#       call <- call %>%
#         mutate(prior = prior) %>%
#         group_by(name) %>%
#         mutate(value1 = case_when(n > cutoff ~ value,
#                                   T ~ NA),
#                min = min(value1, na.rm = T) - 0.1,
#                max = max(value1, na.rm = T) + 0.1) %>%
#         mutate(prior = case_when(prior < min | prior > max ~ NA,
#                                  T ~ prior))
#
#
#       pl <- pl + geom_density(data = call, aes(x = prior), fill = "gray", alpha = 0.5) +
#         labs(y = "Density", x = "Parameter value", color = "Chain", fill = "Chain", subtitle = "Posterior distribution after burnin; black indicates prior distribution")
#     }
#
#
#
#     if (save == T) {
#       ggsave(pl, file = paste0(path, "/3_parameters-", letter, "2_posterior-", sub, "-", d, ".jpg"), height = 6, width = 8)
#     }
#
#
#   }
#
#
#
# }
