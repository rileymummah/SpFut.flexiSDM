# plot_convergence <- function(out) {
#   tmp <- c()
#   for (i in 2:(length(out)-1)) {
#     tmp1 <- out[[i]] %>%
#       mutate(type = names(out)[i])
#     tmp <- bind_rows(tmp, tmp1)
#   }
#
#   tmp1 <- tmp %>%
#     filter(type %in% c("process.coef", "obs.coef", "alpha")) %>%
#     select(!name) %>%
#     mutate(`Log(ESS)` = log(ESS),
#            Rhat = rhat,
#            type1 = case_when(type == "process.coef" ~ "Process \ncoefficients",
#                              type == "obs.coef" ~ "Observation \ncoefficients",
#                              type == "alpha" ~ "Dataset \nintercepts")) %>%
#     pivot_longer(cols = c(Rhat, `Log(ESS)`))
#
#   ggplot(tmp1) +
#     geom_boxplot(aes(x = type1, y = value), outlier.shape = NA) +
#     geom_jitter(aes(x = type1, y = value), width = 0.2) +
#     geom_hline(data = data.frame(yint = 1.1, name = "Rhat"), aes(yintercept = yint), linetype = "solid", color = "red") +
#     geom_hline(data = data.frame(yint = 5.29, name = "Log(ESS)"), aes(yintercept = yint), linetype = "solid", color = "red") +
#     facet_wrap(~name, scales = "free_y") +
#     theme_bw() +
#     theme(axis.text.x = element_text(hjust = 0.5,
#                                      vjust = 1)) +
#     labs(x = "Parameter", y = "Value")
# }
