# plot_pars <- function(out,
#                       plot.group = "process",
#                       plot.type = "full",
#                       path,
#                       title) {
#
#   cov.labs <- read.csv("data/covariate-labels.csv")
#
#   if (plot.type == "full") {
#
#     if (plot.group == "process") {
#
#
#
#       dat <- out$process.coef %>%
#         mutate(x = covariate)
#       xlab <- "Covariate"
#
#       # rename interaction reference level
#       ints <- grep("_x_", dat$x)
#       if (length(ints) == 2) { # interaction with quadratic term
#         cov <- gsub("_x_.*", "", dat$x[ints])
#         newname <- grep(paste0(cov, collapse = "|"), dat$x)[1:2]
#         newname1 <- paste0(dat$x[newname], "_x_reference")
#         dat$x[newname] <- newname1
#
#       } else if (length(ints) == 1) { # interaction with linear term
#         cov <- gsub("_x_.*", "", dat$x[ints])
#         newname <- grep(paste0(cov, collapse = "|"), dat$x)[1]
#         newname1 <- paste0(dat$x[newname], "_x_reference")
#         dat$x[newname] <- newname1
#       }
#
#       dat <- dat %>%
#         mutate(cov1 = gsub("2", "", covariate),
#                tmp = gsub("_x_.*", "", covariate),
#                quad = case_when(substr(tmp, nchar(tmp), nchar(tmp)) == 2 ~ "^2",
#                                 T ~ "")) %>%
#         left_join(cov.labs, by = c("cov1" = "covariate")) %>%
#         mutate(x = paste0(Label, quad)) %>%
#         select(!tmp)
#
#     } else if (plot.group == "observation") {
#       dat <- out$obs.coef %>%
#         mutate(x = covariate,
#                lab = paste0(name, " (", data.type, ')'))
#       xlab <- "Covariate"
#
#
#       dat <- dat %>%
#         mutate(cov1 = gsub("2", "", covariate),
#                quad = case_when(substr(covariate, nchar(covariate), nchar(covariate)) == 2 ~ "^2",
#                                 T ~ "")) %>%
#         left_join(cov.labs, by = c("cov1" = "covariate")) %>%
#         mutate(x = paste0(Label, quad)) %>%
#         mutate(x = case_when(x == "NA" ~ cov1,
#                              T ~ x))
#
#
#     } else if (plot.group == "dataset") {
#       dat <- out$alpha %>%
#         rename(x = name)
#       xlab <- "Dataset"
#     } else {
#       stop("plot.group must be 'process' or 'observation' or 'dataset'")
#     }
#
#
#     pl <- ggplot(dat) +
#       geom_hline(yintercept = 0) +
#       theme_bw() +
#       theme(axis.text.x = element_text(angle = 0,
#                                        hjust = 0.5,
#                                        vjust = 1)) +
#       scale_x_discrete(labels = scales::label_wrap(15)) +
#       geom_point(aes(x = x, y = mean), col='black') +
#       geom_segment(aes(x = x, xend = x,
#                        y = lo, yend = hi), col='black') +
#       labs(y = 'Estimate', x = xlab,
#            title = title,
#            subtitle = "Mean and 95% credible intervals")
#
#
#     if (plot.group == "observation") {
#       pl <- pl + facet_wrap(~lab, scales = "free")
#     }
#
#     if (plot.group == "process") letter <- "a"
#     if (plot.group == "dataset") letter <- "b"
#     if (plot.group == "observation") letter <- "c"
#
#     ggsave(pl, file = paste0(path, "/3_parameters-", letter, "3_", plot.type, "-", plot.group, ".jpg"), height = 6, width = 10)
#
#   }
#
#   if (plot.type == "cv") {
#
#     if (plot.group == "process") {
#       dat <- out %>%
#         mutate(x = covariate)
#       xlab <- "Covariate"
#
#       # rename interaction reference level
#       ints <- grep("_x_", dat$x)
#       if (length(ints) == 2) { # interaction with quadratic term
#         cov <- gsub("_x_.*", "", dat$x[ints])
#         newname <- grep(paste0(cov, collapse = "|"), dat$x)[1:2]
#         newname1 <- paste0(dat$x[newname], "_x_reference")
#         dat$x[newname] <- newname1
#
#       } else { # interaction with linear term
#         cov <- gsub("_x_.*", "", dat$x[ints])
#         newname <- grep(paste0(cov, collapse = "|"), dat$x)[1]
#         newname1 <- paste0(dat$x[newname], "_x_reference")
#         dat$x[newname] <- newname1
#       }
#
#       dat <- dat %>%
#         mutate(cov1 = gsub("2", "", covariate),
#                tmp = gsub("_x_.*", "", covariate),
#                quad = case_when(substr(tmp, nchar(tmp), nchar(tmp)) == 2 ~ "^2",
#                                 T ~ "")) %>%
#         left_join(cov.labs, by = c("cov1" = "covariate")) %>%
#         mutate(x = paste0(Label, quad)) %>%
#         select(!tmp)
#
#
#     } else if (plot.group == "observation") {
#       dat <- out %>%
#         mutate(x = covariate,
#                lab = paste0(name, ", ", data.type))
#       xlab <- "Covariate"
#
#       dat <- dat %>%
#         mutate(cov1 = gsub("2", "", covariate),
#                quad = case_when(substr(covariate, nchar(covariate), nchar(covariate)) == 2 ~ "^2",
#                                 T ~ "")) %>%
#         left_join(cov.labs, by = c("cov1" = "covariate")) %>%
#         mutate(x = paste0(Label, quad)) %>%
#         mutate(x = case_when(x == "NA" ~ cov1,
#                              T ~ x))
#
#     } else if (plot.group == "dataset") {
#       dat <- out %>%
#         rename(x = name)
#       xlab <- "Dataset"
#     } else {
#       stop("plot.group must be 'process' or 'observation' or 'dataset'")
#     }
#
#
#     pl <- ggplot(dat) +
#       geom_hline(yintercept = 0) +
#       theme_bw() +
#       theme(axis.text.x = element_text(angle = 0,
#                                        hjust = 0.5,
#                                        vjust = 1)) +
#       scale_x_discrete(labels = scales::label_wrap(15)) +
#       geom_pointrange(aes(x = x, y = mean, ymax = hi, ymin = lo,
#                           col = as.factor(block.out)),
#                       position = position_dodge(width = 0.5)) +
#       scale_color_manual(values = blockcols) +
#       labs(y = 'Estimate', x = xlab, color = "Excluded block",
#            title = title,
#            subtitle = "Mean and 95% credible intervals")
#
#     if (plot.group == "observation") {
#       pl <- pl + facet_wrap(~lab, scales = "free")
#     }
#
#     if (plot.group == "process") letter <- "a"
#     if (plot.group == "dataset") letter <- "b"
#     if (plot.group == "observation") letter <- "c"
#
#     ggsave(pl, file = paste0(path, "/7_CV-", letter, "_parameters-", plot.group, ".jpg"), height = 6, width = 10)
#
#   }
#
#
#
#
#
#
#
# }
