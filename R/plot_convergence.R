#' Plot chain convergence
#'
#' @param out (list) output from summarize_chains()
#'
#' @returns ggplot of convergence metrics
#' @export
#'
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr bind_rows filter select mutate case_when
#' @importFrom ggplot2 ggplot geom_rect aes geom_boxplot geom_jitter geom_hline facet_wrap theme_bw theme element_text labs


plot_convergence <- function(out) {
  tmp <- c()
  for (i in 2:(length(out))) {#-1)) { # I don't know why the -1 was there but I took it out so that obs.coef are included (CLS 4/3/25)
    tmp1 <- out[[i]] %>% mutate(type = names(out)[i])
    tmp <- bind_rows(tmp, tmp1)
  }

  tmp1 <- tmp %>%
            filter(.data$type %in% c("process.coef", "obs.coef", "alpha")) %>%
            select(!.data$name) %>%
            mutate(`Log(ESS)` = log(.data$ESS),
                   Rhat = .data$rhat,
                   type1 = case_when(type == "process.coef" ~ "Process \ncoefficients",
                                     type == "obs.coef" ~ "Observation \ncoefficients",
                                     type == "alpha" ~ "Dataset \nintercepts")) %>%
            pivot_longer(cols = c(.data$Rhat, .data$`Log(ESS)`))


  boxes <- data.frame(name = c("Log(ESS)", "Rhat"),
                      xmin = c(0.5, 0.5),
                      xmax = c(3.5, 3.5),
                      ymin = c(0, 1.1),
                      ymax = c(5.29, max(c(tmp$rhat, 1.1), na.rm = T)))


  ggplot() +
    geom_rect(data = boxes,
              aes(xmin = .data$xmin, xmax = .data$xmax,
                  ymin = .data$ymin, ymax = .data$ymax),
              fill = "red", alpha = 0.2) +
    geom_boxplot(data = tmp1, aes(x = .data$type1, y = .data$value),
                 outlier.shape = NA) +
    geom_jitter(data = tmp1, aes(x = .data$type1, y = .data$value), width = 0.2) +
    geom_hline(data = data.frame(yint = 1.1, name = "Rhat"),
               aes(yintercept = .data$yint), linetype = "solid", color = "red") +
    geom_hline(data = data.frame(yint = 5.29, name = "Log(ESS)"),
               aes(yintercept = .data$yint), linetype = "solid", color = "red") +
    facet_wrap(~name, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(hjust = 0.5,
                                     vjust = 1)) +
    labs(x = "Parameter", y = "Value",
                  title = "Red shaded area means chains may not have converged")

  }
