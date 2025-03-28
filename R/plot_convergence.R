#' Plot chain convergence
#'
#' @param out (list) output from summarize_chains()
#'
#' @returns ggplot of convergence metrics
#' @export
#'
#' @examples



plot_convergence <- function(out) {
  tmp <- c()
  for (i in 2:(length(out)-1)) {
    tmp1 <- out[[i]] %>%
      mutate(type = names(out)[i])
    tmp <- bind_rows(tmp, tmp1)
  }

  tmp1 <- tmp %>%
    filter(type %in% c("process.coef", "obs.coef", "alpha")) %>%
    select(!name) %>%
    mutate(`Log(ESS)` = log(ESS),
           Rhat = rhat,
           type1 = case_when(type == "process.coef" ~ "Process \ncoefficients",
                             type == "obs.coef" ~ "Observation \ncoefficients",
                             type == "alpha" ~ "Dataset \nintercepts")) %>%
    pivot_longer(cols = c(Rhat, `Log(ESS)`))

  
  boxes <- data.frame(name = c("Log(ESS)", "Rhat"),
                      xmin = c(0.5, 0.5),
                      xmax = c(3.5, 3.5),
                      ymin = c(0, 1.1),
                      ymax = c(5.29, max(c(tmp$rhat, 1.1), na.rm = T)))
  
  
  ggplot() +
    geom_rect(data = boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "red", alpha = 0.2) +
    geom_boxplot(data = tmp1, aes(x = type1, y = value), outlier.shape = NA) +
    geom_jitter(data = tmp1, aes(x = type1, y = value), width = 0.2) +
    geom_hline(data = data.frame(yint = 1.1, name = "Rhat"), aes(yintercept = yint), linetype = "solid", color = "red") +
    geom_hline(data = data.frame(yint = 5.29, name = "Log(ESS)"), aes(yintercept = yint), linetype = "solid", color = "red") +
    facet_wrap(~name, scales = "free_y") +
    theme_bw() +
    theme(axis.text.x = element_text(hjust = 0.5,
                                     vjust = 1)) +
    labs(x = "Parameter", y = "Value", title = "Red shaded area means chains may not have converged")
}
