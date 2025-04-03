#' Plot chain convergence
#'
#' @param out (list) output from summarize_chains()
#'
#' @returns ggplot of convergence metrics
#' @export
#'
#' @importFrom tidyr pivot_longer
#' @import dplyr
#' @import ggplot2
#'
#' @examples



plot_convergence <- function(out) {
  tmp <- c()
  for (i in 2:(length(out))) {#-1)) { # I don't know why the -1 was there but I took it out so that obs.coef are included (CLS 4/3/25)
    tmp1 <- out[[i]] %>% dplyr::mutate(type = names(out)[i])
    tmp <- dplyr::bind_rows(tmp, tmp1)
  }

  tmp1 <- tmp %>%
            dplyr::filter(type %in% c("process.coef", "obs.coef", "alpha")) %>%
            dplyr::select(!name) %>%
            dplyr::mutate(`Log(ESS)` = log(ESS),
                           Rhat = rhat,
                           type1 = dplyr::case_when(type == "process.coef" ~ "Process \ncoefficients",
                                             type == "obs.coef" ~ "Observation \ncoefficients",
                                             type == "alpha" ~ "Dataset \nintercepts")) %>%
            tidyr::pivot_longer(cols = c(Rhat, `Log(ESS)`))


  boxes <- data.frame(name = c("Log(ESS)", "Rhat"),
                      xmin = c(0.5, 0.5),
                      xmax = c(3.5, 3.5),
                      ymin = c(0, 1.1),
                      ymax = c(5.29, max(c(tmp$rhat, 1.1), na.rm = T)))


  ggplot2::ggplot() +
    ggplot2::geom_rect(data = boxes,
                       ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                       fill = "red", alpha = 0.2) +
    ggplot2::geom_boxplot(data = tmp1, ggplot2::aes(x = type1, y = value),
                          outlier.shape = NA) +
    ggplot2::geom_jitter(data = tmp1, ggplot2::aes(x = type1, y = value),
                         width = 0.2) +
    ggplot2::geom_hline(data = data.frame(yint = 1.1, name = "Rhat"),
                        ggplot2::aes(yintercept = yint), linetype = "solid", color = "red") +
    ggplot2::geom_hline(data = data.frame(yint = 5.29, name = "Log(ESS)"),
                        ggplot2::aes(yintercept = yint), linetype = "solid", color = "red") +
    ggplot2::facet_wrap(~name, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 0.5,
                                     vjust = 1)) +
    ggplot2::labs(x = "Parameter", y = "Value",
                  title = "Red shaded area means chains may not have converged")

  }
