#' Plot parameter estimates
#'
#' @param out (list) output from summarize_chains()
#' @param plot.group (character) which type of parameters to plot; options are "process", "observation", or "dataset"; defaults to "process"
#' @param plot.type (character) which type of plot to make; options are "full" or "cv"; defaults to "full
#' @param cov.labs (data.frame) dataframe containing covariate column names ("covariate") and labels ("Label")
#' @param title (character) title for plot
#'
#' @returns A plot of parameter estimates
#' @export
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom stringr str_wrap
#'
#' @examples


plot_pars <- function(out,
                      plot.group = "process",
                      plot.type = "full",
                      cov.labs,
                      title) {


  if (plot.type == "full") {

    if (plot.group == "process") {

      dat <- out$process.coef %>% dplyr::mutate(x = covariate)
      xlab <- "Covariate"

      # rename interaction reference level
      ints <- grep("_x_", dat$x)
      if (length(ints) == 2) { # interaction with quadratic term
        cov <- gsub("_x_.*", "", dat$x[ints])
        newname <- grep(paste0(cov, collapse = "|"), dat$x)[1:2]
        newname1 <- paste0(dat$x[newname], "_x_reference")
        dat$x[newname] <- newname1

      } else if (length(ints) == 1) { # interaction with linear term
        cov <- gsub("_x_.*", "", dat$x[ints])
        newname <- grep(paste0(cov, collapse = "|"), dat$x)[1]
        newname1 <- paste0(dat$x[newname], "_x_reference")
        dat$x[newname] <- newname1
      }

      dat <- dat %>%
              dplyr::mutate(cov1 = gsub("2", "", covariate),
                            tmp = gsub("_x_.*", "", covariate),
                            quad = dplyr::case_when(substr(tmp, nchar(tmp),
                                                           nchar(tmp)) == 2 ~ "^2",
                                                    T ~ "")) %>%
              dplyr::left_join(cov.labs, by = c("cov1" = "covariate")) %>%
              dplyr::mutate(x = paste0(Label, quad)) %>%
              dplyr::select(!tmp)

    } else if (plot.group == "observation") {
      dat <- out$obs.coef %>%
              dplyr::mutate(x = covariate,
                            lab = paste0(name, " (", data.type, ')'))
      xlab <- "Covariate"


      dat <- dat %>%
              dplyr::mutate(cov1 = gsub("2", "", covariate),
                            quad = dplyr::case_when(substr(covariate, nchar(covariate), nchar(covariate)) == 2 ~ "^2",
                                                    T ~ "")) %>%
              dplyr::left_join(cov.labs, by = c("cov1" = "covariate")) %>%
              dplyr::mutate(x = paste0(Label, quad)) %>%
              dplyr::mutate(x = dplyr::case_when(x == "NA" ~ cov1, T ~ x))


    } else if (plot.group == "dataset") {
      dat <- out$alpha %>% dplyr::rename(x = name)
      xlab <- "Dataset"
    } else {
      stop("plot.group must be 'process' or 'observation' or 'dataset'")
    }


    pl <- ggplot2::ggplot(dat) +
            ggplot2::geom_hline(yintercept = 0) +
            ggplot2::theme_bw() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,
                                                               hjust = 0.5,
                                                               vjust = 1)) +
            ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 15)) +
            ggplot2::geom_point(ggplot2::aes(x = x, y = mean), col='black') +
            ggplot2::geom_segment(ggplot2::aes(x = x, xend = x,
                                               y = lo, yend = hi), col='black') +
            ggplot2::labs(y = 'Estimate', x = xlab, title = title,
                          subtitle = "Mean and 95% credible intervals")


    if (plot.group == "observation") {
      pl <- pl + ggplot2::facet_wrap(~lab, scales = "free")
    }


  }

  if (plot.type == "cv") {
    blockcols <- c("none" = "black", "1" = "#e79f1e", "2" = "#009e73", "3" = "#cb79a8")


    if (plot.group == "process") {
      dat <- dplyr::mutate(out, x = covariate)
      xlab <- "Covariate"

      # rename interaction reference level
      ints <- grep("_x_", dat$x)
      if (length(ints) == 2) { # interaction with quadratic term
        cov <- gsub("_x_.*", "", dat$x[ints])
        newname <- grep(paste0(cov, collapse = "|"), dat$x)[1:2]
        newname1 <- paste0(dat$x[newname], "_x_reference")
        dat$x[newname] <- newname1

      } else { # interaction with linear term
        cov <- gsub("_x_.*", "", dat$x[ints])
        newname <- grep(paste0(cov, collapse = "|"), dat$x)[1]
        newname1 <- paste0(dat$x[newname], "_x_reference")
        dat$x[newname] <- newname1
      }

      dat <- dat %>%
              dplyr::mutate(cov1 = gsub("2", "", covariate),
                            tmp = gsub("_x_.*", "", covariate),
                            quad = dplyr::case_when(substr(tmp, nchar(tmp), nchar(tmp)) == 2 ~ "^2",
                                                    T ~ "")) %>%
              dplyr::left_join(cov.labs, by = c("cov1" = "covariate")) %>%
              dplyr::mutate(x = paste0(Label, quad)) %>%
              dplyr::select(!tmp)


    } else if (plot.group == "observation") {
      dat <- dplyr::mutate(out, x = covariate, lab = paste0(name, ", ", data.type))
      xlab <- "Covariate"

      dat <- dat %>%
              dplyr::mutate(cov1 = gsub("2", "", covariate),
                            quad = case_when(substr(covariate, nchar(covariate), nchar(covariate)) == 2 ~ "^2",
                                             T ~ "")) %>%
              dplyr::left_join(cov.labs, by = c("cov1" = "covariate")) %>%
              dplyr::mutate(x = paste0(Label, quad)) %>%
              dplyr::mutate(x = case_when(x == "NA" ~ cov1, T ~ x))

    } else if (plot.group == "dataset") {
      dat <- rename(out, x = name)
      xlab <- "Dataset"
    } else {
      stop("plot.group must be 'process' or 'observation' or 'dataset'")
    }


    pl <- ggplot2::ggplot(dat) +
            ggplot2::geom_hline(yintercept = 0) +
            ggplot2::theme_bw() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0,
                                                               hjust = 0.5,
                                                               vjust = 1)) +
            ggplot2::scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
            ggplot2::geom_pointrange(ggplot2::aes(x = x, y = mean, ymax = hi, ymin = lo,
                                                  col = as.factor(block.out)),
                                     position = ggplot2::position_dodge(width = 0.5)) +
            ggplot2::scale_color_manual(values = blockcols) +
            ggplot2::labs(y = 'Estimate', x = xlab, color = "Excluded fold",
                          title = title,
                          subtitle = "Mean and 95% credible intervals")

    if (plot.group == "observation") {
      pl <- pl + ggplot2::facet_wrap(~lab, scales = "free")
    }


  }




  return(pl)


}
