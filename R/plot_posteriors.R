#' Plot posterior distributions
#'
#' @param samples (list) output from nimble
#' @param data (list) list containing data that was input into nimble
#' @param cov.labs (data.frame) dataframe containing covariate column names ("covariate") and labels ("Label")
#' @param cutoff (numeric) where to cut off chains for plotting; defaults to 0
#' @param plot (character) which parameter to plot; defaults to "B"; options are "B" and "alpha"
#' @param Bpriordist (character) distribution for beta priors; defaults to "dnorm"; options are "dnorm" or "dunif"
#' @param Bpriorvar1 (numeric) first parameter for beta priors; defaults to 0
#' @param Bpriorvar2 (numeric) second parameter for beta priors; defaults to 1
#' @param chaincols (vector) colors to use for chains; defaults to pink, green, and blue
#'
#' @returns ggplot object with plotted posterior distributions
#' @export
#'
#' @importFrom rstan Rhat
#' @importFrom stats rnorm runif
#' @import ggplot2 dplyr
#'
#' @examples



plot_posteriors <- function(samples,
                        data,
                        cov.labs,
                        cutoff = 0,
                        plot = "B",
                        Bpriordist = "dnorm",
                        Bpriorvar1 = 0,
                        Bpriorvar2 = 1,
                        chaincols = c("1" = "hotpink1", "2" = "olivedrab3", "3" = "deepskyblue3")) {


    if (plot == "B") {
      bnames <- data.frame(name = colnames(data$Xz),
                           param = paste0("B[", 1:ncol(data$Xz), "]"))
      bind <- grep("B", colnames(samples[[1]]))
      rm <- grep("XB", colnames(samples[[1]]))
      bind <- bind[-which(bind %in% rm)]

    } else if (plot == "alpha") {
      bind <- grep("alpha", colnames(samples[[1]]))
      bnames <- data.frame(name = unlist(constants[grep("name", names(constants))]),
                           param = paste0("alpha[", 1:length(bind), "]"))
    }

    chains <- 1:3

    # calculate rhat
    all <- list()
    for (i in 1:length(bind)) {
      df <- as.matrix(data.frame(chain1 = as.vector(samples[[1]][,bind[i]]),
                                 chain2 = as.vector(samples[[2]][,bind[i]]),
                                 chain3 = as.vector(samples[[3]][,bind[i]])))
      df <- df[cutoff:nrow(df), chains]
      all[[i]] <- df
    }


    rhat <- unlist(lapply(all, rstan::Rhat))
    bnames$rhat <- round(rhat, 2)


    # plot chains
    call <- c()
    for (c in chains) {
      c1 <- as.data.frame(samples[[c]][,bind]) %>%
              dplyr::mutate(n = 1:nrow(.)) %>%
              tidyr::pivot_longer(!n) %>%
              dplyr::mutate(chain = c)
      call <- dplyr::bind_rows(call, c1)
    }

    # If there's only one, need to rename it
    if (length(bind) == 1) {
      call$name <- paste0(d, "[", 1, "]")
    }

    call <- dplyr::full_join(call, bnames, by = c("name" = "param"))

    if (plot == "B") {
      call <- call %>%
              dplyr::mutate(cov1 = gsub("2", "", name.y),
                            tmp = gsub("_x_.*", "", name.y),
                            quad = dplyr::case_when(substr(tmp, nchar(tmp), nchar(tmp)) == 2 ~ "^2",
                                                    T ~ "")) %>%
              dplyr::left_join(cov.labs, by = c("cov1" = "covariate")) %>%
              dplyr::mutate(name.y = paste0(Label, quad)) %>%
              dplyr::select(!tmp)
    }


    call <- dplyr::mutate(call, lab = paste0(name.y, "\nRhat = ", rhat))

    pl <- ggplot2::ggplot(data = dplyr::filter(call, n > cutoff)) +
            ggplot2::geom_density(ggplot2::aes(x = value, color = as.factor(chain),
                                               fill = as.factor(chain)), alpha = 0.1) +
            ggplot2::facet_wrap(~lab, scales = "free") +
            ggplot2::labs(y = "Density", x = "Parameter value", color = "Chain",
                          fill = "Chain", subtitle = "Posterior distribution after burnin") +
            ggplot2::scale_fill_manual(values = chaincols) +
            ggplot2::scale_color_manual(values = chaincols) +
            ggplot2::theme_bw()

    if (plot %in% c("B", "alpha")) {

      if (plot == "B") {
        if (Bpriordist == "dnorm") {
          prior <- stats::rnorm(nrow(call), Bpriorvar1, Bpriorvar2)
        } else if (Bpriordist == "dunif") {
          prior <- stats::runif(nrow(call), Bpriorvar1, Bpriorvar2)
        }
      } else if (plot == "alpha") {
        prior <- stats::rnorm(nrow(call), 0, 1)
        prior <- exp(prior)
      }



      call <- call %>%
              dplyr::mutate(prior = prior) %>%
              dplyr::group_by(name) %>%
              dplyr::mutate(value1 = dplyr::case_when(n > cutoff ~ value,
                                                      T ~ NA),
                            min = min(value1, na.rm = T) - 0.1,
                            max = max(value1, na.rm = T) + 0.1) %>%
              dplyr::mutate(prior = dplyr::case_when(prior < min | prior > max ~ NA,
                                                     T ~ prior))


      pl <- pl + ggplot2::geom_density(data = call, ggplot2::aes(x = prior), fill = "gray", alpha = 0.5) +
            ggplot2::labs(y = "Density", x = "Parameter value", color = "Chain",
                          fill = "Chain", subtitle = "Posterior distribution after burnin; black indicates prior distribution")
    }


    return(pl)


}
