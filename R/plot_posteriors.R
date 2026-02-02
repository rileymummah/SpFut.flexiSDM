#' Plot posterior distributions
#'
#' @param samples (list) output from nimble
#' @param data (list) list containing data that were input into nimble
#' @param constants (list) list containing constants that were input into nimble
#' @param cov.labs (data.frame) dataframe containing covariate column names ("covariate") and labels ("Label")
#' @param cutoff (numeric) where to cut off chains for plotting; defaults to 0
#' @param plot (character) which parameter to plot; defaults to "B"; options are "B" and "alpha"
#' @param Bprior (character) distribution for beta priors; defaults to "dnorm(0,1)"
#' @param chaincols (vector) colors to use for chains; defaults to pink, green, and blue
#'
#' @returns ggplot object with plotted posterior distributions
#' @export
#'
#' @importFrom rstan Rhat
#' @importFrom stats rnorm runif
#' @importFrom ggplot2 ggplot geom_density facet_wrap labs scale_fill_manual scale_color_manual theme_bw
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate bind_rows full_join left_join select


plot_posteriors <- function(samples,
                            data,
                            constants,
                            cov.labs,
                            cutoff = 0,
                            Bprior = "dnorm(0,1)",
                            plot = "B",
                            chaincols = c("1" = "hotpink1", "2" = "olivedrab3", "3" = "deepskyblue3")) {


  if ("covariate" %in% colnames(cov.labs) == F) stop ("cov.labs must have 'covariate' column that matches covariates used in the model")
  if ("Label" %in% colnames(cov.labs) == F) stop ("cov.labs must have 'Label' column with desired covariate labels")


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


    rhat <- unlist(lapply(all, Rhat))
    bnames$rhat <- round(rhat, 2)


    # plot chains
    call <- c()
    for (c in chains) {
      c1 <- as.data.frame(samples[[c]][,bind]) %>%
              mutate(n = 1:n()) %>%
              pivot_longer(!n) %>%
              mutate(chain = c)
      call <- bind_rows(call, c1)
    }

    # If there's only one, need to rename it
    if (length(bind) == 1) {
      call$name <- bnames$param[1]
    }

    call <- full_join(call, bnames, by = c("name" = "param"))

    if (plot == "B") {
      call <- call %>%
              mutate(cov1 = gsub("2", "", .data$name.y),
                     tmp = gsub("_x_.*", "", .data$name.y),
                     quad = case_when(substr(tmp, nchar(tmp), nchar(tmp)) == 2 ~ "^2", T ~ "")) %>%
              left_join(cov.labs, by = c("cov1" = "covariate")) %>%
              mutate(name.y = paste0(.data$Label, .data$quad)) %>%
              select(!"tmp")
    }


    call <- mutate(call, lab = paste0(.data$name.y, "\nRhat = ", rhat))

    pl <- ggplot(data = filter(call, n > cutoff)) +
            geom_density(aes(x = .data$value, color = as.factor(.data$chain),
                             fill = as.factor(.data$chain)), alpha = 0.1) +
            facet_wrap(~lab, scales = "free") +
            labs(y = "Density", x = "Parameter value", color = "Chain",
                 fill = "Chain", subtitle = "Posterior distribution after burnin") +
            scale_fill_manual(values = chaincols) +
            scale_color_manual(values = chaincols) +
            theme_bw()

    if (plot %in% c("B", "alpha")) {

      if (plot == "B") {

        Bprior1 <- gsub(" ", "", Bprior)
        Bpriordist <- substr(Bprior1, 1, 5)
        Bpriorvar1 <- as.numeric(substr(Bprior, 7, 7))
        Bpriorvar2 <- as.numeric(substr(Bprior, 9, 9))


        if (Bpriordist == "dnorm") {
          prior <- rnorm(nrow(call), Bpriorvar1, Bpriorvar2)
        } else if (Bpriordist == "dunif") {
          prior <- runif(nrow(call), Bpriorvar1, Bpriorvar2)
        }
      } else if (plot == "alpha") {
        prior <- rnorm(nrow(call), 0, 1)
        prior <- exp(prior)
      }



      call <- call %>%
              mutate(prior = prior) %>%
              group_by(.data$name) %>%
              mutate(value1 = case_when(n > cutoff ~ value, T ~ NA),
                     min = min(.data$value1, na.rm = T) - 0.1,
                     max = max(.data$value1, na.rm = T) + 0.1) %>%
              mutate(prior = case_when(prior < min | prior > max ~ NA,
                                       T ~ prior)) %>%
        filter(is.na(prior) == F)


      pl <- pl + geom_density(data = call, aes(x = .data$prior),
                              fill = "gray", alpha = 0.5) +
            labs(y = "Density", x = "Parameter value", color = "Chain",
                 fill = "Chain",
                 subtitle = "Posterior distribution after burnin; black indicates prior distribution")
    }


    return(pl)


}
