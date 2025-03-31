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
        mutate(n = 1:nrow(.)) %>%
        pivot_longer(!n) %>%
        mutate(chain = c)
      call <- bind_rows(call, c1)
    }
    
    # If there's only one, need to rename it
    if (length(bind) == 1) {
      call$name <- paste0(d, "[", 1, "]")
    }
    
    call <- call %>%
      full_join(bnames, by = c("name" = "param"))
    
    if (plot == "B") {
      call <- call %>%
        mutate(cov1 = gsub("2", "", name.y),
               tmp = gsub("_x_.*", "", name.y),
               quad = case_when(substr(tmp, nchar(tmp), nchar(tmp)) == 2 ~ "^2",
                                T ~ "")) %>%
        left_join(cov.labs, by = c("cov1" = "covariate")) %>%
        mutate(name.y = paste0(Label, quad)) %>%
        select(!tmp)
    }
    
    
    call <- call %>%
      mutate(lab = paste0(name.y, "\nRhat = ", rhat))
    
    pl <- ggplot(data = filter(call, n > cutoff)) +
      geom_density(aes(x = value, color = as.factor(chain), fill = as.factor(chain)), alpha = 0.1) +
      facet_wrap(~lab, scales = "free") +
      labs(y = "Density", x = "Parameter value", color = "Chain", fill = "Chain", subtitle = "Posterior distribution after burnin") +
      scale_fill_manual(values = chaincols) +
      scale_color_manual(values = chaincols) +
      theme_bw()

    if (plot %in% c("B", "alpha")) {

      if (plot == "B") {
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
        group_by(name) %>%
        mutate(value1 = case_when(n > cutoff ~ value,
                                  T ~ NA),
               min = min(value1, na.rm = T) - 0.1,
               max = max(value1, na.rm = T) + 0.1) %>%
        mutate(prior = case_when(prior < min | prior > max ~ NA,
                                 T ~ prior))


      pl <- pl + geom_density(data = call, aes(x = prior), fill = "gray", alpha = 0.5) +
        labs(y = "Density", x = "Parameter value", color = "Chain", fill = "Chain", subtitle = "Posterior distribution after burnin; black indicates prior distribution")
    }


    return(pl)


}
