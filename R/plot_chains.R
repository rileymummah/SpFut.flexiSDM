#' #' Plot chains
#' #'
#' #' @param samples (list) output from nimble
#' #' @param data (list) list containing data that was input into nimble
#' #' @param constants (list) list containing constants that were input into nimble
#' #' @param cov.labs (data.frame) dataframe containing covariate column names ("covariate") and labels ("Label")
#' #' @param cutoff (numeric) where to cut off chains for plotting; defaults to 0
#' #' @param plot (character) which parameter to plot; defaults to "B"; options are "B" and "alpha"
#' #' @param chaincols (vector) colors to use for chains; defaults to pink, green, and blue
#' #'
#' #' @returns ggplot object with plotted chains
#' #' @export
#' #'
#' #' @importFrom rstan Rhat
#' #' @importFrom dplyr mutate bind_rows select full_join left_join
#' #' @importFrom tidyr pivot_longer
#' #' @importFrom ggplot2 ggplot geom_hline geom_line aes geom_vline facet_wrap labs scale_color_manual theme_bw
#' 
#' 
#' plot_chains <- function(samples,
#'                         data,
#'                         constants,
#'                         cov.labs,
#'                         cutoff = 0,
#'                         plot = "B",
#'                         chaincols = c("1" = "hotpink1", "2" = "olivedrab3", "3" = "deepskyblue3")) {
#' 
#'   
#'   
#'     if (plot == "B") {
#'       bnames <- data.frame(name = colnames(data$Xz),
#'                            param = paste0("B[", 1:ncol(data$Xz), "]"))
#'       bind <- grep("B", colnames(samples[[1]]))
#'       rm <- grep("XB", colnames(samples[[1]]))
#'       bind <- bind[-which(bind %in% rm)]
#' 
#'     } else if (plot == "alpha") {
#'       bind <- grep("alpha", colnames(samples[[1]]))
#'       bnames <- data.frame(name = unlist(constants[grep("name", names(constants))]),
#'                            param = paste0("alpha[", 1:length(bind), "]"))
#'       
#'     } else if (plot == "tau") {
#'       bind <- grep("tau", colnames(samples[[1]]))
#'       bnames <- data.frame(name = "tau",
#'                            param = "tau[1]")
#'       
#'       if(length(bind) == 0) stop("The model does not include a spatial random effect, therefore tau is not estimated")
#'       
#'     } else if (plot == "observation") {
#'       bind <- grep("A|C|D", colnames(samples[[1]]))
#'       xind <- grep("Xw|Xy|Xv", names(data))
#'       nameind <- grep("name", names(constants))
#'       
#'       # get names
#'       xnames <- c()
#'       for (x in 1:length(xind)) {
#'         num <- as.numeric(substr(names(data)[[xind[x]]], 3, 3))
#'         
#'         tmp <- colnames(data[[xind[x]]])
#'         tmp1 <- constants[[nameind[num]]]
#'         
#'         xnames <- c(xnames, paste0(tmp1, ": ", tmp))
#'       }
#'       
#'       
#'       bnames <- data.frame(name = xnames,
#'                            param = colnames(samples[[1]])[bind])
#'     }
#' 
#' 
#'     chains <- 1:3
#' 
#'     # calculate rhat
#'     all <- list()
#'     for (i in 1:length(bind)) {
#'       df <- as.matrix(data.frame(chain1 = as.vector(samples[[1]][,bind[i]]),
#'                                  chain2 = as.vector(samples[[2]][,bind[i]]),
#'                                  chain3 = as.vector(samples[[3]][,bind[i]])))
#'       df <- df[cutoff:nrow(df), chains]
#'       all[[i]] <- df
#'     }
#' 
#' 
#'     rhat <- unlist(lapply(all, Rhat))
#'     bnames$rhat <- round(rhat, 2)
#' 
#' 
#'     # plot chains
#'     call <- c()
#'     for (c in chains) {
#'       c1 <- as.data.frame(samples[[c]][,bind]) %>%
#'               mutate(n = 1:n()) %>%
#'               pivot_longer(!n) %>%
#'               mutate(chain = c)
#'       call <- bind_rows(call, c1)
#'     }
#' 
#'     # If there's only one, need to rename it
#'     if (length(bind) == 1) {
#'       call$name <- paste0(plot, "[", 1, "]")
#'     }
#' 
#'     call <- full_join(call, bnames, by = c("name" = "param"))
#' 
#'     if (plot == "B") {
#' 
#'       if ("covariate" %in% colnames(cov.labs) == F) stop ("cov.labs must have 'covariate' column that matches covariates used in the model")
#'       if ("Label" %in% colnames(cov.labs) == F) stop ("cov.labs must have 'Label' column with desired covariate labels")
#'       
#'       call <- call %>%
#'                 mutate(cov1 = gsub("2", "", .data$name.y),
#'                               tmp = gsub("_x_.*", "", .data$name.y),
#'                               quad = case_when(substr(.data$tmp, nchar(.data$tmp),
#'                                                       nchar(.data$tmp)) == 2 ~ "^2",
#'                                                T ~ "")) %>%
#'                 left_join(cov.labs, by = c("cov1" = "covariate")) %>%
#'                 mutate(name.y = paste0(.data$Label, .data$quad)) %>%
#'                 select(!"tmp")
#'     }
#' 
#'     call <- mutate(call, lab = paste0(.data$name.y, "\nRhat = ", rhat))
#'     call$lab <- gsub("^2", "\u00B2", call$lab, fixed = T)
#' 
#'     pl <- ggplot(call) +
#'             geom_hline(yintercept = 0) +
#'             geom_line(aes(x = .data$n, y = .data$value,
#'                           color = as.factor(.data$chain)),
#'                       linewidth = 0.1) +
#'             geom_vline(xintercept = cutoff) +
#'             facet_wrap(~lab, scales = "free_y") +
#'             labs(x = "Iteration", y = "Parameter value",
#'                           color = "Chain",
#'                  subtitle = "Black vertical line indicates burnin") +
#'             scale_color_manual(values = chaincols) +
#'             theme_bw()
#' 
#'     out <- list(dat = call,
#'                 plot = pl)
#'     
#'     return(out)
#' 
#' }
