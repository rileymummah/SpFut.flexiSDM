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
#' @examples


plot_pars <- function(out,
                      plot.group = "process",
                      plot.type = "full",
                      cov.labs,
                      title) {
  
  
  if (plot.type == "full") {
    
    if (plot.group == "process") {
      
      dat <- out$process.coef %>%
        mutate(x = covariate)
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
        mutate(cov1 = gsub("2", "", covariate),
               tmp = gsub("_x_.*", "", covariate),
               quad = case_when(substr(tmp, nchar(tmp), nchar(tmp)) == 2 ~ "^2",
                                T ~ "")) %>%
        left_join(cov.labs, by = c("cov1" = "covariate")) %>%
        mutate(x = paste0(Label, quad)) %>%
        select(!tmp)
      
    } else if (plot.group == "observation") {
      dat <- out$obs.coef %>%
        mutate(x = covariate,
               lab = paste0(name, " (", data.type, ')'))
      xlab <- "Covariate"
      
      
      dat <- dat %>%
        mutate(cov1 = gsub("2", "", covariate),
               quad = case_when(substr(covariate, nchar(covariate), nchar(covariate)) == 2 ~ "^2",
                                T ~ "")) %>%
        left_join(cov.labs, by = c("cov1" = "covariate")) %>%
        mutate(x = paste0(Label, quad)) %>%
        mutate(x = case_when(x == "NA" ~ cov1,
                             T ~ x))
      
      
    } else if (plot.group == "dataset") {
      dat <- out$alpha %>%
        rename(x = name)
      xlab <- "Dataset"
    } else {
      stop("plot.group must be 'process' or 'observation' or 'dataset'")
    }
    
    
    pl <- ggplot(dat) +
      geom_hline(yintercept = 0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0,
                                       hjust = 0.5,
                                       vjust = 1)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      geom_point(aes(x = x, y = mean), col='black') +
      geom_segment(aes(x = x, xend = x,
                       y = lo, yend = hi), col='black') +
      labs(y = 'Estimate', x = xlab,
           title = title,
           subtitle = "Mean and 95% credible intervals")
    
    
    if (plot.group == "observation") {
      pl <- pl + facet_wrap(~lab, scales = "free")
    }
    
    
  }
  
  if (plot.type == "cv") {
    
    if (plot.group == "process") {
      dat <- out %>%
        mutate(x = covariate)
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
        mutate(cov1 = gsub("2", "", covariate),
               tmp = gsub("_x_.*", "", covariate),
               quad = case_when(substr(tmp, nchar(tmp), nchar(tmp)) == 2 ~ "^2",
                                T ~ "")) %>%
        left_join(cov.labs, by = c("cov1" = "covariate")) %>%
        mutate(x = paste0(Label, quad)) %>%
        select(!tmp)
      
      
    } else if (plot.group == "observation") {
      dat <- out %>%
        mutate(x = covariate,
               lab = paste0(name, ", ", data.type))
      xlab <- "Covariate"
      
      dat <- dat %>%
        mutate(cov1 = gsub("2", "", covariate),
               quad = case_when(substr(covariate, nchar(covariate), nchar(covariate)) == 2 ~ "^2",
                                T ~ "")) %>%
        left_join(cov.labs, by = c("cov1" = "covariate")) %>%
        mutate(x = paste0(Label, quad)) %>%
        mutate(x = case_when(x == "NA" ~ cov1,
                             T ~ x))
      
    } else if (plot.group == "dataset") {
      dat <- out %>%
        rename(x = name)
      xlab <- "Dataset"
    } else {
      stop("plot.group must be 'process' or 'observation' or 'dataset'")
    }
    
    
    pl <- ggplot(dat) +
      geom_hline(yintercept = 0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0,
                                       hjust = 0.5,
                                       vjust = 1)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      geom_pointrange(aes(x = x, y = mean, ymax = hi, ymin = lo,
                          col = as.factor(block.out)),
                      position = position_dodge(width = 0.5)) +
      scale_color_manual(values = blockcols) +
      labs(y = 'Estimate', x = xlab, color = "Excluded block",
           title = title,
           subtitle = "Mean and 95% credible intervals")
    
    if (plot.group == "observation") {
      pl <- pl + facet_wrap(~lab, scales = "free")
    }
    
    
  }
  
  
  
  
  return(pl)
  
  
}
