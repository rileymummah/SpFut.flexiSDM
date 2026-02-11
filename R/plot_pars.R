#' Plot parameter estimates
#'
#' @param out (data.frame) output from summarize_chains() containing correct parameters
#' @param plot.type (character) which type of plot to make; options are "full" or "cv"; defaults to "full
#' @param cov.labs (data.frame) dataframe containing covariate column names ("covariate") and labels ("Label")
#'
#' @returns A plot of parameter estimates
#' @export
#'
#' @importFrom ggplot2 ggplot geom_hline theme_bw theme scale_x_discrete geom_point geom_segment labs facet_wrap
#' @importFrom dplyr mutate left_join select left_join rename
#' @importFrom stringr str_wrap


plot_pars <- function(out,
                      plot.type = "full",
                      cov.labs) {

  if ("list" %in% class(out)) stop("'out' must be a dataframe element from the summarize_samples() output")

  tmp <- colnames(out)
  if ("covariate" %in% tmp & "data.type" %in% tmp) {
    plot.group <- "observation"
    title <- "Observation parameter estimates"
  } else if ("covariate" %in% tmp) {
    plot.group <- "process"
    title <- "Process parameter estimates"
  } else if ("data.type" %in% tmp) {
    plot.group <- "dataset"
    title <- "Dataset intercept estimates"
  } else {
    plot.group <- "tau"
    title <- "Tau estimate"
  }

  blockcols <- c("none" = "black", "1" = "#e79f1e", "2" = "#009e73", "3" = "#cb79a8")


  if (plot.group == "process") {
    dat <- mutate(out, x = .data$covariate)
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

    if ("covariate" %in% colnames(cov.labs) == F) stop ("cov.labs must have 'covariate' column that matches covariates used in the model")
    if ("Label" %in% colnames(cov.labs) == F) stop ("cov.labs must have 'Label' column with desired covariate labels")


    dat <- dat %>%
      mutate(cov1 = gsub("2", "", .data$covariate),
             tmp = gsub("_x_.*", "", .data$covariate),
             quad = case_when(substr(tmp, nchar(tmp), nchar(tmp)) == 2 ~ "^2",
                              T ~ "")) %>%
      left_join(cov.labs, by = c("cov1" = "covariate")) %>%
      mutate(x = paste0(.data$Label, .data$quad)) %>%
      select(!"tmp")


  } else if (plot.group == "observation") {
    dat <- mutate(out, x = .data$covariate, lab = paste0(.data$name, ", ", .data$data.type))
    xlab <- "Covariate"

    dat <- dat %>%
      mutate(cov1 = gsub("2", "", .data$covariate),
             quad = case_when(substr(.data$covariate,
                                     nchar(.data$covariate),
                                     nchar(.data$covariate)) == 2 ~ "^2",
                              T ~ "")) %>%
      left_join(cov.labs, by = c("cov1" = "covariate")) %>%
      mutate(x = paste0(.data$Label, .data$quad)) %>%
      mutate(x = case_when(x == "NA" ~ cov1, T ~ x))

  } else if (plot.group == "dataset") {
    dat <- rename(out, x = "name")
    xlab <- "Dataset"
  } else if (plot.group == "tau") {
    dat <- mutate(out, x = "tau")
    xlab <- "Tau"
  } else {
    stop("plot.group must be 'process' or 'observation' or 'dataset'")
  }

  dat$x <- gsub("^2", "\u00B2", dat$x, fixed = T)


  if (plot.type == "full") {
    dat <- filter(dat, .data$block.out == "none")
    if (nrow(dat) == 0) stop("No information on this parameter")

    pl <- ggplot(dat) +
      geom_hline(yintercept = 0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0,
                                       hjust = 0.5,
                                       vjust = 1)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      geom_pointrange(aes(x = .data$x, y = .data$mean,
                          ymax = .data$hi, ymin = .data$lo),
                      position = position_dodge(width = 0.5)) +
      labs(y = 'Estimate', x = xlab,
           title = title,
           subtitle = "Mean and 95% credible intervals")

  } else if (plot.type == "cv") {
    if (nrow(dat) == 0) stop("No information on this parameter")

    pl <- ggplot(dat) +
      geom_hline(yintercept = 0) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 0,
                                       hjust = 0.5,
                                       vjust = 1)) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      geom_pointrange(aes(x = .data$x, y = .data$mean,
                          ymax = .data$hi, ymin = .data$lo,
                          col = as.factor(.data$block.out)),
                      position = position_dodge(width = 0.5)) +
      scale_color_manual(values = blockcols) +
      labs(y = 'Estimate', x = xlab, color = "Excluded fold",
           title = title,
           subtitle = "Mean and 95% credible intervals")

  } else {stop("plot.type must be 'full' or 'cv'")}



  if (plot.group == "observation") {
    pl <- pl + facet_wrap(~lab, scales = "free")
  }

  dat <- select(dat, !any_of(c("cov1", "quad", "Label")))
  

  out <- list(dat = dat,
              plot = pl)
  return(out)

}
