#' Plot marginal effects of process parameters
#'
#' @param data (list) output from data_for_nimble()
#' @param out (list) output from summarize_chains()
#' @param breaks (numeric) distance between covariate values to simulate
#' @param unscale_covar (data.frame) data.frame containing unscaled covariates, or FALSE
#'
#' @returns plot of marginal effects
#' @export
#'
#' @importFrom stats quantile
#' @importFrom magrittr "%>%"
#' @importFrom dplyr bind_rows case_when inner_join mutate
#' @importFrom utils read.csv
#' @importFrom ggplot2 ggplot geom_hline geom_ribbon aes geom_line facet_wrap theme_bw labs scale_fill_manual scale_color_manual


plot_effects <- function(data,
                         out,
                         breaks = 0.01,
                         unscale_covar = F) {

  # Start with process covariates
  ball <- out$process.coef

  # Get names of covariates, excluding intercepts, squares, and interactions
  covs <- colnames(data$Xz)
  rm <- grep("Z1|2|_x_", covs)
  if (length(rm) > 0) covs <- covs[-rm]


  # For each covariate, produce marginal effect curve
  all <- c()
  all.labs <- c()
  for (n in 1:length(covs)) {
    cov <- covs[n]

    # get index of main effect
    ind <- grep(cov, ball$covariate, value = T)
    ind1 <- grep(cov, ball$covariate)
    ind1 <- ind1[ind == covs[n]]

    # get index of quadratic (if it exists)
    ind <- grep(paste0(cov, "2"), ball$covariate, value = T)
    ind2 <- grep(paste0(cov, "2"), ball$covariate)
    ind2 <- ind2[ind == paste0(cov, "2")]

    # get index of interactions (if it exists)
    ind3 <- grep(paste0(cov, "_x_"), ball$covariate, value = T)
    ind3 <- which(ball$covariate %in% ind3)

    # get index of interactions with quadratic (if it exists)
    ind4 <- grep(paste0(cov, "2_x_"), ball$covariate, value = T)
    ind4 <- which(ball$covariate %in% ind4)


    # get scaled covariate values
    q99 <- quantile(data$Xz[,cov], 0.99)
    data$Xz[,cov][data$Xz[,cov] > q99] <- q99
    s <- seq(min(data$Xz[,cov]), max(data$Xz[,cov]), breaks)


    # linear term with no interaction
    # b1 * x
    if (length(ind2) == 0 & length(ind3) == 0) {

      b <- as.numeric(ball[ind1, "mean"])
      blo <- as.numeric(ball[ind1, "lo"])
      bhi <- as.numeric(ball[ind1, "hi"])

      use <- data.frame(cov = cov,
                        x = s,
                        mean = exp(b * s),
                        hi = exp(bhi * s),
                        lo = exp(blo * s),
                        factor = "none")

      if (class(unscale_covar) == "data.frame") {
        # now get unscaled values
        mn <- mean(covar_unscaled[,cov])
        sd <- sd(covar_unscaled[,cov])

        use$x_unscaled <- (use$x * sd) + mn

        all <- bind_rows(all, use)
      }

      all <- bind_rows(all, use)
    }

    # linear term with interaction
    # b1 * x + b2 * r * x
    # r is 0 or 1
    if (length(ind2) == 0 & length(ind3) > 0) {

      main <- ind1
      int <- ind3

      b1 <- as.numeric(ball[main, "mean"])
      blo1 <- as.numeric(ball[main, "lo"])
      bhi1 <- as.numeric(ball[main, "hi"])

      b.int <- ball[ind3,]
      new <- data.frame(paste0(cov, "_x_", .data$covs.int.factor, ".", .data$reference), NA, 0, 0, 0, 0, 0, 0, 0)
      colnames(new) <- colnames(b.int)
      b.int <- rbind(b.int, new)


      use <- expand.grid(cov = cov,
                         factor = b.int$covariate,
                         x = s)

      # for each level of interaction factor, add values

      use$mean <- NA
      use$hi <- NA
      use$lo <- NA
      for (r in 1:nrow(use)) {
        use$mean[r] <- exp(b1 * use$x[r] +
                             b.int$mean[b.int$covariate == use$factor[r]] * use$x[r])
        use$hi[r] <- exp(bhi1 * use$x[r] +
                           b.int$hi[b.int$covariate == use$factor[r]] * use$x[r])
        use$lo[r] <- exp(blo1 * use$x[r] +
                           b.int$lo[b.int$covariate == use$factor[r]] * use$x[r])
      }

      use$factor <- gsub(".*[.]", "", use$factor)

      if (class(unscale_covar) == "data.frame") {
        # now get unscaled values
        mn <- mean(covar_unscaled[,cov])
        sd <- sd(covar_unscaled[,cov])

        use$x_unscaled <- (use$x * sd) + mn


        all <- bind_rows(all, use)
      }

      all <- bind_rows(all, use)
    }

    # quadratic term
    # b1 * x + b2 * x^2
    if (length(ind2) == 1 & length(ind3) == 0) {

      quad <- ind2
      main <- ind1

      b1 <- as.numeric(ball[main, "mean"])
      blo1 <- as.numeric(ball[main, "lo"])
      bhi1 <- as.numeric(ball[main, "hi"])

      b2 <- as.numeric(ball[quad, "mean"])
      blo2 <- as.numeric(ball[quad, "lo"])
      bhi2 <- as.numeric(ball[quad, "hi"])


      use <- data.frame(cov = cov,
                        x = s,
                        mean = exp(b1 * s + b2 * s^2),
                        hi = exp(bhi1 * s + bhi2 * s^2),
                        lo = exp(blo1 * s + blo2 * s^2),
                        factor = "none")

      if (class(unscale_covar) == "data.frame") {
        # now get unscaled values
        mn <- mean(covar_unscaled[,cov])
        sd <- sd(covar_unscaled[,cov])

        use$x_unscaled <- (use$x * sd) + mn


        all <- bind_rows(all, use)
      }

      all <- bind_rows(all, use)
    }

    # quadratic term with interaction
    # reference: b1 * x + b2 * x^2 + b3 * x * r + b4 * x^2 * r (r = 0)
    # nonreference: b1 * x + b2 * x^2 + b3 * x * r + b4 * x^2 * r (r = 1)
    if (length(ind2) == 1 & length(ind3) > 0) {

      main <- ind1
      quad <- ind2
      int <- ind3
      quadint <- ind4

      b1 <- as.numeric(ball[main, "mean"])
      blo1 <- as.numeric(ball[main, "lo"])
      bhi1 <- as.numeric(ball[main, "hi"])

      b2 <- as.numeric(ball[quad, "mean"])
      blo2 <- as.numeric(ball[quad, "lo"])
      bhi2 <- as.numeric(ball[quad, "hi"])



      # for each level of interaction factor, add values
      b.int <- ball[ind3,]
      new <- data.frame(paste0(cov, "_x_", .data$covs.int.factor, ".", .data$reference), NA, 0, 0, 0, 0, 0, 0, 0, 0)
      colnames(new) <- colnames(b.int)
      b.int <- rbind(b.int, new)

      b.quadint <- ball[ind4,]
      new <- data.frame(paste0(cov, "2_x_", .data$covs.int.factor, ".", .data$reference), NA, 0, 0, 0, 0, 0, 0, 0, 0)
      colnames(new) <- colnames(b.quadint)
      b.quadint <- rbind(b.quadint, new)

      use <- expand.grid(cov = cov,
                         factor = b.int$covariate,
                         x = s) %>%
        mutate(factor2 = gsub("_x_", "2_x_", factor))

      use$mean <- NA
      use$hi <- NA
      use$lo <- NA

      for (r in 1:nrow(use)) {
        use$mean[r] <- exp((b1 * use$x[r]) +
                       (b2 * use$x[r]) +
                       (b.int$mean[b.int$covariate == use$factor[r]] * use$x[r]) +
                       (b.quadint$mean[b.quadint$covariate == use$factor2[r]] * use$x[r]))
        use$hi[r] <- exp((bhi1 * use$x[r]) +
                     (bhi2 * use$x[r]) +
                     (b.int$hi[b.int$covariate == use$factor[r]] * use$x[r]) +
                     (b.quadint$hi[b.quadint$covariate == use$factor2[r]] * use$x[r]))
        use$lo[r] <- exp((blo1 * use$x[r]) +
                     (blo2 * use$x[r]) +
                     (b.int$lo[b.int$covariate == use$factor[r]] * use$x[r]) +
                     (b.quadint$lo[b.quadint$covariate == use$factor2[r]] * use$x[r]))
      }

      use$factor <- gsub(".*[.]", "", use$factor)
      use$factor2 <- NULL


      if (class(unscale_covar) == "data.frame") {
        # now get unscaled values
        mn <- mean(covar_unscaled[,cov])
        sd <- sd(covar_unscaled[,cov])

        use$x_unscaled <- (use$x * sd) + mn

        all <- bind_rows(all, use)
      }

      all <- bind_rows(all, use)

    }
  }

  all1 <- all %>%
    mutate(hi = case_when(hi > 5000 ~ 5000, T ~ hi),
           mean = case_when(mean > 5000 ~ 5000, T ~ mean),
           lo = case_when(lo > 5000 ~ 5000, T ~ lo))


  cov.labs <- read.csv("data/covariate-labels.csv")

  all1 <- inner_join(all1, cov.labs, by = c("cov" = "covariate")) %>%
            mutate(cov = .data$Label)


  if (class(unscale_covar) != "data.frame") {
    all1$xplot <- all1$x
    xlab <- "Scaled covariate value"
  } else {
    all1$xplot <- all1$x_unscaled
    xlab <- "Covariate value"
  }


  if (length(unique(all1$factor)) == 1) { # no interactions

    pl <- ggplot(filter(all1)) +
            geom_hline(yintercept = 0) +
            geom_ribbon(aes(x = .data$xplot, ymax = .data$hi, ymin = .data$lo),
                        alpha = 0.5) +
            geom_line(aes(x = .data$xplot, y = .data$mean)) +
            facet_wrap(~ cov, scales = "free") +
            theme_bw() +
            labs(x = xlab, y = "Exp(Estimate)",
                          fill = "Interaction",
                          title = "Marginal effects of process covariates",
                          subtitle = "Ribbon indicates 95% credible interval")

  } else { # interactions

    levs <- unique(all1$factor)[-which(unique(all1$factor == "none"))]
    all1$factor <- factor(all1$factor, levels = c("none", levs))

    groups <- paste0(as.vector(unique(all1$factor)), collapse = ", ")
    if (groups == c("none, east, west")) {
      pl <- ggplot(filter(all1)) +
              geom_hline(yintercept = 0) +
              geom_ribbon(aes(x = .data$xplot, ymax = .data$hi, ymin = .data$lo,
                              fill = .data$factor), alpha = 0.5) +
              geom_line(aes(x = .data$xplot, y = .data$mean, color = .data$factor)) +
              scale_fill_manual(values = c("none" = "gray20", "east" = "darkgreen", "west" = "goldenrod"),
                                labels = c("none" = "No Interaction", "east" = "East", "west" = "West")) +
              scale_color_manual(values = c("none" = "gray20", "east" = "darkgreen", "west" = "goldenrod"),
                                 labels = c("none" = "No Interaction", "east" = "East", "west" = "West")) +
              facet_wrap(~ cov, scales = "free") +
              theme_bw() +
              labs(x = xlab, y = "Exp(Estimate)",
                   color = "Interaction", fill = "Interaction",
                   title = "Marginal effects of process covariates",
                   subtitle = "Ribbon indicates 95% credible interval")

    } else {
      pl <- ggplot(filter(all1)) +
              geom_hline(yintercept = 0) +
              geom_ribbon(aes(x = .data$xplot, ymax = .data$hi, ymin = .data$lo,
                              fill = .data$factor), alpha = 0.5) +
              geom_line(aes(x = .data$xplot, y = .data$mean, color = .data$factor)) +
              facet_wrap(~ cov, scales = "free") +
              theme_bw() +
              labs(x = xlab, y = "Exp(Estimate)",
                   color = "Interaction", fill = "Interaction",
                   title = "Marginal effects of process covariates",
                   subtitle = "Ribbon indicates 95% credible interval")

    }

  }

  return(pl)

}
