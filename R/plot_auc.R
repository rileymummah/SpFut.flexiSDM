#' Plot AUC
#'
#' @param auc (data.frame) dataframe containing auc information
#' @param lines (logical) whether to plot lines connecting in- and out-of-sample AUC (T) or not (F)
#'
#' @returns ggplot of AUC
#' @export
#'
#' @importFrom scales label_wrap
#' @import ggplot2
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom dplyr case_when select mutate distinct


plot_auc <- function(auc,
                     lines) {

  blockcols <- c("none" = "black", "1" = "#e79f1e", "2" = "#009e73", "3" = "#cb79a8")


  auc1 <- pivot_longer(auc, cols = !c("source", "block")) %>%
    mutate(inout = case_when(name %in% c("AUCin", "AUCin.full", "in.cell", "in.full.cell", "in.full.n", "in.n") ~ "In sample",
                             T ~ "Out of sample"),
           type = case_when(name %in% c("AUCin", "AUCin.full", "AUCout", "AUCout.full") ~ "AUC",
                            name %in% c("in.cell", "in.full.cell", "out.cell", "out.full.cell") ~ "cells",
                            name %in% c("in.full.n", "in.n", "out.full.n", "out.n") ~ "samples"),
           full = case_when(name %in% c("AUCin.full", "AUCout.full", "in.full.cell", "in.full.n", "out.full.cell", "out.full.n") ~ "full",
                            T ~ "dataset")) %>%
    select(!"name") %>%
    pivot_wider(names_from = c("type"), values_from = "value") %>%
    mutate(source = case_when(full == "dataset" ~ source, T ~ "All")) %>%
    distinct() %>%
    filter(is.na(.data$AUC) == F)

  if (lines == T) {
    pl <- ggplot(auc1) +
            geom_line(aes(x = .data$source, y = .data$AUC,
                          group = interaction(source, .data$block),
                          color = as.factor(.data$block)),
                      position = position_dodge(width = 0.6)) +
            geom_point(aes(x = .data$source, y = .data$AUC, shape = .data$inout,
                           color = as.factor(.data$block),
                           group = as.factor(.data$block), size = .data$cells),
                       position = position_dodge(width = 0.6)) +
            theme_bw() +
            scale_shape_manual(values = c(16, 1)) +
            scale_color_manual(values = blockcols) +
            labs(x = "Dataset", y = "AUC", color = "Excluded fold",
                 shape = "Validation", size = "Number of cells \nwith data",
                 title = "Accuracy of predictions") +
            theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1)) +
            scale_x_discrete(labels = label_wrap(15))

  } else if (lines == F) {
    pl <- ggplot(auc1) +
            geom_point(aes(x = .data$source, y = .data$AUC, shape = .data$inout,
                           color = as.factor(.data$block),
                           group = as.factor(.data$block), size = .data$cells),
                       position = position_dodge(width = 0.6)) +
            theme_bw() +
            scale_shape_manual(values = c(16, 1)) +
            scale_color_manual(values = blockcols) +
            labs(x = "Dataset", y = "AUC", color = "Excluded fold",
                 shape = "Validation", size = "Number of cells \nwith data",
                 title = "Accuracy of predictions") +
            theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1)) +
            scale_x_discrete(labels = label_wrap(15))

  }

  return(pl)

}
