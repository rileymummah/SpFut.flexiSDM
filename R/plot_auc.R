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
#'
#' @examples



plot_auc <- function(auc,
                     lines) {

  blockcols <- c("none" = "black", "1" = "#e79f1e", "2" = "#009e73", "3" = "#cb79a8")


  auc1 <- tidyr::pivot_longer(auc, cols = !c("sp.code", "block", "source")) %>%
    dplyr::mutate(inout = dplyr::case_when(name %in% c("AUCin", "AUCin.full", "in.cell", "in.full.cell", "in.full.n", "in.n") ~ "In sample",
                                            T ~ "Out of sample"),
                   type = dplyr::case_when(name %in% c("AUCin", "AUCin.full", "AUCout", "AUCout.full") ~ "AUC",
                                           name %in% c("in.cell", "in.full.cell", "out.cell", "out.full.cell") ~ "cells",
                                           name %in% c("in.full.n", "in.n", "out.full.n", "out.n") ~ "samples"),
                   full = dplyr::case_when(name %in% c("AUCin.full", "AUCout.full", "in.full.cell", "in.full.n", "out.full.cell", "out.full.n") ~ "full",
                                           T ~ "dataset")) %>%
    dplyr::select(!name) %>%
    tidyr::pivot_wider(names_from = c(type), values_from = value) %>%
    dplyr::mutate(source = dplyr::case_when(full == "dataset" ~ source,
                                            T ~ "All")) %>%
    dplyr::distinct()

  if (lines == T) {
    pl <- ggplot2::ggplot(auc1) +
            ggplot2::geom_line(ggplot2::aes(x = source, y = AUC,
                                            group = interaction(source, block), color = as.factor(block)),
                               position = ggplot2::position_dodge(width = 0.6)) +
            ggplot2::geom_point(ggplot2::aes(x = source, y = AUC, shape = inout,
                                             color = as.factor(block), group = as.factor(block), size = cells),
                                position = ggplot2::position_dodge(width = 0.6)) +
            ggplot2::theme_bw() +
            ggplot2::scale_shape_manual(values = c(16, 1)) +
            ggplot2::scale_color_manual(values = blockcols) +
            ggplot2::labs(x = "Dataset", y = "AUC", color = "Excluded block",
                          shape = "Validation", size = "Number of cells \nwith data",
                          title = "Accuracy of predictions") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 1)) +
            ggplot2::scale_x_discrete(labels = scales::label_wrap(15))


  } else if (lines == F) {
    pl <- ggplot2::ggplot(auc1) +
            ggplot2::geom_point(aes(x = source, y = AUC, shape = inout,
                                    color = as.factor(block), group = as.factor(block), size = cells),
                                position = ggplot2::position_dodge(width = 0.6)) +
            ggplot2::theme_bw() +
            ggplot2::scale_shape_manual(values = c(16, 1)) +
            ggplot2::scale_color_manual(values = blockcols) +
            ggplot2::labs(x = "Dataset", y = "AUC", color = "Excluded block",
                          shape = "Validation", size = "Number of cells \nwith data",
                          title = "Accuracy of predictions") +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 1)) +
            ggplot2::scale_x_discrete(labels = scales::label_wrap(15))


  }


  return(pl)


}
