# plot_auc <- function(auc,
#                      lines) {
#
#   auc1 <- pivot_longer(auc, cols = !c("sp.code", "block", "source")) %>%
#     mutate(inout = case_when(name %in% c("AUCin", "AUCin.full", "in.cell", "in.full.cell", "in.full.n", "in.n") ~ "In sample",
#                              T ~ "Out of sample"),
#            type = case_when(name %in% c("AUCin", "AUCin.full", "AUCout", "AUCout.full") ~ "AUC",
#                             name %in% c("in.cell", "in.full.cell", "out.cell", "out.full.cell") ~ "cells",
#                             name %in% c("in.full.n", "in.n", "out.full.n", "out.n") ~ "samples"),
#            full = case_when(name %in% c("AUCin.full", "AUCout.full", "in.full.cell", "in.full.n", "out.full.cell", "out.full.n") ~ "full",
#                             T ~ "dataset")) %>%
#     select(!name) %>%
#     pivot_wider(names_from = c(type), values_from = value) %>%
#     mutate(source = case_when(full == "dataset" ~ source,
#                               T ~ "All")) %>%
#     distinct()
#
#   if (lines == T) {
#     pl <- ggplot(auc1) +
#       geom_line(aes(x = source, y = AUC, group = interaction(source, block), color = as.factor(block)),
#                 position = position_dodge(width = 0.6)) +
#       geom_point(aes(x = source, y = AUC, shape = inout, color = as.factor(block), group = as.factor(block), size = cells),
#                  position = position_dodge(width = 0.6)) +
#       theme_bw() +
#       scale_shape_manual(values = c(16, 1)) +
#       scale_color_manual(values = blockcols) +
#       labs(x = "Dataset", y = "AUC", color = "Excluded block", shape = "Validation", size = "Number of cells \nwith data",
#            title = "Accuracy of predictions") +
#       theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1)) +
#       scale_x_discrete(labels = scales::label_wrap(15))
#
#     ggsave(pl, file = paste0(out.dir, "7_CV-d_AUC-withlines.jpg"), height = 7, width = 10)
#
#   } else if (lines == F) {
#     pl <- ggplot(auc1) +
#       geom_point(aes(x = source, y = AUC, shape = inout, color = as.factor(block), group = as.factor(block), size = cells),
#                  position = position_dodge(width = 0.6)) +
#       theme_bw() +
#       scale_shape_manual(values = c(16, 1)) +
#       scale_color_manual(values = blockcols) +
#       labs(x = "Dataset", y = "AUC", color = "Excluded block", shape = "Validation", size = "Number of cells \nwith data",
#            title = "Accuracy of predictions") +
#       theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1)) +
#       scale_x_discrete(labels = scales::label_wrap(15))
#
#     ggsave(pl, file = paste0(out.dir, "7_CV-d_AUC-withoutlines.jpg"), height = 7, width = 10)
#
#   }
#
#
#
#
#
# }
