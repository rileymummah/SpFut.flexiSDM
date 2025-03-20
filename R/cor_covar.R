#' Plot correlations between covariates
#'
#' @description
#'
#' @param covar data.frame holding covariate values and conus.grid.id
#' @param cov.names column names from covar to plot
#' @param cov.labels labels for covariate names, defaults to use column names as labels
#' @param covs.int.factor name of columns in covar that has an interaction
#' @param out.path path to save figure
#' @param out.name file name to save figure
#' @param color.threshold threshold above which to add color to plot
#'
#' @returns 
#' @export
#'
#' @examples
#' \dontrun{
#' range <- get_range('ANMI')
#' }


cor_covar <- function(covar,
                      cov.names,
                      cov.labels = cov.names,
                      covs.int.factor = NA,
                      out.path = "",
                      out.name = "covariate-cor",
                      color.threshold = 0.25) {
  
  
  cov.labs <- data.frame(name = cov.names,
                         label = cov.labels)
  
  
    cors <- as.data.frame(covar[,cov.names])
    if (length(covs.int.factor) == 1 & is.na(covs.int.factor) == F) {
      # convert factors to numbers
      tmp <- cors %>%
        group_by_at(covs.int.factor) %>%
        mutate(factor = cur_group_id())
      cors[[covs.int.factor]] <- tmp$factor
    }

    cors <- as.data.frame(cor(cors)) %>%
      mutate(cov1 = row.names(.)) %>%
      pivot_longer(!cov1) %>%
      filter(value != 1) %>%

      # add labels
      left_join(cov.labs, by = "name") %>%
      #select(!name) %>%
      rename(cov2 = "label") %>%

      left_join(cov.labs, by = c("cov1" = "name")) %>%
      select(!c(cov1, name)) %>%
      rename(cov1 = "label") 


    corspl <- ggplot() +
      geom_tile(data = filter(cors, value > color.threshold |
                                value < color.threshold*-1),
                aes(x = cov1, y = cov2, fill = value)) +
      geom_hline(yintercept = c(1:length(unique(cors$cov1))), color = "gray90", alpha = 0.2) +
      geom_vline(xintercept = c(1:length(unique(cors$cov1))), color = "gray90", alpha = 0.2) +
      geom_text(data = cors,
                aes(x = cov1, y = cov2, label = round(value, 2))) +
      scale_fill_gradient2(limits = c(-1, 1)) +
      scale_color_gradient2(limits = c(-1, 1)) +
      labs(x = "", y = "", fill = "Correlation \ncoefficient", color = "Correlation \ncoefficient",
           title = paste0("Correlations between covariates (colored if >", color.threshold, ")")) +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
            axis.text = element_text(size = 8),
            legend.title = element_text(size = 9),
            legend.text = element_text(size = 8))

    ggsave(corspl, file = paste0(out.path, out.name, ".jpg"),
           height = 5, width = 9)
}




# cor_covar <- function(covar,
#                       covs.z,
#                       covs.int.factor = "",
#                       path,
#                       type = "process",
#                       threshold = 0.25) {
#
#   cov.labs <- read.csv("data/covariate-labels.csv")
#
#   cors <- as.data.frame(covar[,covs.z])
#   if (length(covs.int.factor) == 1 & is.na(covs.int.factor) == F) {
#     # convert factors to numbers
#     tmp <- cors %>%
#       group_by_at(covs.int.factor) %>%
#       mutate(factor = cur_group_id())
#     cors[[covs.int.factor]] <- tmp$factor
#   }
#
#   cors <- as.data.frame(cor(cors)) %>%
#     mutate(cov1 = row.names(.)) %>%
#     pivot_longer(!cov1) %>%
#     filter(value != 1) %>%
#
#     # add labels
#     left_join(cov.labs, by = c("name" = "covariate")) %>%
#     select(!name) %>%
#     rename(cov2 = "Label") %>%
#
#     left_join(cov.labs, by = c("cov1" = "covariate")) %>%
#     select(!cov1) %>%
#     rename(cov1 = "Label")
#
#
#   corspl <- ggplot() +
#     geom_tile(data = filter(cors, value > threshold |
#                               value < threshold*-1),
#               aes(x = cov1, y = cov2, fill = value)) +
#     geom_hline(yintercept = c(1:length(unique(cors$cov1))), color = "gray90", alpha = 0.2) +
#     geom_vline(xintercept = c(1:length(unique(cors$cov1))), color = "gray90", alpha = 0.2) +
#     geom_text(data = cors,
#               aes(x = cov1, y = cov2, label = round(value, 2))) +
#     scale_fill_gradient2(limits = c(-1, 1)) +
#     scale_color_gradient2(limits = c(-1, 1)) +
#     labs(x = "", y = "", fill = "Correlation \ncoefficient", color = "Correlation \ncoefficient",
#          title = paste0("Correlations between covariates (colored if >", threshold, ")")) +
#     theme_bw() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
#           axis.text = element_text(size = 8),
#           legend.title = element_text(size = 9),
#           legend.text = element_text(size = 8))
#
#   ggsave(corspl, file = paste0(path, "1_covariates-", type, "-correlations.jpg"),
#          height = 5, width = 9)
# }
