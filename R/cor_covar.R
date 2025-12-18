#' Plot correlations between covariates
#'
#' @description Calculates and plots correlations between covariates.
#'
#' @param covar (data.frame) data.frame holding covariate values and conus.grid.id
#' @param cov.names (character vector) column names from covar to plot
#' @param cov.labels (character vector) labels for covariate names, defaults to use column names as labels
#' @param cov.levels (character vector) column names in order they should appear on plot
#' @param covs.int.factor (character vector) name of columns in covar that has an interaction
#' @param out.path (character) path to save figure
#' @param out.name (character) file name to save figure
#' @param color.threshold (numeric) threshold above which to add color to plot; default is 0.25
#'
#' @returns Saves a plot showing correlations between covariates
#' @export
#'
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#' @importFrom stats cor
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by_at mutate cur_group_id filter left_join rename select
#' @importFrom ggplot2 ggplot geom_tile aes geom_hline geom_vline geom_text ggsave scale_fill_gradient2 scale_color_gradient2 labs theme_bw theme element_text
#'
#' @examples
#' \dontrun{
#' cor_covar(covar,
#'           cov.names = c("cwd", "permwater"),
#'           cov.labels = c("CWD", "Permanent water"),
#'           out.path = "outputs/",
#'           out.name = "covs-cor")
#'}



cor_covar <- function(covar,
                      cov.names,
                      cov.labels = cov.names,
                      cov.levels = cov.labels,
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
            mutate(cov1 = row.names()) %>%
            pivot_longer(!.data$cov1) %>%
            filter(.data$value != 1) %>%

            # add labels
            left_join(cov.labs, by = "name") %>%
            rename(cov2 = "label") %>%

            left_join(cov.labs, by = c("cov1" = "name")) %>%
            select(!c(.data$cov1, .data$name)) %>%
            rename(cov1 = "label")


  cors$cov1 <- factor(cors$cov1, levels = cov.levels)
  cors$cov2 <- factor(cors$cov2, levels = cov.levels)


  corspl <- ggplot() +
              geom_tile(data = filter(cors, .data$value > color.threshold | .data$value < color.threshold*-1),
                        aes(x = .data$cov1, y = .data$cov2, fill = .data$value)) +
              geom_hline(yintercept = c(1:length(unique(cors$cov1))), color = "gray90", alpha = 0.2) +
              geom_vline(xintercept = c(1:length(unique(cors$cov1))), color = "gray90", alpha = 0.2) +
              geom_text(data = cors,
                        aes(x = .data$cov1, y = .data$cov2, label = round(.data$value, 2))) +
              scale_fill_gradient2(limits = c(-1, 1)) +
              scale_color_gradient2(limits = c(-1, 1)) +
              labs(x = "", y = "", fill = "Correlation \ncoefficient", color = "Correlation \ncoefficient",
                   title = paste0("Correlations between covariates (colored if >", color.threshold, ")")) +
              theme_bw() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
                    axis.text = element_text(size = 8),
                    legend.title = element_text(size = 9),
                    legend.text = element_text(size = 8))

  ggsave(corspl,
         filename = paste0(out.path, out.name, ".jpg"),
         height = 5, width = 9)
}

