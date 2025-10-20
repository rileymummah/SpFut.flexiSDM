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
#' @importFrom stats cor
#'
#' @examples
#' \dontrun{
#' cor_covar(covar,
#'           cov.names = c("cwd", "permwater"),
#'           cov.labels = c("CWD", "Permanent water"),
#'           out.path = "outputs/",
#'           out.name = "covs-cor")
#'
#'}



cor_covar <- function(covar,
                      cov.names,
                      cov.labels = cov.names,
                      cov.levels = cov.names,
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
            dplyr::group_by_at(covs.int.factor) %>%
            dplyr::mutate(factor = dplyr::cur_group_id())
    cors[[covs.int.factor]] <- tmp$factor
  }

  cors <- as.data.frame(stats::cor(cors)) %>%
            dplyr::mutate(cov1 = row.names(.)) %>%
            tidyr::pivot_longer(!cov1) %>%
            dplyr::filter(value != 1) %>%

            # add labels
            dplyr::left_join(cov.labs, by = "name") %>%
            #select(!name) %>%
            dplyr::rename(cov2 = "label") %>%

            dplyr::left_join(cov.labs, by = c("cov1" = "name")) %>%
            dplyr::select(!c(cov1, name)) %>%
            dplyr::rename(cov1 = "label")

  
  cors$cov1 <- factor(cors$cov1, levels = cov.levels)
  cors$cov2 <- factor(cors$cov2, levels = cov.levels)
  

  corspl <- ggplot2::ggplot() +
              ggplot2::geom_tile(data = dplyr::filter(cors, value > color.threshold | value < color.threshold*-1),
                                 ggplot2::aes(x = cov1, y = cov2, fill = value)) +
              ggplot2::geom_hline(yintercept = c(1:length(unique(cors$cov1))), color = "gray90", alpha = 0.2) +
              ggplot2::geom_vline(xintercept = c(1:length(unique(cors$cov1))), color = "gray90", alpha = 0.2) +
              ggplot2::geom_text(data = cors,
                                 ggplot2::aes(x = cov1, y = cov2, label = round(value, 2))) +
              ggplot2::scale_fill_gradient2(limits = c(-1, 1)) +
              ggplot2::scale_color_gradient2(limits = c(-1, 1)) +
              ggplot2::labs(x = "", y = "", fill = "Correlation \ncoefficient", color = "Correlation \ncoefficient",
                            title = paste0("Correlations between covariates (colored if >", color.threshold, ")")) +
              ggplot2::theme_bw() +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1),
                             axis.text = ggplot2::element_text(size = 8),
                             legend.title = ggplot2::element_text(size = 9),
                             legend.text = ggplot2::element_text(size = 8))

  ggplot2::ggsave(corspl,
                  file = paste0(out.path, out.name, ".jpg"),
                  height = 5, width = 9)
}

