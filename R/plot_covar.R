#' Plot covariates
#'
#' @description Makes maps of scaled covariate values.
#'
#' @param covar (data.frame) dataframe holding covariate values and conus.grid.id
#' @param region (list) region output from make_region()
#' @param cov.names (character vector) column names from covar to plot
#' @param cov.labels (character vector) labels for covariate names, defaults to use column names as labels
#' @param covs.int.factor (character) name of columns in covar that has an interaction
#' @param out.path (character) path to save figure
#' @param out.name (character) file name to save figure
#'
#' @returns Saves a map of scaled covariate values to the desired path.
#' @export
#'
#' @importFrom rnaturalearth ne_states
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_longer
#' @importFrom grid unit
#'
#' @examples
#' \dontrun{
#' 
#'
#' plot_covar(covar,
#'            region,
#'            cov.names = c("cwd", "permwater"),
#'            cov.labels = c("CWD", "Permanent water"),
#'            out.path = "outputs/",
#'            out.name = "covs-map")
#'
#'}


plot_covar <- function(covar,
                       region,
                       cov.names,
                       cov.labels = cov.names,
                       covs.int.factor = NA,
                       out.path = "",
                       out.name = "covariate-map") {

  cov.labs <- data.frame(name = cov.names,
                         label = cov.labels)

  st <- rnaturalearth::ne_states(country = c("Canada", "Mexico", "United States of America"),
                                 returnclass = "sf") %>%
          sf::st_transform(crs = 3857) %>%
          dplyr::select(!name)

  bb <- region$sp.grid %>%
          sf::st_buffer(buffer) %>%
          sf::st_bbox()

  sp.grid <- region$sp.grid %>%
              dplyr::full_join(covar, by = c("conus.grid.id")) %>%
              dplyr::select(tidyselect::all_of(c("conus.grid.id", cov.names)))

  if (length(covs.int.factor) == 1 & is.na(covs.int.factor) == F) {
    # convert factors to numbers
    tmp <- sp.grid %>%
            dplyr::group_by_at(covs.int.factor) %>%
            dplyr::mutate(factor = dplyr::cur_group_id())
    sp.grid[[covs.int.factor]] <- tmp$factor
  }

  sp.grid <- sp.grid %>% tidyr::pivot_longer(!c(conus.grid.id, geometry)) %>%

              # remove covariates that are squares
              dplyr::mutate(square = substr(name, nchar(name), nchar(name))) %>%
              dplyr::filter(square != 2) %>%
              dplyr::mutate(value = dplyr::case_when(value < -5 ~ -5,
                                                     value > 5 ~ 5,
                                                     T ~ value)) %>%

              # add labels
              dplyr::left_join(cov.labs, by = "name")


  pl <- ggplot2::ggplot(dplyr::filter(sp.grid, is.na(value) == F)) +
          ggplot2::geom_sf(ggplot2::aes(fill = value, color = value)) +
          ggplot2::geom_sf(data = st, fill = NA, color= "gray40") +
          ggplot2::facet_wrap(~label) +
          ggplot2::scale_fill_gradient2(high = "darkblue", low = "darkred", mid = "white") +
          ggplot2::scale_color_gradient2(high = "darkblue", low = "darkred", mid = "white", guide = "none") +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.grid = ggplot2::element_blank(),
                         axis.text = ggplot2::element_text(size = 16),
                         axis.text.x = ggplot2::element_text(angle = 90,
                                                             hjust = 0.5,
                                                             vjust = 1),
                         strip.text = ggplot2::element_text(size = 16),
                         legend.title = ggplot2::element_text(size = 20),
                         legend.text = ggplot2::element_text(size = 17),
                         text = ggplot2::element_text(size = 18)) +
          ggplot2::guides(fill = ggplot2::guide_colorbar(ggplot2::theme = theme(
                                                                          legend.key.height = grid::unit(20, "lines"),
                                                                          legend.key.width = grid::unit(1.5, "lines")),
                                                         title.hjust = 0, title.vjust = 1)) +
          ggplot2::labs(fill = "Value", color = "Value",
                        title = "Scaled covariate values") +
          ggplot2::coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax))


  ggplot2::ggsave(pl, file = paste0(out.path, out.name, ".jpg"), height = 15, width = 15)

}



