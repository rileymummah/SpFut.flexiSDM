#' Plot covariates
#'
#' @description Makes maps of scaled covariate values.
#'
#' @param covar (data.frame) dataframe holding covariate values and conus.grid.id
#' @param region (list) region output from make_region()
#' @param cov.names (character vector) column names from covar to plot
#' @param cov.labels (character vector) labels for covariate names, defaults to use column names as labels
#'
#' @returns Saves a map of scaled covariate values to the desired path.
#' @export
#'
#' @importFrom rnaturalearth ne_states
#' @importFrom sf st_transform st_buffer st_bbox
#' @importFrom tidyselect all_of
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr "%>%"
#' @importFrom grid unit
#' @importFrom dplyr select full_join group_by_at filter mutate left_join
#' @importFrom ggplot2 ggplot aes geom_sf facet_wrap scale_fill_gradient2 scale_color_gradient2 theme_bw element_blank element_text guides guide_colorbar theme labs coord_sf ggsave
#'
#' @examples
#' \dontrun{
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
                       scaled){ #,
                       # out.path = "",
                       # out.name = "covariate-map") {

  cov.labs <- data.frame(name = cov.names,
                         label = cov.labels)

  st <- ne_states(country = c("Canada", "Mexico", "United States of America"),
                  returnclass = "sf") %>%
          st_transform(crs = 3857) %>%
          select(!"name")

  bb <- region$region %>%
          #st_buffer(.data$buffer) %>%
          st_bbox()

  sp.grid <- region$sp.grid %>%
              full_join(covar, by = c("conus.grid.id")) %>%
              select(all_of(c("conus.grid.id", cov.names)))

  

  sp.grid <- sp.grid %>% pivot_longer(!c("conus.grid.id", "geometry")) %>%

              # remove covariates that are squares
              mutate(square = substr(.data$name, nchar(.data$name), nchar(.data$name))) %>%
              filter(.data$square != 2) %>%
              mutate(value = case_when(value < -5 ~ -5,
                                       value > 5 ~ 5,
                                       T ~ value)) %>%

              # add labels
              left_join(cov.labs, by = "name") %>%
    select(!"square")


  if (scaled == T) title <- "Scaled covariate values"
  if (scaled == F) title <- "Covariate values"
  pl <- ggplot(filter(sp.grid, is.na(.data$value) == F)) +
          geom_sf(aes(fill = .data$value, color = .data$value)) +
          geom_sf(data = st, fill = NA, color= "gray40") +
          facet_wrap(~label) +
          scale_fill_gradient2(high = "darkblue", low = "darkred", mid = "white") +
          scale_color_gradient2(high = "darkblue", low = "darkred", mid = "white", guide = "none") +
          theme_bw() +
          theme(panel.grid = element_blank(),
                axis.text = element_text(size = 16),
                axis.text.x = element_text(angle = 90,
                                           hjust = 0.5,
                                           vjust = 1),
                strip.text = element_text(size = 16),
                legend.title = element_text(size = 20),
                legend.text = element_text(size = 17),
                text = element_text(size = 18)) +
          guides(fill = guide_colorbar(theme = theme(legend.key.height = unit(20, "lines"),
                                                     legend.key.width = unit(1.5, "lines")),
                                       title.hjust = 0, title.vjust = 1)) +
          labs(fill = "Value", color = "Value",
               title = title) +
          coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax))


  # ggsave(pl, filename = paste0(out.path, out.name, ".jpg"), height = 15, width = 15)

  return(list(dat = sp.grid,
              plot = pl))
}

