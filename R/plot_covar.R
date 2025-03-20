#' Plot covariates
#'
#' @description
#'
#' @param covar data.frame holding covariate values and conus.grid.id
#' @param region region output from make_region()
#' @param cov.names column names from covar to plot
#' @param cov.labels labels for covariate names, defaults to use column names as labels
#' @param covs.int.factor name of columns in covar that has an interaction
#' @param out.path path to save figure
#' @param out.name file name to save figure
#'
#' @returns 
#' @export
#'
#' @examples
#' \dontrun{
#' range <- get_range('ANMI')
#' }


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
    st_transform(crs = 3857) %>%
    select(!name)
  
  bb <- region$sp.grid %>%
    st_buffer(buffer) %>%
    st_bbox()
  
  sp.grid <- region$sp.grid %>%
    full_join(covar, by = c("conus.grid.id")) %>%
    select(all_of(c("conus.grid.id", cov.names)))
  
  if (length(covs.int.factor) == 1 & is.na(covs.int.factor) == F) {
    # convert factors to numbers
    tmp <- sp.grid %>%
      group_by_at(covs.int.factor) %>%
      mutate(factor = cur_group_id())
    sp.grid[[covs.int.factor]] <- tmp$factor
  }
  
  sp.grid <- sp.grid %>%
    pivot_longer(!c(conus.grid.id, geometry)) %>%
    
    # remove covariates that are squares
    mutate(square = substr(name, nchar(name), nchar(name))) %>%
    filter(square != 2) %>%
    mutate(value = case_when(value < -5 ~ -5,
                             value > 5 ~ 5,
                             T ~ value)) %>%
    
  # add labels
  left_join(cov.labs, by = "name")
  
  
  pl <- ggplot(filter(sp.grid, is.na(value) == F)) +
    geom_sf(aes(fill = value, color = value)) +
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
    guides(fill = guide_colorbar(theme = theme(
      legend.key.height = unit(20, "lines"),
      legend.key.width = unit(1.5, "lines")), title.hjust = 0, title.vjust = 1)) +
    labs(fill = "Value", color = "Value",
         title = "Scaled covariate values") +
    coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax))
  
  ggsave(pl, file = paste0(out.path, out.name, ".jpg"), height = 15, width = 15)
  
}







# plot_covar <- function(covar,
#                        covs.z,
#                        covs.int.factor,
#                        region,
#                        path,
#                        buffer = 3,
#                        type = "process") {
#
#   cov.labs <- read.csv("data/covariate-labels.csv")
#
#   st <- rnaturalearth::ne_states(country = c("Canada", "Mexico", "United States of America"),
#                                  returnclass = "sf") %>%
#     st_transform(crs = 3857) %>%
#     select(!name)
#
#   bb <- region$sp.grid %>%
#     st_buffer(buffer) %>%
#     st_bbox()
#
#   sp.grid <- region$sp.grid %>%
#     full_join(covar, by = c("conus.grid.id", "sp.grid.id")) %>%
#     select(all_of(c("conus.grid.id", covs.z)))
#
#   #sp.grid <- as.data.frame(sp.grid[,covs.z])
#   if (length(covs.int.factor) == 1 & is.na(covs.int.factor) == F) {
#     # convert factors to numbers
#     tmp <- sp.grid %>%
#       group_by_at(covs.int.factor) %>%
#       mutate(factor = cur_group_id())
#     sp.grid[[covs.int.factor]] <- tmp$factor
#   }
#
#   sp.grid <- sp.grid %>%
#     pivot_longer(!c(conus.grid.id, geometry)) %>%
#
#     # remove covariates that are squares
#     mutate(square = substr(name, nchar(name), nchar(name))) %>%
#     filter(square != 2) %>%
#     mutate(value = case_when(value < -5 ~ -5,
#                              value > 5 ~ 5,
#                              T ~ value)) %>%
#
#     # add labels
#     left_join(cov.labs, by = c("name" = "covariate"))
#
#
#   pl <- ggplot(filter(sp.grid, is.na(value) == F)) +
#     geom_sf(aes(fill = value, color = value)) +
#     geom_sf(data = st, fill = NA, color= "gray40") +
#     facet_wrap(~Label) +
#     scale_fill_gradient2(high = "darkblue", low = "darkred", mid = "white") +
#     scale_color_gradient2(high = "darkblue", low = "darkred", mid = "white", guide = "none") +
#     theme_bw() +
#     theme(panel.grid = element_blank(),
#           axis.text = element_text(size = 16),
#           axis.text.x = element_text(angle = 90,
#                                      hjust = 0.5,
#                                      vjust = 1),
#           strip.text = element_text(size = 16),
#           legend.title = element_text(size = 20),
#           legend.text = element_text(size = 17),
#           text = element_text(size = 18)) +
#     guides(fill = guide_colorbar(theme = theme(
#       legend.key.height = unit(20, "lines"),
#       legend.key.width = unit(1.5, "lines")), title.hjust = 0, title.vjust = 1)) +
#     labs(fill = "Value", color = "Value",
#          title = "Scaled covariate values") +
#     coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax))
#
#   ggsave(pl, file = paste0(path, "/1_covariates-", type, "-map.jpg"), height = 15, width = 15)
#
# }
