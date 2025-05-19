#' Map species data
#'
#' @param title (character) string to use as title of map
#' @param region (list) output from make_region()
#' @param plot (character) indicates what should be mapped; possible values are "samples", "lambda", "psi", "spat", "boundary", "XB"
#' @param plot.range (logical) whether to plot range outlines contained in region$range (T) or not (F); defaults to T
#' @param plot.region (logical) whether to plot region contained in region$region (T) or not (F); defaults to F
#' @param plot.cells (logical) whether to plot grid cells contained in region$sp.grid (T) or not (F); defaults to F
#' @param species.data (list) output from load_species_data(); only required if plot == "samples"
#' @param year.start (numeric) plot data from during or after this year; defaults to 1900; only required if plot == "samples"
#' @param year.end (numeric) plot data from during or before this year; defaults to current year; only required if plot == "samples"
#' @param details (logical) whether to add dataset details to map (T) or not (F); defaults to F; only required if plot == "samples"
#' @param plot.blocks (logical) whether to add blocks to map (T) or not (F); defaults to F; only required if plot == "samples"
#' @param blocks (sf) sf object containing blocks, e.g., output from cv_spatial(), out$blocks; only required if plot.blocks == T
#' @param out (list) summarized output from nimble model; output from summarize_samples(); only required if plot != "samples"
#' @param plot.current (logical) whether to plot the current output (T) or projections (F); defaults to T; only required if plot != "samples"
#' @param plot.uncertainty (logical) whether to plot uncertainty (T) or mean values (F); defaults to F; only required if plot != "samples"
#' @param plot.log (logical) whether to plot the log of the value (T) or not (F); defaults to F; only required if plot != "samples"
#' @param plot.exp (logical) whether to plot the exponent of the value (T) or not (F); defaults to F; only required if plot != "samples"
#' @param plot.change (logical) whether to plot the projected value (F) or the projected change from current (T); defaults to F; only required if plot.current == F
#' @param proj.names (character vector) names for each projection; only required if plot.current == F
#' @param threshold (numeric) value between 0 and 1 to use as occupancy probability threshold for drawing range boundary; defaults to 0.5; only required if plot == "boundary"
#' @param coarse.grid (logical) whether to use coarse grid (T) or not (F)
#' @param spatRegion (list) output from make_spatKey(); only required if coarse.grid == T
#'
#' @returns ggplot object containing map of desired data
#' @export
#'
#' @importFrom rnaturalearth ne_countries
#' @importFrom viridis scale_fill_viridis scale_color_viridis
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom stats median quantile
#'
#' @examples


map_species_data <- function(title,
                             region,
                             plot = "samples",

                             # Map features
                             plot.range = T,
                             plot.region = F,
                             plot.cells = F,

                             # For map of data samples
                             species.data,
                             year.start = 1900,
                             year.end = as.numeric(format(Sys.Date(), "%Y")),
                             details = F,
                             plot.blocks = F,
                             blocks,

                             # For map of model output
                             out,
                             plot.current = T,
                             plot.uncertainty = F,
                             plot.log = F,
                             plot.exp = F,
                             plot.change = F,
                             proj.names = "",
                             threshold = 0.5,

                             # Species grid
                             coarse.grid,
                             spatRegion) {

  if (plot.uncertainty %in% c(F, "lotail", "hitail", "unc.rel", "unc.range") == F) {
    stop("Options for uncertainty are F (do not plot uncertainty) or 'unc.rel.', 'unc.range', 'hitail', or 'lotail'")
  }

  if (plot %in% c("samples", "lambda", "psi", "spat", "boundary", "XB") == F) {
    stop("Options for plot are 'samples', 'lambda', 'psi', 'boundary', 'XB' or 'spat'")
  }

  blockcols <- c("none" = "black", "1" = "#e79f1e", "2" = "#009e73", "3" = "#cb79a8")



  # 1. load map features ----
  na <- rnaturalearth::ne_countries(continent = "North America",
                                    returnclass = "sf",
                                    scale = 10) %>%
          sf::st_transform(crs = sf::st_crs(region$range))

  wa <- rnaturalearth::ne_download(type = "lakes",
                                   category = "physical", load = T,
                                   returnclass = "sf",
                                   scale = 10) %>%
          sf::st_transform(crs = sf::st_crs(region$range))

  st <- rnaturalearth::ne_states(country = c("Canada", "Mexico", "United States of America"),
                                 returnclass = "sf") %>%
          sf::st_transform(crs = sf::st_crs(region$range))

  # 2. make base map ----
  base <- ggplot2::ggplot() +
          ggplot2::geom_sf(data = na, fill = "gray90") +
          ggplot2::geom_sf(data = st, fill = "gray90", color= "gray40") +
          ggplot2::geom_sf(data = wa, fill = "lightsteelblue") +
          ggplot2::theme(panel.background = ggplot2::element_rect(fill = "lightsteelblue"),
                         panel.grid = ggplot2::element_blank(),
                         axis.text = ggplot2::element_text(size = 12),
                         legend.text = ggplot2::element_text(size = 12),
                         legend.title = ggplot2::element_text(size = 14))



  # 3. add data to map ----

  # __a. region ----
  if (plot.region == T) {
    base <- base +
      ggplot2::geom_sf(data = region$region, alpha = 1, fill = "gray80", color = NA)
  }

  # __b. cells ----
  if (plot.cells == T) {
    base <- base +
      ggplot2::geom_sf(data = region$sp.grid, alpha = 1, fill = "lightblue", color = "gray20")
  }


  # __c. fill color ----


  # for anything other than plot == "samples"
  if (plot %in% c("lambda", "psi", "spat", "boundary", "XB")) {

    if (plot == "lambda") {
      fill <- "Relative abundance"
    } else if (plot == "psi") {
      fill <- "Occupancy probability"
    } else if (plot == "spat") {
      fill <- "Spatial effect"
    } else if (plot == "boundary") {
      fill <- paste0(threshold, " occupancy probability threshold")
    } else if (plot == "XB") {
      fill <- "XB"
    }


    plot1 <- ifelse(plot == "boundary", "psi", plot)

    # put all values together
    ind <- grep(plot1, names(out))
    intensity <- c()
    for (i in 1:length(ind)) {

      tmp <- out[[ind[i]]] %>%
        dplyr::mutate(scenario = gsub(plot1, "", names(out)[[ind[i]]]))
      intensity <- dplyr::bind_rows(intensity, tmp)

    }

    # get correct column to plot
    if (plot.uncertainty == F) {
      intensity$plot.val <- intensity$mean
      unc <- ""

    } else if (plot.uncertainty == "unc.rel") {
      intensity$plot.val <- intensity$unc.rel
      unc <- "\n(relative uncertainty)"

    } else if (plot.uncertainty == "unc.range") {
      intensity$plot.val <- intensity$unc.range
      unc <- "\n(range)"

    } else if (plot.uncertainty == "hitail") {
      intensity$plot.val <- intensity$hitail
      unc <- "\n(probability of higher than expected)"

    } else if (plot.uncertainty == "lotail") {
      intensity$plot.val <- intensity$lotail
      unc <- "\n(probability of lower than expected)"

    }



    # adjust values
    if (plot.log == F & plot != "spat") {
      q99 <- stats::quantile(intensity$plot.val, 0.99, na.rm = T)
      intensity$plot.val[which(intensity$plot.val > q99)] <- q99
    }

    if (plot.log == T) {
      intensity$plot.val <- log(intensity$plot.val)
    }

    if (plot.exp == T) {
      intensity$plot.val <- exp(intensity$plot.val)
    }

    if (plot == "boundary") {
      intensity$plot.val <- ifelse(intensity$plot.val >= threshold, "Present", "Absent")
    }

    # add geometry
    intensity <- dplyr::full_join(region$sp.grid, intensity, by = "conus.grid.id")




    # add fill color
    if (plot != "spat") {

      if (plot.current == T) { # Plot current

        if (plot == "boundary") {

          # set color scheme
          base <- base +
            ggplot2::scale_fill_manual(values = c("white", "darkblue"), na.value = NA) +
            ggplot2::scale_color_manual(values = c("white", "darkblue"), na.value = NA)


          # Plot current
          intensity0 <- dplyr::filter(intensity, scenario == 0)

          base <- base +
            ggplot2::geom_sf(data = intensity0, ggplot2::aes(fill = plot.val, color = plot.val)) +
            ggplot2::labs(fill = paste0(fill, unc), color = paste0(fill, unc), title = title)


        } else {
          # set color scheme
          base <- base +
            viridis::scale_fill_viridis(guide = ggplot2::guide_colorbar(),
                                        option = "magma",
                                        na.value = NA,
                                        direction = -1) +
            viridis::scale_color_viridis(guide = "none",
                                         option = "magma",
                                         na.value = NA,
                                         direction = -1)


          # Plot current
          intensity0 <- dplyr::filter(intensity, scenario == 0)

          base <- base +
            ggplot2::geom_sf(data = intensity0, ggplot2::aes(fill = plot.val, color = plot.val)) +
            ggplot2::guides(fill = ggplot2::guide_colorbar(theme = ggplot2::theme(legend.key.height = unit(0.75, "lines"),
                                                                                  legend.key.width = unit(20, "lines")),
                                                           title.hjust = 0.9, title.vjust = 1)) +
            ggplot2::labs(fill = paste0(fill, unc), color = paste0(fill, unc), title = title)

        }



      } else { # Plot projections


        if (length(proj.names) == 1) {
          if (proj.names == "") {
            proj.names <- paste0("Scenario ", 1:project)
          }
        }

        labs <- proj.names
        names(labs) <- paste0(1:project)


        if (plot.change == F) { # Plot estimated values


          if (plot == "boundary") {
            # set color scheme
            base <- base +
              ggplot2::scale_fill_manual(values = c("white", "darkblue"), na.value = NA) +
              ggplot2::scale_color_manual(values = c("white", "darkblue"), na.value = NA)

            intensity0 <- dplyr::filter(intensity, scenario != 0, is.na(plot.val) == F)

            base <- base +
              ggplot2::geom_sf(data = intensity0, ggplot2::aes(fill = plot.val, color = plot.val)) +
              ggplot2::labs(fill = paste0(fill, unc), color = paste0(fill, unc), title = title) +
              ggplot2::facet_wrap(~scenario, labeller = ggplot2::labeller(scenario = labs))


          } else {
            # set color scheme
            base <- base +
              viridis::scale_fill_viridis(guide = ggplot2::guide_colorbar(),
                                          option = "magma",
                                          na.value = NA,
                                          direction = -1) +
              viridis::scale_color_viridis(guide = "none",
                                           option = "magma",
                                           na.value = NA,
                                           direction = -1)

            intensity0 <- dplyr::filter(intensity, scenario != 0, is.na(plot.val) == F)

            base <- base +
              ggplot2::geom_sf(data = intensity0, ggplot2::aes(fill = plot.val, color = plot.val)) +
              ggplot2::guides(fill = ggplot2::guide_colorbar(theme = ggplot2::theme(legend.key.height = unit(0.75, "lines"),
                                                                                    legend.key.width = unit(20, "lines")),
                                                             title.hjust = 1, title.vjust = 1)) +
              ggplot2::labs(fill = paste0(fill, unc), color = paste0(fill, unc), title = title) +
              ggplot2::facet_wrap(~scenario, labeller = ggplot2::labeller(scenario = labs))
          }



        } else if (plot.change == "absolute") { # Plot estimated change

          if (plot == "boundary") {

            # set color scheme
            base <- base +
              ggplot2::scale_fill_manual(values = c("Unsuitable" = "white",
                                                     "Safe" = "darkblue",
                                                     "Loss" = "red",
                                                     "Gain" = "green"),
                                          na.value = NA) +
              ggplot2::scale_color_manual(values = c("Unsuitable" = "white",
                                                      "Safe" = "darkblue",
                                                      "Loss" = "red",
                                                      "Gain" = "green"),
                                           na.value = NA)

            scens <- unique(intensity$scenario)
            scens <- scens[-which(scens == "0")]
            intensity0 <- intensity %>%
                            dplyr::select(conus.grid.id, scenario, plot.val) %>%
                            tidyr::pivot_wider(names_from = scenario, values_from = plot.val) %>%
                            tidyr::pivot_longer(cols = tidyselect::all_of(scens)) %>%
                            dplyr::rename(current = "0",
                                          future = "value",
                                          scenario = "name") %>%
              dplyr::mutate(change = dplyr::case_when(current == "Absent" & future == "Absent" ~ "Unsuitable",
                                                      current == "Present" & future == "Absent" ~ "Loss",
                                                      current == "Absent" & future == "Present" ~ "Gain",
                                                      current == "Present" & future == "Present" ~ "Safe")) %>%
              dplyr::filter(is.na(change) == F)

            base <- base +
                      ggplot2::geom_sf(data = intensity0, ggplot2::aes(fill = change, color = change)) +
                      ggplot2::labs(fill = paste0(fill, unc), color = paste0(fill, unc), title = title) +
                      ggplot2::facet_wrap(~scenario, labeller = ggplot2::labeller(scenario = labs))

          } else {
            # set color scheme
            base <- base +
              ggplot2::scale_fill_gradient2(guide = ggplot2::guide_colorbar(), high = "darkblue", low = "darkred", mid = "white", na.value = NA) +
              ggplot2::scale_color_gradient2(guide = "none", high = "darkblue", low = "darkred", mid = "white", na.value = NA)

            scens <- unique(intensity$scenario)
            scens <- scens[-which(scens == "0")]
            intensity0 <- intensity %>%
                            dplyr::select(conus.grid.id, scenario, plot.val) %>%
                            tidyr::pivot_wider(names_from = scenario, values_from = plot.val) %>%
                            tidyr::pivot_longer(cols = tidyselect::all_of(scens)) %>%
                            dplyr::rename(current = "0",
                                          future = "value",
                                          scenario = "name") %>%
                            dplyr::mutate(change = future - current) %>%
                            dplyr::filter(is.na(change) == F)

            base <- base +
                      ggplot2::geom_sf(data = intensity0, ggplot2::aes(fill = change, color = change)) +
                      ggplot2::guides(fill = ggplot2::guide_colorbar(theme = ggplot2::theme(legend.key.height = unit(0.75, "lines"),
                                                                                            legend.key.width = unit(20, "lines")),
                                                                     title.hjust = 1, title.vjust = 1)) +
                      ggplot2::labs(fill = paste0("Absolute change in ", tolower(fill), unc), title = title) +
                      ggplot2::facet_wrap(~scenario, labeller = ggplot2::labeller(scenario = labs))
          }





        } else if (plot.change == "relative") { # Plot estimated relative change
          # set color scheme
          base <- base +
            ggplot2::scale_fill_gradient2(guide = ggplot2::guide_colorbar(), high = "darkblue", low = "darkred", mid = "white", na.value = NA) +
            ggplot2::scale_color_gradient2(guide = "none", high = "darkblue", low = "darkred", mid = "white", na.value = NA)

          scens <- unique(intensity$scenario)
          scens <- scens[-which(scens == "0")]
          intensity0 <- intensity %>%
                          dplyr::select(conus.grid.id, scenario, plot.val) %>%
                          tidyr::pivot_wider(names_from = scenario, values_from = plot.val) %>%
                          tidyr::pivot_longer(cols = tidyselect::all_of(scens)) %>%
                          dplyr::rename(current = "0",
                                        future = "value",
                                        scenario = "name") %>%
                          dplyr::mutate(change = (future - current)/current) %>%
                          dplyr::filter(is.na(change) == F)

          base <- base +
                    ggplot2::geom_sf(data = intensity0, ggplot2::aes(fill = change, color = change)) +
                    ggplot2::guides(fill = ggplot2::guide_colorbar(theme = ggplot2::theme(legend.key.height = unit(0.75, "lines"),
                                                                                          legend.key.width = unit(20, "lines")),
                                                                   title.hjust = 1, title.vjust = 1)) +
            ggplot2::labs(fill = paste0("Relative change in ", tolower(fill), unc), title = title) +
            ggplot2::facet_wrap(~scenario, labeller = ggplot2::labeller(scenario = labs))

        } else {
          stop("plot.change must be FALSE, 'relative', or 'absolute'")
        }

      } # end plot projections


    } else { # if plot == 'spat'
      base <- base +
        ggplot2::scale_fill_gradient2(guide = ggplot2::guide_colorbar(), high = "darkblue", low = "darkred", mid = "white", na.value = NA) +
        ggplot2::scale_color_gradient2(guide = "none", high = "darkblue", low = "darkred", mid = "white", na.value = NA)

      intensity <- dplyr::filter(intensity, is.na(plot.val) == F)

      base <- base +
                ggplot2::geom_sf(data = intensity, ggplot2::aes(fill = plot.val, color = plot.val)) +
                ggplot2::guides(fill = ggplot2::guide_colorbar(theme = ggplot2::theme(legend.key.height = unit(0.75, "lines"),
                                                                                      legend.key.width = unit(20, "lines")),
                                                               title.hjust = 1, title.vjust = 1)) +
                ggplot2::labs(fill = paste0(fill, unc), color = paste0(fill, unc), title = title)
    }

  }




  # put state lines and water on top of fill color
  base <- base +
            ggplot2::geom_sf(data = st, fill = NA) +
            ggplot2::geom_sf(data = wa, fill = "lightsteelblue")



  # __d. range ----
  if (plot.range == T) {
    base <- base +
             ggplot2::geom_sf(data = region$range, fill = NA, color = "gray35", linewidth = 0.4, aes(linetype = range.name))
  }

  # __e. blocks ----
  if (plot.blocks == T) {

    blocks <- sf::st_intersection(blocks, region$region)

    if (length(unique(blocks$folds)) == 1) {
      base <- base +
              ggplot2::geom_sf(data = blocks, size = 1, linewidth = .15,
                               ggplot2::aes(fill = as.factor(folds)), color = "gray30", alpha = 0.5) +
              ggplot2::scale_fill_manual(values = blockcols, name = "Out-of-sample test area",
                                         label = paste0("Block ", blocks$folds[1]))
    } else {
      base <- base +
                ggplot2::geom_sf(data = blocks, size = 1, linewidth = .15,
                                 ggplot2::aes(fill = as.factor(folds)), color = "gray30", alpha = 0.3) +
                ggplot2::scale_fill_manual(values = blockcols, name = "Block")
    }


  }

  # __f. samples ----
  if (plot == "samples") {

    if (details == T) {

      locs <- species.data$locs$cont %>%
                sf::st_drop_geometry() %>%
                dplyr::select(site.id, source) %>%
                dplyr::distinct()

      n.sites <- as.data.frame(table(locs$source))
      colnames(n.sites) <- c("source", "n.sites")

      locs <- species.data$locs$cont %>%
                sf::st_drop_geometry() %>%
                dplyr::select(site.id, survey.id, source)  %>%
                dplyr::distinct()

      n.surveys <- as.data.frame(table(locs$source, locs$site.id)) %>%
                    dplyr::filter(Freq != 0) %>%
                    dplyr::group_by(Var1) %>%
                    dplyr::summarize(n.visits = stats::median(Freq))
      colnames(n.surveys) <- c("source", "n.visits")

      tab <- dplyr::full_join(n.sites, n.surveys, by = "source") %>%
              dplyr::select(source, n.sites, n.visits)

      plotpoints <- species.data$locs$cont %>%
                    dplyr::full_join(tab, by = "source") %>%
                    dplyr::mutate(label = paste0(source, " (", data.type, ": ", n.sites, ", ", n.visits, ")"))
    } else {
      plotpoints <- species.data$locs$cont %>%
                    dplyr::filter(year >= year.start,
                                  year <= year.end,
                                  survey.conducted == 1)
    }


    plotpoints <- dplyr::filter(plotpoints, year <= year.end & year >= year.start)



    if (details == T) {
      base <- base +
                ggplot2::geom_sf(data = plotpoints, ggplot2::aes(color = label, shape = data.type), alpha = 1, size = 1.5) +
                ggplot2::scale_shape_manual(values = c("PO" = 4, "DND" = 0, "count" = 15, "CMR" = 17)) +
                ggplot2::labs(color = "Data source (data type: number of sites, \nmedian visits per site)",
                              shape = "Data type",
                              subtitle = paste0("All available data between ", year.start, " and ", year.end),
                              title = title,
                              linetype = "Range")
    } else {
      base <- base +
              ggplot2::geom_sf(data = plotpoints, ggplot2::aes(color = source, shape = data.type), alpha = 1, size = 1.5) +
              ggplot2::scale_shape_manual(values = c("PO" = 4, "DND" = 0, "count" = 15, "CMR" = 17)) +
              ggplot2::labs(color = "Data source",
                            shape = "Data type",
                            subtitle = paste0("All available data between ", year.start, " and ", year.end),
                            title = title,
                            linetype = "Range")
    }

  }






  # 4. add limits ----
  if (plot.blocks == F) {
    bb <- sf::st_bbox(region$region)
    xlim <- c(bb[1], bb[3])
    ylim <- c(bb[2], bb[4])
  } else {
    bb <- sf::st_bbox(sf::st_union(region$region, blocks))
    xlim <- c(bb[1], bb[3])
    ylim <- c(bb[2], bb[4])
  }

  base <- base +
    ggplot2::coord_sf(xlim = xlim, ylim = ylim)





  # 5. change theme ----
  if (plot == "samples") {
    base <- base +
            ggplot2::theme(legend.key = ggplot2::element_rect(fill = "gray90"),
                           axis.text.x = ggplot2::element_text(angle = 45,
                                                               hjust = 1,
                                                               vjust = 1))
  } else {
    base <- base +
            ggplot2::theme(legend.position = "bottom",
                           axis.text.x = ggplot2::element_text(angle = 45,
                                                               hjust = 1,
                                                               vjust = 1),
                           strip.text = ggplot2::element_text(size = 12)) +
      ggplot2::scale_linetype(guide = "none")
  }

  base



}
