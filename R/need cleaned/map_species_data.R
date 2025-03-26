map_species_data <- function(title,
                             region,
                             
                             # Map features
                             plot.range = T,
                             plot.region = F,
                             plot.cells = F,
                             
                             # For map of data samples
                             species.data,
                             year.start,
                             year.end = as.numeric(format(Sys.Date(), "%Y")),
                             plot = "samples",
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
                             proj = 0,
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
    st_transform(crs = st_crs(region$range))
  
  wa <- rnaturalearth::ne_download(type = "lakes",
                                   category = "physical", load = T,
                                   returnclass = "sf",
                                   scale = 10) %>%
    st_transform(crs = st_crs(region$range))
  
  st <- rnaturalearth::ne_states(country = c("Canada", "Mexico", "United States of America"),
                                 returnclass = "sf") %>%
    st_transform(crs = st_crs(region$range))
  
  
  # 2. make base map ----
  base <- ggplot() +
    geom_sf(data = na, fill = "gray90") +
    geom_sf(data = st, fill = "gray90", color= "gray40") +
    geom_sf(data = wa, fill = "lightsteelblue") +
    theme(panel.background = element_rect(fill = "lightsteelblue"),
          panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
  
  
  
  # 3. add data to map ----
  
  # __a. region ----
  if (plot.region == T) {
    base <- base +
      geom_sf(data = region$region, alpha = 1, fill = "gray80", color = NA)
  }
  
  # __b. cells ----
  if (plot.cells == T) {
    base <- base +
      geom_sf(data = region$sp.grid, alpha = 1, fill = "lightblue", color = "gray20")
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
        mutate(scenario = gsub(plot1, "", names(out)[[ind[i]]]))
      intensity <- bind_rows(intensity, tmp)
      
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
      q99 <- quantile(intensity$plot.val, 0.99, na.rm = T)
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
    intensity <- full_join(region$sp.grid, intensity, by = "conus.grid.id")
    
    
    
    
    # add fill color
    if (plot != "spat") {
      
      if (plot.current == T) { # Plot current
        
        if (plot == "boundary") {
          
          # set color scheme
          base <- base +
            scale_fill_manual(values = c("white", "darkblue"), na.value = NA) +
            scale_color_manual(values = c("white", "darkblue"), na.value = NA)
          
          
          # Plot current
          intensity0 <- filter(intensity, scenario == 0)
          
          base <- base +
            geom_sf(data = intensity0, aes(fill = plot.val, color = plot.val)) +
            labs(fill = paste0(fill, unc), color = paste0(fill, unc), title = title)
          
          
        } else {
          # set color scheme
          base <- base +
            viridis::scale_fill_viridis(guide = guide_colorbar(),
                                        option = "magma",
                                        na.value = NA,
                                        direction = -1) +
            viridis::scale_color_viridis(guide = "none",
                                         option = "magma",
                                         na.value = NA,
                                         direction = -1)
          
          
          # Plot current
          intensity0 <- filter(intensity, scenario == 0)
          
          base <- base +
            geom_sf(data = intensity0, aes(fill = plot.val, color = plot.val)) +
            guides(fill = guide_colorbar(theme = theme(
              legend.key.height = unit(0.75, "lines"),
              legend.key.width = unit(20, "lines")), title.hjust = 0.9, title.vjust = 1)) +
            labs(fill = paste0(fill, unc), color = paste0(fill, unc), title = title)
          
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
              scale_fill_manual(values = c("white", "darkblue"), na.value = NA) +
              scale_color_manual(values = c("white", "darkblue"), na.value = NA)
            
            intensity0 <- filter(intensity, scenario != 0, is.na(plot.val) == F)
            
            base <- base +
              geom_sf(data = intensity0, aes(fill = plot.val, color = plot.val)) +
              labs(fill = paste0(fill, unc), color = paste0(fill, unc), title = title) +
              facet_wrap(~scenario, labeller = labeller(scenario = labs))
            
            
          } else {
            # set color scheme
            base <- base +
              viridis::scale_fill_viridis(guide = guide_colorbar(),
                                          option = "magma",
                                          na.value = NA,
                                          direction = -1) +
              viridis::scale_color_viridis(guide = "none",
                                           option = "magma",
                                           na.value = NA,
                                           direction = -1)
            
            intensity0 <- filter(intensity, scenario != 0, is.na(plot.val) == F)
            
            base <- base +
              geom_sf(data = intensity0, aes(fill = plot.val, color = plot.val)) +
              guides(fill = guide_colorbar(theme = theme(
                legend.key.height = unit(0.75, "lines"),
                legend.key.width = unit(20, "lines")), title.hjust = 1, title.vjust = 1)) +
              labs(fill = paste0(fill, unc), color = paste0(fill, unc), title = title) +
              facet_wrap(~scenario, labeller = labeller(scenario = labs))
          }
          
          
          
        } else if (plot.change == "absolute") { # Plot estimated change
          
          if (plot == "boundary") {
            
            # set color scheme
            base <- base +
              scale_fill_manual(values = c("Unsuitable" = "white",
                                           "Safe" = "darkblue",
                                           "Loss" = "red",
                                           "Gain" = "green"),
                                na.value = NA) +
              scale_color_manual(values = c("Unsuitable" = "white",
                                            "Safe" = "darkblue",
                                            "Loss" = "red",
                                            "Gain" = "green"),
                                 na.value = NA)
            
            scens <- unique(intensity$scenario)
            scens <- scens[-which(scens == "0")]
            intensity0 <- intensity %>%
              select(conus.grid.id, scenario, plot.val) %>%
              pivot_wider(names_from = scenario, values_from = plot.val) %>%
              pivot_longer(cols = all_of(scens)) %>%
              rename(current = "0",
                     future = "value",
                     scenario = "name") %>%
              mutate(change = case_when(current == "Absent" & future == "Absent" ~ "Unsuitable",
                                        current == "Present" & future == "Absent" ~ "Loss",
                                        current == "Absent" & future == "Present" ~ "Gain",
                                        current == "Present" & future == "Present" ~ "Safe")) %>%
              filter(is.na(change) == F)
            
            base <- base +
              geom_sf(data = intensity0, aes(fill = change, color = change)) +
              labs(fill = paste0(fill, unc), color = paste0(fill, unc), title = title) +
              facet_wrap(~scenario, labeller = labeller(scenario = labs))
            
          } else {
            # set color scheme
            base <- base +
              scale_fill_gradient2(guide = guide_colorbar(), high = "darkblue", low = "darkred", mid = "white", na.value = NA) +
              scale_color_gradient2(guide = "none", high = "darkblue", low = "darkred", mid = "white", na.value = NA)
            
            scens <- unique(intensity$scenario)
            scens <- scens[-which(scens == "0")]
            intensity0 <- intensity %>%
              select(conus.grid.id, scenario, plot.val) %>%
              pivot_wider(names_from = scenario, values_from = plot.val) %>%
              pivot_longer(cols = all_of(scens)) %>%
              rename(current = "0",
                     future = "value",
                     scenario = "name") %>%
              mutate(change = future - current) %>%
              filter(is.na(change) == F)
            
            base <- base +
              geom_sf(data = intensity0, aes(fill = change, color = change)) +
              guides(fill = guide_colorbar(theme = theme(
                legend.key.height = unit(0.75, "lines"),
                legend.key.width = unit(20, "lines")), title.hjust = 1, title.vjust = 1)) +
              labs(fill = paste0("Absolute change in ", tolower(fill), unc), title = title) +
              facet_wrap(~scenario, labeller = labeller(scenario = labs))
          }
          
          
          
          
          
        } else if (plot.change == "relative") { # Plot estimated relative change
          # set color scheme
          base <- base +
            scale_fill_gradient2(guide = guide_colorbar(), high = "darkblue", low = "darkred", mid = "white", na.value = NA) +
            scale_color_gradient2(guide = "none", high = "darkblue", low = "darkred", mid = "white", na.value = NA)
          
          scens <- unique(intensity$scenario)
          scens <- scens[-which(scens == "0")]
          intensity0 <- intensity %>%
            select(conus.grid.id, scenario, plot.val) %>%
            pivot_wider(names_from = scenario, values_from = plot.val) %>%
            pivot_longer(cols = all_of(scens)) %>%
            rename(current = "0",
                   future = "value",
                   scenario = "name") %>%
            mutate(change = (future - current)/current) %>%
            filter(is.na(change) == F)
          
          base <- base +
            geom_sf(data = intensity0, aes(fill = change, color = change)) +
            guides(fill = guide_colorbar(theme = theme(
              legend.key.height = unit(0.75, "lines"),
              legend.key.width = unit(20, "lines")), title.hjust = 1, title.vjust = 1)) +
            labs(fill = paste0("Relative change in ", tolower(fill), unc), title = title) +
            facet_wrap(~scenario, labeller = labeller(scenario = labs))
          
        } else {
          stop("plot.change must be FALSE, 'relative', or 'absolute'")
        }
        
      } # end plot projections
      
      
    } else { # if plot == 'spat'
      base <- base +
        scale_fill_gradient2(guide = guide_colorbar(), high = "darkblue", low = "darkred", mid = "white", na.value = NA) +
        scale_color_gradient2(guide = "none", high = "darkblue", low = "darkred", mid = "white", na.value = NA)
      
      intensity <- filter(intensity, is.na(plot.val) == F)
      
      base <- base +
        geom_sf(data = intensity, aes(fill = plot.val, color = plot.val)) +
        guides(fill = guide_colorbar(theme = theme(
          legend.key.height = unit(0.75, "lines"),
          legend.key.width = unit(20, "lines")), title.hjust = 1, title.vjust = 1)) +
        labs(fill = paste0(fill, unc), color = paste0(fill, unc), title = title)
    }
    
  }
  
  
  
  
  # put state lines and water on top of fill color
  base <- base +
    geom_sf(data = st, fill = NA) +
    geom_sf(data = wa, fill = "lightsteelblue")
  
  
  
  # __d. range ----
  if (plot.range == T) {
    base <- base +
      geom_sf(data = region$range, fill = NA, linewidth = 0.65)
  }
  
  # __e. blocks ----
  if (plot.blocks == T) {
    
    blocks <- st_intersection(blocks, region$region)
    
    if (length(unique(blocks$folds)) == 1) {
      base <- base +
        geom_sf(data = blocks, size = 1, linewidth = 1, aes(fill = as.factor(folds)), color = NA, alpha = 0.5) +
        scale_fill_manual(values = blockcols, name = "Out-of-sample test area", label = paste0("Block ", blocks$folds[1]))
    } else {
      base <- base +
        geom_sf(data = blocks, size = 1, linewidth = 1, aes(fill = as.factor(folds)), color = NA, alpha = 0.3) +
        scale_fill_manual(values = blockcols, name = "Block")
    }
    
    
  }
  
  # __f. samples ----
  if (plot == "samples") {
    
    if (details == T) {
      # plotpoints <- species.data$locs$cont %>%
      #   filter(year >= year.start,
      #          year <= year.end,
      #          survey.conducted == 1) %>%
      #   select(!unique.id) %>%
      #   distinct() %>%
      #   group_by(source) %>%
      #   mutate(nsites = length(unique(site.id))) %>%
      #   group_by(source, site.id) %>%
      #   mutate(nvisits = n()) %>%
      #   slice_head() %>%
      #   ungroup()
      # 
      # sourcesummary <- plotpoints %>%
      #   st_drop_geometry() %>%
      #   select(source, data.type, nsites, nvisits) %>%
      #   group_by(source, data.type) %>%
      #   summarize(nsites = mean(nsites),
      #             nvisits = median(nvisits),
      #             .groups = "drop") %>%
      #   mutate(nvisits = case_when(data.type == "PO" ~ "",
      #                              T ~ paste0(", ", nvisits)))
      # 
      # plotpoints <- plotpoints %>%
      #   select(!c(nsites, nvisits)) %>%
      #   full_join(sourcesummary, by = c("source", "data.type")) %>%
      #   mutate(label = paste0(source, " (", data.type, ": ", nsites, nvisits, ")"))
      
      locs <- species.data$locs$cont %>%
        st_drop_geometry() %>%
        select(site.id, source) %>%
        distinct()
      
      n.sites <- as.data.frame(table(locs$source))
      colnames(n.sites) <- c("source", "n.sites")
      
      locs <- species.data$locs$cont %>%
        st_drop_geometry() %>%
        select(site.id, survey.id, source)  %>%
        distinct() 
      
      n.surveys <- as.data.frame(table(locs$source, locs$site.id)) %>%
        filter(Freq != 0) %>%
        group_by(Var1) %>%
        summarize(n.visits = median(Freq))
      colnames(n.surveys) <- c("source", "n.visits")
      
      tab <- full_join(n.sites, n.surveys, by = "source") %>%
        select(source, n.sites, n.visits)
      
      plotpoints <- species.data$locs$cont %>%
        full_join(tab, by = "source") %>%
        mutate(label = paste0(source, " (", data.type, ": ", n.sites, ", ", n.visits, ")"))
    } else {
      plotpoints <- species.data$locs$cont %>%
        filter(year >= year.start,
               year <= year.end,
               survey.conducted == 1) 
    }
    
    
    plotpoints <- filter(plotpoints, year <= year.end & year >= year.start)
    
    
    
    if (details == T) {
      base <- base +
        geom_sf(data = plotpoints, aes(color = label, shape = data.type), alpha = 1, size = 1.5) +
        scale_shape_manual(values = c("PO" = 4, "DND" = 0, "count" = 15, "CMR" = 17)) +
        labs(color = "Data source (data type: number of sites, \nmedian visits per site)",
             shape = "Data type",
             subtitle = paste0("All available data between ", year.start, " and ", year.end),
             title = title)
    } else {
      base <- base +
        geom_sf(data = plotpoints, aes(color = source, shape = data.type), alpha = 1, size = 1.5) +
        scale_shape_manual(values = c("PO" = 4, "DND" = 0, "count" = 15, "CMR" = 17)) +
        labs(color = "Data source",
             shape = "Data type",
             subtitle = paste0("All available data between ", year.start, " and ", year.end),
             title = title)
    }
    
  }
  
  
  
  
  
  
  # 4. add limits ----
  if (plot.blocks == F) {
    bb <- st_bbox(region$region)
    xlim <- c(bb[1], bb[3])
    ylim <- c(bb[2], bb[4])
  } else {
    bb <- st_bbox(st_union(region$region, blocks))
    xlim <- c(bb[1], bb[3])
    ylim <- c(bb[2], bb[4])
  }
  
  base <- base +
    coord_sf(xlim = xlim, ylim = ylim)
  
  
  
  
  
  # 5. change theme ----
  if (plot == "samples") {
    base <- base +
      theme(legend.key = element_rect(fill = "gray90"),
            axis.text.x = element_text(angle = 45,
                                       hjust = 1,
                                       vjust = 1))
  } else {
    base <- base +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45,
                                       hjust = 1,
                                       vjust = 1),
            strip.text = element_text(size = 12))
  }
  
  base
  
  
  
}
