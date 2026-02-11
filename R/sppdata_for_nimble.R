#' Format species data for nimble
#'
#' @param species.data (list) output from load_species_data()
#' @param region (list) output from make_region()
#' @param file.info (data.frame) columns 'file.name', 'file.label', 'covar.mean', and 'covar.sum' describing the datasets to be read in for this species
#' @param covar (data.frame) dataframe containing covariates for PO data
#' @param stategrid (data.frame) output from make_state_grid()
#' @param statelines.rm (logical) should state-specific PO data in cells that cross state lines be removed (T) or not (F)
#' @param covs.inat (character vector) vector of column names (from covar) to use for effort covariates for iNat data
#' @param covs.PO (character vector) vector of column names (from covar) to use for effort covariates for non-iNat PO data
#' @param DND.maybe (numeric) whether to treat maybe detections as detections (1) or non-detections(0); defaults to 1
#' @param keep.conus.grid.id (vector) vector of conus.grid.ids to use to fit the model
#' @param min.visits.incl (numeric) minimum number (inclusive) of median visits per site to use n-mixture or occupancy models
#'
#' @returns (list) list of species data partially formatted for nimble, needs to be input into data_for_nimble before use in nimble
#' @export
#'
#' @importFrom tidyselect any_of all_of
#' @importFrom tibble add_column
#' @importFrom readr read_rds
#' @importFrom lubridate yday
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate filter select group_by summarize full_join across arrange
#' @importFrom rnaturalearth ne_states


sppdata_for_nimble <- function(species.data,
                               region,
                               file.info,
                               covar,
                               stategrid,
                               statelines.rm = T,
                               covs.inat = c("traveltime", "density", "n.inat"),
                               covs.PO = c("traveltime", "density"),
                               DND.maybe = 1,
                               keep.conus.grid.id,
                               min.visits.incl = 3) {

  # Make sure file.info matches data in species.data
  file.info <- filter(file.info, .data$file.label %in% names(species.data$obs))
  file.info <- file.info[order(match(file.info$file.label, names(species.data$obs))),] %>% distinct()

  # remove area
  file.info$area <- ""

  # Pull info from file.info
  data.type <- file.info$data.type
  PO.extent <- file.info$PO.extent
  covs.mean <- file.info$covar.mean
  covs.mean[is.na(covs.mean)] <- ""
  covs.sum <- file.info$covar.sum
  covs.sum[is.na(covs.sum)] <- ""
  offset.area <- file.info$area
  PO.extent <- file.info$PO.extent

  # these are indices of datasets that don't have any data (i.e., wrong years, species, etc.)
  # they will be removed at the end
  rm.ind <- c()

  # this is where the datasets will go once they're set up for nimble
  dat.ready <- list()


  file.names <- names(species.data$obs)


  counter <- 1 # This numbers each dataset

  # do PO ----
  POind <- which(data.type == "PO")
  if (length(POind) > 0) {

    # __ Process iNat ----
    inat.ind <- grep("iNaturalist", file.names)

    if (length(inat.ind) > 1) {
      # If there is more than one labeled iNat, error

      stop("There cannot be more than one dataset labeled 'iNaturalist'")

    } else if (length(inat.ind) == 0) {
      # If there are none labeled iNat, message

      cat(paste0("No iNaturalist data for this species\n"))

    } else if (length(inat.ind) == 1) {
      # If there is one labeled iNat, process


      if (sum(file.info$data.type == "PO") > 1) {
        cat("Looking for duplicates in PO data\n")
        rm <- id_dup_records(species.data)

        if (length(rm) > 0) {
          cat(paste0("    Removing ", length(rm), " records from iNaturalist that may be duplicated in other datasets\n"))
        }
      } else {
        rm <- c()
      }


      # Filter PO data
      data <- species.data$obs[[inat.ind]]
      name <- file.names[inat.ind]

      inat.start <- data
      cat("\nLoading PO")

      # Aggregate iNat to grid cells
      POdata <- inat.start %>%

        # remove records that were identified as duplicates
        mutate(site.survey.id = paste0(.data$site.id, "-", .data$survey.id)) %>%
        filter((.data$unique.id %in% rm) == F) %>%
        select(!"site.survey.id") %>%

        # aggregate to grid cell
        group_by(.data$conus.grid.id) %>%
        summarize(count = n()) %>%

        # add in the rest of grid.ids and fill 0s if there were no records (NA)
        full_join(select(covar, "conus.grid.id"), by = "conus.grid.id") %>%
        mutate(count = case_when(is.na(count) ~ 0,
                                               T ~ count)) %>%
        filter(is.na(.data$conus.grid.id) == F,
               .data$conus.grid.id %in% keep.conus.grid.id) # get rid of cells that we're excluding

      # Get the covariates to use for iNat
      covs.inat[which(is.na(covs.inat))] <- ""
      covariates <- select(covar, "conus.grid.id", any_of(covs.inat)) %>%
        filter(is.na(.data$conus.grid.id) == F,
               .data$conus.grid.id %in% keep.conus.grid.id)
      missing <- setdiff(covs.inat, names(covariates))
      missing <- missing[!missing == ""]
      missing <- missing[!is.na(missing)]
      if (length(missing) > 0) warning("iNat effort covariate(s) ", missing, " missing")

      # make sure everything is in the right order
      POdata <- POdata[order(match(POdata$conus.grid.id, keep.conus.grid.id)),]
      covariates <- covariates %>%
                    mutate(order_index = match(.data$conus.grid.id, keep.conus.grid.id)) %>%
                    arrange(.data$order_index) %>%
                    select(-"order_index")

      PO.inat <- PO_for_nimble(POdata, covariates,
                               rename = counter)
      PO.inat$constants[[paste0("name", counter)]] <- name
      PO.inat$data[[paste0("area", counter)]] <- rep(1, nrow(POdata))


      # iNat always has a logit link so it needs an intercept
        PO.inat$data[[paste0("Xw", counter)]] <- PO.inat$data[[paste0("Xw", counter)]] %>%
          add_column(.before = 1, intercept = 1)
        PO.inat$constants[[paste0("nCovW", counter)]] <- PO.inat$constants[[paste0("nCovW", counter)]] + 1

      # Add dataset to data list
      dat.ready[[paste0("PO", counter)]] <- PO.inat

      # Add one to dataset counter
      counter <- counter + 1

    }

    # __ Process other PO ----
    covs.PO[which(is.na(covs.PO))] <- ""

    if (length(inat.ind) > 0) {
      other.ind <- POind[-which(POind == inat.ind)]
    } else {
      other.ind <- POind
    }

    if (length(other.ind) >= 1) {

      if (is.null(PO.extent)) stop("non-iNaturalist PO datasets require PO.extent ('CONUS' or state code) column in file.info")
      # ____CONUS extent ----
      # do each of these separately
      con.ext <- which(PO.extent[other.ind] == "CONUS")
      con.ind <- other.ind[con.ext]

      if (length(con.ind) > 0) {
        for (o in 1:length(con.ind)) {

          # Filter dataset
          data <- species.data$obs[[con.ind[o]]]
          name <- file.names[con.ind[o]]

          other.start <- data
          cat("\nLoading PO")

          # Aggregate records to grid.ids
          POdata <- other.start %>%

            # aggregate to grid cell
            group_by(.data$conus.grid.id) %>%
            summarize(count = n()) %>%

            # add in the rest of grid.ids and fill 0s if there were no records (NA)
            full_join(select(covar, "conus.grid.id"), by = "conus.grid.id") %>%
            mutate(count = case_when(is.na(count) ~ 0,
                                                   T ~ count)) %>%
            filter(is.na(.data$conus.grid.id) == F,
                   .data$conus.grid.id %in% keep.conus.grid.id) # get rid of cells that we're excluding

          covariates <- select(covar, "conus.grid.id", any_of(covs.PO)) %>%
            filter(is.na(.data$conus.grid.id) == F,
                   .data$conus.grid.id %in% keep.conus.grid.id)

          # make sure everything is in the right order
          POdata <- POdata[order(match(POdata$conus.grid.id, keep.conus.grid.id)),]
          covariates <- covariates %>%
            mutate(order_index = match(.data$conus.grid.id, keep.conus.grid.id)) %>%
            arrange(.data$order_index) %>%
            select(-"order_index")

          PO.other <- PO_for_nimble(POdata, covariates,
                                    rename = counter)

          PO.other$constants[[paste0("name", counter)]] <- name
          PO.other$data[[paste0("area", counter)]] <- rep(1, nrow(POdata))

          # Add dataset to data list
          dat.ready[[paste0("PO", counter)]] <- PO.other

          # Add one to dataset counter
          counter <- counter + 1


          # Next dataset
        }
      } # End conus-extent datasets



      # ____state extent ----
      # these are all going to be combined into one
      sta.ext <- which(PO.extent[other.ind] != "CONUS")
      sta.ind <- other.ind[sta.ext]

      if (length(sta.ind) > 0) {

        sta.dat <- c()
        for (o in 1:length(sta.ind)) {
          dat <- species.data$obs[[sta.ind[o]]]
          dat$state <- PO.extent[sta.ind[o]]

          sta.dat <- bind_rows(sta.dat, dat)
        }
        name <- paste0(file.names[sta.ind], collapse = ", ")

        other.start <- sta.dat
        cat("\nLoading PO")

        # Aggregate to grid.ids
        POdata <- other.start %>%

          # aggregate to grid cell
          group_by(.data$conus.grid.id) %>%
          summarize(count = n(), .groups = "drop") %>%

          # add in the rest of grid.ids and fill 0s if there were no records (NA)
          full_join(select(covar, "conus.grid.id"), by = "conus.grid.id") %>%
          mutate(count = case_when(is.na(count) ~ 0,
                                                 T ~ count)) %>%
          filter(is.na(.data$conus.grid.id) == F,
                 .data$conus.grid.id %in% keep.conus.grid.id) # get rid of cells that we're excluding

        # Get covariates
        states <- unique(other.start$state)
        if (length(states) >= 2) {
          # st <- ne_states(country = c("Canada", "Mexico", "United States of America"),
          #                 returnclass = "sf") %>%
          #   st_transform(st_crs(region$sp.grid))
          #
          # cat("\nAssigning grid cells to states")
          # suppressWarnings(stategrid <- st_intersection(region$sp.grid, st) %>%
          #   st_drop_geometry() %>%
          #     mutate(value = 1,
          #            name = postal,
          #            conus.grid.id = as.character(conus.grid.id)) %>%
          #   select("conus.grid.id", "name", "value"))
          # stategrid is now read in as an argument

          # stategrid <- get("stategrid", envir = asNamespace('SpFut.flexiSDM'))

          all.states <- stategrid %>%
            pivot_wider(names_from = name, values_from = "value", values_fill = 0) %>%
            filter(.data$conus.grid.id %in% region$sp.grid$conus.grid.id)

          # leave out first state because this is for state intercept
          statecols <- states[2:length(states)]
          stateints <- all.states[,c("conus.grid.id", statecols)]
          covariates <- covar %>%
            mutate(conus.grid.id = as.character(.data$conus.grid.id)) %>%
            inner_join(stateints, by = "conus.grid.id") %>%
            select("conus.grid.id", any_of(covs.PO), all_of(statecols)) %>%
            filter(is.na(.data$conus.grid.id) == F,
                   .data$conus.grid.id %in% keep.conus.grid.id)

          # # use all states because this is for S (whether cell is in state with PO data or not)
          # covariates1 <- covar %>%
          #   inner_join(all.states, by = "conus.grid.id") %>%
          #   select(conus.grid.id, all_of(covs.PO), all_of(states)) %>%
          #   filter(is.na(conus.grid.id) == F,
          #                 conus.grid.id %in% keep.conus.grid.id)
        # } else if (length(states) == 1) {
        #   # grid.state <- readr::read_rds("data/USA/grid-states.rds")
        #   utils::data("grid_states")
        #   all.states <- stategrid %>%
        #     pivot_wider(names_from = name, values_from = value, values_fill = 0) %>%
        #     filter(conus.grid.id %in% region$sp.grid$conus.grid.id)
        #
        #   # don't need intercept since it's just one state
        #   covariates <- covar %>%
        #     select(conus.grid.id, all_of(covs.PO)) %>%
        #     filter(is.na(conus.grid.id) == F,
        #                   conus.grid.id %in% keep.conus.grid.id)
        #
        #   # # use states for S (whether cell is in state with PO data or not)
        #   # covariates1 <- inner_join(covar, grid.state, by = "conus.grid.id") %>%
        #   #                 select(conus.grid.id, all_of(covs.PO), all_of(states)) %>%
        #   #   filter(is.na(conus.grid.id) == F,
        #   #                 conus.grid.id %in% keep.conus.grid.id)
        } else {
          covariates <- select(covar, "conus.grid.id", any_of(covs.PO)) %>%
            filter(is.na(.data$conus.grid.id) == F,
                   .data$conus.grid.id %in% keep.conus.grid.id)
        }

        # covariates <- covariates %>%
        #   filter(is.na(conus.grid.id) == F,
        #                 conus.grid.id %in% keep.conus.grid.id)

        # make sure everything is in the right order
        POdata <- POdata[order(match(POdata$conus.grid.id, keep.conus.grid.id)),]
        covariates <- covariates %>%
          mutate(order_index = match(.data$conus.grid.id, keep.conus.grid.id)) %>%
          arrange(.data$order_index) %>%
          select(-"order_index")

        if (statelines.rm == T) {
          # only keep cells that do not cross state lines
          POdata <- POdata %>% filter(.data$conus.grid.id %in% region$sp.grid$conus.grid.id[which(region$sp.grid$nstate == "single")])
          covariates <- covariates %>% filter(.data$conus.grid.id %in% region$sp.grid$conus.grid.id[which(region$sp.grid$nstate == "single")])

        }


        PO.other <- PO_for_nimble(POdata, covariates, rename = counter)
        PO.other$constants[[paste0("name", counter)]] <- name
        PO.other$constants[[paste0("states", counter)]] <- states
        PO.other$data[[paste0("area", counter)]] <- rep(1, nrow(POdata))

        # if (length(states) > 1) PO.other$data[[paste0("S", counter)]] <- apply(covariates1[,states], 1, max) # use all states
        # if (length(states) == 1) PO.other$data[[paste0("S", counter)]] <- covariates1[,states] # use one state


        # Add dataset to data list
        dat.ready[[paste0("PO", counter)]] <- PO.other

        # Add one to dataset counter
        counter <- counter + 1

      }


    } # end non-iNat POs


  } # end set up PO datasets


  # do DND ----
  DNDind <- which(data.type == "DND")
  DNDlabs <- file.info$file.label[DNDind]
  if (length(DNDind) > 0) {
    for (d in 1:length(DNDind)) {

      # pull out covariates for this dataset
      dnd.covs.mean <- unlist(strsplit(covs.mean[DNDind[d]], split = ", "))
      covs.mean1 <- dnd.covs.mean[which(is.na(dnd.covs.mean) == F)]

      dnd.covs.sum <- unlist(strsplit(covs.sum[DNDind[d]], split = ", "))
      covs.sum1 <- dnd.covs.sum[which(is.na(dnd.covs.sum) == F)]

      cov.names <- c(covs.mean1, covs.sum1)

      dnd.area <- unlist(strsplit(offset.area[DNDind[d]], split = ", "))
      dnd.area <- dnd.area[which(is.na(dnd.area) == F)]


      # data <- species.data$obs[[DNDind[d]]]
      data <- species.data$obs[[DNDlabs[d]]]
      name <- file.names[DNDind[d]]

      # Add yday column
      if ("yday" %in% cov.names) {
        tmp <- as.Date(paste0(data$month, "-", data$day, "-", data$year), format = "%m-%d-%Y")
        data$yday <- yday(tmp)
      }

      # warning about missing covariates
      missing <- setdiff(cov.names, names(data))
      missing <- missing[!missing == ""]
      missing <- missing[!is.na(missing)]
      if (length(missing) > 0) warning(name, " detection covariate(s) ", missing, " missing")

      # filter and aggregate DND observations
      dndstart <- DND_filter(data,
                             covs.mean1,
                             covs.sum1,
                             DND.maybe = 1,
                             offset.area = dnd.area)


      # remove if only one data point
      if(nrow(dndstart) < 2) {
        cat("Only one data point, removing dataset\n")
        rm.ind <- c(rm.ind, DNDind[d])
        dat.ready[[paste0("DND", length(dat.ready)+1)]] <- NA

      } else {

        # clean up columns
        data <- dndstart %>%
          select("conus.grid.id", "site.id", "survey.id",
                 any_of(cov.names), "count") %>%
          group_by(.data$site.id) %>%
          mutate(site.id = cur_group_id()) %>%
          ungroup() %>%
          mutate(across(any_of(cov.names), scale_this))

        if ("yday" %in% cov.names) {
          data$yday2 <- data$yday * data$yday
          cov.names <- c(cov.names, "yday2")
        }

        if (offset.area[DNDind[d]] != "") {
          area <- data$area
          data$area <- NULL
        }

        # format for nimble
        data <- filter(data, .data$conus.grid.id %in% keep.conus.grid.id)
        DND <- survey_for_nimble(data,
                                 cov.names,
                                 type = "DND",
                                 rename = counter)

        # if this is going to have a logit link (ie nVisits >= min.visits.incl)
        # it needs an intercept
        if (DND$constants[[paste0("nVisitsV", counter)]] >= min.visits.incl) {
          DND$data[[paste0("Xv", counter)]] <- DND$data[[paste0("Xv", counter)]] %>%
                                                add_column(.before = 1, intercept = 1)
          DND$constants[[paste0("nCovV", counter)]] <- DND$constants[[paste0("nCovV", counter)]] + 1
        }

        DND$constants[[paste0("name", counter)]] <- name

        if(offset.area[DNDind[d]] != "") {
          DND$data[[paste0("area", counter)]] <- area
        } else {
          DND$data[[paste0("area", counter)]] <- rep(1, length(DND$constants[[paste0("Vcells", counter)]]))
        }


        # Add dataset to data list
        dat.ready[[paste0("DND", counter)]] <- DND

        # Add one to counter
        counter <- counter + 1

      }

    } # end loop through DND datasets

  } # end set up DND datasets



  # do count ----
  countind <- which(data.type %in% c("Count", "count"))
  countlabs <- file.info$file.label[countind]
  if (length(countind) > 0) {

    for (d in 1:length(countind)) {

      # pull out covariates for this dataset
      count.covs.mean <- unlist(strsplit(covs.mean[countind[d]], split = ", "))
      covs.mean1 <- count.covs.mean[which(is.na(count.covs.mean) == F)]

      count.covs.sum <- unlist(strsplit(covs.sum[countind[d]], split = ", "))
      covs.sum1 <- count.covs.sum[which(is.na(count.covs.sum) == F)]

      count.covs <- c(covs.mean1, covs.sum1)


      count.area <- unlist(strsplit(offset.area[countind[d]], split = ", "))
      count.area <- count.area[which(is.na(count.area) == F)]


      # data <- species.data$obs[[countind[d]]]
      data <- species.data$obs[[countlabs[d]]]
      name <- data$source[1]

      # Add yday
      if ("yday" %in% count.covs) {
        tmp <- as.Date(paste0(data$month, "-", data$day, "-", data$year), format = "%m-%d-%Y")
        data$yday <- yday(tmp)
      }

      # warning about missing covariates
      missing <- setdiff(count.covs, names(data))
      missing <- missing[!missing == ""]
      missing <- missing[!is.na(missing)]
      if (length(missing) > 0) warning(name, " detection covariate(s) ", missing, " missing")

      # Filter data
      count.start <- count_filter(data,
                                  covs.mean1,
                                  covs.sum1,
                                  offset.area = count.area)

      # remove if 0 or 1 data point
      if(nrow(count.start) < 2) {
        cat("Only one data point, removing dataset\n")
        rm.ind <- c(rm.ind, countind[d])
        dat.ready[[paste0("count", counter)]] <- NA

      } else {

        # clean up columns
        data <- count.start %>%
          select("conus.grid.id", "site.id", "survey.id", any_of(count.covs), any_of("area"), "count") %>%
          group_by(.data$site.id) %>%
          mutate(site.id = cur_group_id()) %>%
          ungroup() %>%
          mutate(across(any_of(count.covs), scale_this))

        # add yday quadratic column
        if ("yday" %in% count.covs) {
          yday2 <- data$yday * data$yday
          data <- data %>%
            add_column(.after = "yday", yday2)

          count.covs <- c(count.covs, "yday2")
        }

        if (length(count.area) > 0) {
          area <- data$area
          data$area <- NULL
        }

        # format for nimble
        data <- filter(data, .data$conus.grid.id %in% keep.conus.grid.id)
        COUNT <- survey_for_nimble(data,
                                   count.covs,
                                   type = "count",
                                   rename = counter)

        # if this is going to have a logit link (ie nVisits >= min.visits.incl)
        # it needs an intercept
        if (COUNT$constants[[paste0("nVisitsY", counter)]] >= min.visits.incl) {
          COUNT$data[[paste0("Xy", counter)]] <- COUNT$data[[paste0("Xy", counter)]] %>%
            add_column(.before = 1, intercept = 1)
          COUNT$constants[[paste0("nCovY", counter)]] <- COUNT$constants[[paste0("nCovY", counter)]] + 1
        }

        COUNT$constants[[paste0("name", counter)]] <- name

        if(length(count.area) > 0) {
          COUNT$data[[paste0("area", counter)]] <- area
        } else {
          COUNT$data[[paste0("area", counter)]] <- rep(1, length(COUNT$constants[[paste0("Ycells", counter)]]))
        }

        # add dataset to data list
        dat.ready[[paste0("count", counter)]] <- COUNT

        # add one to counter
        counter <- counter + 1

      }

    }

  } # end set up count datasets

  if (length(rm.ind) > 0)   dat.ready <- dat.ready[-rm.ind]

  return(dat.ready)
}
