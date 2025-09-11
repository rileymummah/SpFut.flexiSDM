#' Format species data for nimble
#'
#' @param species.data (list) output from load_species_data()
#' @param region (list) output from make_region()
#' @param data.type (character vector) vector of data types for each dataset in species.data
#' @param PO.extent (character vector) vector of extents for each PO dataset (CONUS or state code); use NA for non-PO datasets
#' @param covar (data.frame) dataframe containing covariates for PO data
#' @param covs.inat (character vector) vector of column names (from covar) to use for effort covariates for iNat data
#' @param covs.PO (character vector) vector of column names (from covar) to use for effort covariates for non-iNat PO data
#' @param covs.mean (list) list containing one vector for each dataset that says which columns to use as detection covariates that should be averaged (e.g., water temperature)
#' @param covs.sum (list) list containing one vector for each dataset that says which columns to use as detection covariates that should be summed (e.g., survey duration)
#' @param offset.area (character vector) vector containing column names to use for area offset for each dataset
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
#'
#' @examples


sppdata_for_nimble <- function(species.data,
                               region,
                               data.type,
                               PO.extent,
                               covar,
                               covs.inat = c("traveltime", "density", "n.inat"),
                               covs.PO = c("traveltime", "density"),
                               covs.mean,
                               covs.sum,
                               offset.area,
                               DND.maybe = 1,
                               keep.conus.grid.id,
                               min.visits.incl = 3) {


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

      cat("Looking for duplicates in PO data\n")
      rm <- id_dup_records(species.data)

      if (length(rm) > 0) {
        cat(paste0("Removing ", length(rm), " records from iNaturalist that may be duplicated in other datasets\n"))
      }

      # Filter PO data
      data <- species.data$obs[[inat.ind]]
      name <- file.names[inat.ind]

      inat.start <- data
      cat("\nLoading PO")

      # Aggregate iNat to grid cells
      POdata <- inat.start %>%

        # remove records that were identified as duplicates
        dplyr::mutate(site.survey.id = paste0(site.id, "-", survey.id)) %>%
        dplyr::filter((unique.id %in% rm) == F) %>%
        dplyr::select(!site.survey.id) %>%

        # aggregate to grid cell
        dplyr::group_by(conus.grid.id) %>%
        dplyr::summarize(count = dplyr::n()) %>%

        # add in the rest of grid.ids and fill 0s if there were no records (NA)
        dplyr::full_join(select(covar, conus.grid.id), by = "conus.grid.id") %>%
        dplyr::mutate(count = dplyr::case_when(is.na(count) ~ 0,
                                               T ~ count)) %>%
        dplyr::filter(is.na(conus.grid.id) == F,
                      conus.grid.id %in% keep.conus.grid.id) # get rid of cells that we're excluding

      # Get the covariates to use for iNat
      covariates <- dplyr::select(covar, conus.grid.id, tidyselect::all_of(covs.inat)) %>%
        dplyr::filter(is.na(conus.grid.id) == F,
                      conus.grid.id %in% keep.conus.grid.id)

      # make sure everything is in the right order
      POdata <- POdata[order(match(POdata$conus.grid.id, keep.conus.grid.id)),]
      if (ncol(covariates) > 1) {
        covariates <- covariates[order(match(covariates$conus.grid.id, keep.conus.grid.id)),]
      } else {
        covariates <- as.data.frame(covariates[order(match(covariates$conus.grid.id, keep.conus.grid.id)),])
        colnames(covariates)[1] <- "conus.grid.id"
      }

      PO.inat <- PO_for_nimble(POdata, covariates,
                               rename = counter)
      PO.inat$constants[[paste0("name", counter)]] <- name
      PO.inat$data[[paste0("area", counter)]] <- rep(1, nrow(POdata))


      # iNat always has a logit link so it needs an intercept
        PO.inat$data[[paste0("Xw", counter)]] <- PO.inat$data[[paste0("Xw", counter)]] %>%
          tibble::add_column(.before = 1, intercept = 1)
        PO.inat$constants[[paste0("nCovW", counter)]] <- PO.inat$constants[[paste0("nCovW", counter)]] + 1



      # Add dataset to data list
      dat.ready[[paste0("PO", counter)]] <- PO.inat

      # Add one to dataset counter
      counter <- counter + 1

    }

    # __ Process other PO ----
    if (length(inat.ind) > 0) {
      other.ind <- POind[-which(POind == inat.ind)]
    } else {
      other.ind <- POind
    }

    if (length(other.ind) >= 1) {

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
            dplyr::group_by(conus.grid.id) %>%
            dplyr::summarize(count = dplyr::n()) %>%

            # add in the rest of grid.ids and fill 0s if there were no records (NA)
            dplyr::full_join(dplyr::select(covar, conus.grid.id), by = "conus.grid.id") %>%
            dplyr::mutate(count = dplyr::case_when(is.na(count) ~ 0,
                                                   T ~ count)) %>%
            dplyr::filter(is.na(conus.grid.id) == F,
                          conus.grid.id %in% keep.conus.grid.id) # get rid of cells that we're excluding

          covariates <- dplyr::select(covar, conus.grid.id, tidyselect::all_of(covs.PO)) %>%
            dplyr::filter(is.na(conus.grid.id) == F,
                          conus.grid.id %in% keep.conus.grid.id)

          # make sure everything is in the right order
          POdata <- POdata[order(match(POdata$conus.grid.id, keep.conus.grid.id)),]
          covariates <- covariates[order(match(covariates$conus.grid.id, keep.conus.grid.id)),]

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

          sta.dat <- dplyr::bind_rows(sta.dat, dat)
        }
        name <- paste0(file.names[sta.ind], collapse = ", ")

        other.start <- sta.dat
        cat("\nLoading PO")

        # Aggregate to grid.ids
        POdata <- other.start %>%

          # aggregate to grid cell
          dplyr::group_by(conus.grid.id) %>%
          dplyr::summarize(count = dplyr::n(), .groups = "drop") %>%

          # add in the rest of grid.ids and fill 0s if there were no records (NA)
          dplyr::full_join(dplyr::select(covar, conus.grid.id), by = "conus.grid.id") %>%
          dplyr::mutate(count = dplyr::case_when(is.na(count) ~ 0,
                                                 T ~ count)) %>%
          dplyr::filter(is.na(conus.grid.id) == F,
                        conus.grid.id %in% keep.conus.grid.id) # get rid of cells that we're excluding

        # Get covariates
        states <- unique(other.start$state)
        if (length(states) >= 2) {
          # all.states <- readr::read_rds("data/USA/grid-states.rds") %>%
          #                 dplyr::filter(conus.grid.id %in% region$sp.grid$conus.grid.id)
          utils::data("stategrid")
          all.states <- stategrid %>%
            tidyr::pivot_wider(names_from = name, values_from = value, values_fill = 0) %>%
            dplyr::filter(conus.grid.id %in% region$sp.grid$conus.grid.id)

          # make sure these cells are in the same order as the cells in data and constants
          #all.states <- all.states[order(match(all.states$conus.grid.id, covar$conus.grid.id)),]

          # leave out first state because this is for state intercept
          statecols <- states[2:length(states)]
          stateints <- all.states[,c("conus.grid.id", statecols)]
          covariates <- covar %>%
            dplyr::inner_join(stateints, by = "conus.grid.id") %>%
            dplyr::select(conus.grid.id, tidyselect::all_of(covs.PO), tidyselect::all_of(statecols)) %>%
            dplyr::filter(is.na(conus.grid.id) == F,
                          conus.grid.id %in% keep.conus.grid.id)

          # # use all states because this is for S (whether cell is in state with PO data or not)
          # covariates1 <- covar %>%
          #   dplyr::inner_join(all.states, by = "conus.grid.id") %>%
          #   dplyr::select(conus.grid.id, tidyselect::all_of(covs.PO), tidyselect::all_of(states)) %>%
          #   dplyr::filter(is.na(conus.grid.id) == F,
          #                 conus.grid.id %in% keep.conus.grid.id)
        # } else if (length(states) == 1) {
        #   # grid.state <- readr::read_rds("data/USA/grid-states.rds")
        #   utils::data("grid_states")
        #   all.states <- stategrid %>%
        #     tidyr::pivot_wider(names_from = name, values_from = value, values_fill = 0) %>%
        #     dplyr::filter(conus.grid.id %in% region$sp.grid$conus.grid.id)
        #
        #   # don't need intercept since it's just one state
        #   covariates <- covar %>%
        #     dplyr::select(conus.grid.id, tidyselect::all_of(covs.PO)) %>%
        #     dplyr::filter(is.na(conus.grid.id) == F,
        #                   conus.grid.id %in% keep.conus.grid.id)
        #
        #   # # use states for S (whether cell is in state with PO data or not)
        #   # covariates1 <- dplyr::inner_join(covar, grid.state, by = "conus.grid.id") %>%
        #   #                 dplyr::select(conus.grid.id, tidyselect::all_of(covs.PO), tidyselect::all_of(states)) %>%
        #   #   dplyr::filter(is.na(conus.grid.id) == F,
        #   #                 conus.grid.id %in% keep.conus.grid.id)
        } else {
          covariates <- dplyr::select(covar, conus.grid.id, tidyselect::all_of(covs.PO)) %>%
            dplyr::filter(is.na(conus.grid.id) == F,
                          conus.grid.id %in% keep.conus.grid.id)
        }

        # covariates <- covariates %>%
        #   dplyr::filter(is.na(conus.grid.id) == F,
        #                 conus.grid.id %in% keep.conus.grid.id)

        # make sure everything is in the right order
        POdata <- POdata[order(match(POdata$conus.grid.id, keep.conus.grid.id)),]
        covariates <- covariates[order(match(covariates$conus.grid.id, keep.conus.grid.id)),]
        #covariates1 <- covariates1[order(match(covariates1$conus.grid.id, keep.conus.grid.id)),]

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


      data <- species.data$obs[[DNDind[d]]]
      name <- file.names[DNDind[d]]

      # Add yday column
      if ("yday" %in% cov.names) {
        tmp <- as.Date(paste0(data$month, "-", data$day, "-", data$year), format = "%m-%d-%Y")
        data$yday <- lubridate::yday(tmp)
      }


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
          dplyr::select(conus.grid.id, site.id, survey.id, tidyselect::any_of(cov.names), count) %>%
          dplyr::group_by(site.id) %>%
          dplyr::mutate(site.id = dplyr::cur_group_id()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(dplyr::across(tidyselect::any_of(cov.names), scale_this))

        if ("yday" %in% cov.names) {
          data$yday2 <- data$yday * data$yday
          cov.names <- c(cov.names, "yday2")
        }

        if (offset.area[DNDind[d]] != "") {
          area <- data$area
          data$area <- NULL
        }

        # format for nimble
        DND <- survey_for_nimble(data,
                                 cov.names,
                                 type = "DND",
                                 rename = counter,
                                 keep.conus.grid.id = keep.conus.grid.id)

        # if this is going to have a logit link (ie nVisits >= min.visits.incl)
        # it needs an intercept
        if (DND$constants[[paste0("nVisitsV", counter)]] >= min.visits.incl) {
          DND$data[[paste0("Xv", counter)]] <- DND$data[[paste0("Xv", counter)]] %>%
                                                tibble::add_column(.before = 1, intercept = 1)
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
  countind <- which(data.type == "Count")
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


      data <- species.data$obs[[countind[d]]]
      name <- data$source[1]

      # Add yday
      if ("yday" %in% count.covs) {
        tmp <- as.Date(paste0(data$month, "-", data$day, "-", data$year), format = "%m-%d-%Y")
        data$yday <- lubridate::yday(tmp)
      }


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
          dplyr::select(conus.grid.id, site.id, survey.id, tidyselect::any_of(count.covs), tidyselect::any_of("area"), count) %>%
          dplyr::group_by(site.id) %>%
          dplyr::mutate(site.id = dplyr::cur_group_id()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(dplyr::across(tidyselect::any_of(count.covs), scale_this))

        # add yday quadratic column
        if ("yday" %in% count.covs) {
          yday2 <- data$yday * data$yday
          data <- data %>%
            tibble::add_column(.after = "yday", yday2)

          count.covs <- c(count.covs, "yday2")
        }

        if (length(count.area) > 0) {
          area <- data$area
          data$area <- NULL
        }

        # format for nimble
        COUNT <- survey_for_nimble(data,
                                   count.covs,
                                   type = "count",
                                   rename = counter,
                                   keep.conus.grid.id = keep.conus.grid.id)

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
