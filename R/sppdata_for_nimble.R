#' Format species data for nimble
#'
#' @description
#'
#' @param 
#'
#' @returns 
#' @export
#'
#' @examples
#' \dontrun{
#' range <- get_range('ANMI')
#' }


## This function still needs work for iNat data, state PO data, and count data

sppdata_for_nimble <- function(sp.code, # not necessary?
                               species.data,
                               region,
                               data.type,
                               PO.extent,
                               covar,
                               covs.inat = c("traveltime", "density", "n.inat"),
                               covs.PO = c("traveltime", "density"),
                               covs.mean,
                               covs.sum,
                               DND.maybe = 1,
                               keep.conus.grid.id,
                               min.visits.incl = 3) {


  # these are indices of datasets that don't have any data (i.e., wrong years, species, etc.)
  # they will be removed at the end
  rm.ind <- c()

  # this is where the datasets will go once they're set up for nimble
  dat.ready <- list()

  # # read in names of data files that have sp.code in them
  # allfiles <- read.csv("DATA SWAMP/dataset-summary-full.csv") %>%
  #   filter(species %in% sp.code)
  # 
  # # get info for data files that have sp.code in them
  # summary.sp <- read.csv("DATA SWAMP/00-data-summary.csv") %>%
  #   filter(Use == 1,
  #          Data.Swamp.file.name %in% allfiles$file,
  #          Name %in% names(species.data$obs))

  # types.use <- summary.sp$Type.true
  # if (CMR.to.count == T) {
  #   types.use[grep("CMR", summary.sp$Type.true)] <- "Count"
  # }
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

      cat(paste0("No iNaturalist data for ", sp.code, "\n"))

    } else if (length(inat.ind) == 1) {
      # If there is one labeled iNat, process

      cat("Looking for duplicates in PO data\n")
      rm <- id_dup_records(sp.code, species.data)

      if (length(rm) > 0) {
        cat(paste0("Removing ", length(rm), " records from iNaturalist that may be duplicated in other datasets\n"))
      }

      # Filter PO data
      data <- species.data$obs[[inat.ind]]
      name <- file.names[inat.ind]
      #name <- data$source[1]

      # inat.start <- PO_filter(data,
      #                         sp.code)
      inat.start <- data

      # Aggregate iNat to grid cells
      POdata <- inat.start %>%
        #rename(grid.id = "sp.grid.id") %>%

        # remove records that were identified as duplicates
        mutate(site.survey.id = paste0(site.id, "-", survey.id)) %>%
        filter((unique.id %in% rm) == F) %>%
        select(!site.survey.id) %>%

        # aggregate to grid cell
        group_by(conus.grid.id) %>%
        summarize(count = n()) %>%

        # add in the rest of grid.ids and fill 0s if there were no records (NA)
        full_join(select(covar, conus.grid.id), by = "conus.grid.id") %>%
        mutate(count = case_when(is.na(count) ~ 0,
                                 T ~ count)) %>%
        filter(is.na(conus.grid.id) == F,
               conus.grid.id %in% keep.conus.grid.id) # get rid of cells that we're excluding

      # Get the covariates to use for iNat
      #covariates <- get_covs.PO(covs.inat, covar)
      covariates <- select(covar, all_of(covs.inat))

      PO.inat <- PO_for_nimble(POdata, covariates,
                               rename = counter)
      PO.inat$constants[[paste0("name", counter)]] <- name

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
          #name <- data$source[1]
          # other.start <- PO_filter(data,
          #                          sp.code)
          other.start <- data
          cat("\nLoading PO")
          
          # Aggregate records to grid.ids
          POdata <- other.start %>%
            #rename(grid.id = "sp.grid.id") %>%

            # aggregate to grid cell
            group_by(conus.grid.id) %>%
            summarize(count = n()) %>%

            # add in the rest of grid.ids and fill 0s if there were no records (NA)
            full_join(select(covar, conus.grid.id), by = "conus.grid.id") %>%
            mutate(count = case_when(is.na(count) ~ 0,
                                     T ~ count)) %>%
            filter(is.na(conus.grid.id) == F,
                   conus.grid.id %in% keep.conus.grid.id) # get rid of cells that we're excluding

          #covariates <- get_covs.PO(covs.PO, covar)
          covariates <- select(covar, conus.grid.id, all_of(covs.PO))
          
          # make sure everything is in the right order
          POdata <- POdata[order(match(POdata$conus.grid.id, keep.conus.grid.id)),]
          covariates <- covariates[order(match(covariates$conus.grid.id, keep.conus.grid.id)),]

          PO.other <- PO_for_nimble(POdata, covariates,
                                    rename = counter)
          PO.other$constants[[paste0("name", counter)]] <- name

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

        # Filter dataset
        # other.start <- PO_filter(sta.dat,
        #                          sp.code = sp.code)
        other.start <- data
        
        # Aggregate to grid.ids
        POdata <- other.start %>%
          #rename(grid.id = "sp.grid.id") %>%

          # aggregate to grid cell
          group_by(conus.grid.id) %>%
          summarize(count = n(), .groups = "drop") %>%

          # add in the rest of grid.ids and fill 0s if there were no records (NA)
          full_join(select(covar, conus.grid.id), by = "conus.grid.id") %>%
          mutate(count = case_when(is.na(count) ~ 0,
                                   T ~ count)) %>%
          filter(is.na(conus.grid.id) == F,
                 conus.grid.id %in% keep.conus.grid.id) # get rid of cells that we're excluding

        # Get covariates
        states <- unique(other.start$state)
        if (length(states) >= 2) {
          all.states <- read_rds("data/USA/grid-states.rds") %>%
            filter(conus.grid.id %in% region$sp.grid$conus.grid.id)

          # make sure these cells are in the same order as the cells in data and constants
          all.states <- all.states[order(match(all.states$conus.grid.id, covar$conus.grid.id)),]

          # leave out first state because this is for state intercept
          statecols <- states[2:length(states)]
          stateints <- all.states[,c("conus.grid.id", statecols)]
          covariates <- covar %>%
            inner_join(stateints, by = "conus.grid.id") %>%
            select(conus.grid.id, all_of(covs.PO), all_of(statecols))

          # use all states because this is for S (whether cell is in state with PO data or not)
          covariates1 <- covar %>%
            inner_join(all.states, by = "conus.grid.id") %>%
            select(conus.grid.id, all_of(covs.PO), all_of(states))
        } else if (length(states) == 1) {
          grid.state <- read_rds("data/USA/grid-states.rds")

          # don't need intercept since it's just one state
          covariates <- covar %>%
            select(conus.grid.id, all_of(covs.PO))

          # use states for S (whether cell is in state with PO data or not)
          covariates1 <- covar %>%
            inner_join(grid.state, by = "conus.grid.id") %>%
            select(conus.grid.id, all_of(covs.PO), all_of(states))
        } else {
          covariates <- covar %>%
            select(conus.grid.id, all_of(covs.PO))
        }

        # make sure everything is in the right order
        POdata <- POdata[order(match(POdata$conus.grid.id, keep.conus.grid.id)),]
        covariates <- covariates[order(match(covariates$conus.grid.id, keep.conus.grid.id)),]
        
        PO.other <- PO_for_nimble(POdata, covariates, rename = counter)
        PO.other$constants[[paste0("name", counter)]] <- name
        PO.other$constants[[paste0("states", counter)]] <- T

        if (length(states) > 1) PO.other$data[[paste0("S", counter)]] <- apply(covariates1[,states], 1, max) # use all states
        if (length(states) == 1) PO.other$data[[paste0("S", counter)]] <- covariates1[,states] # use one state


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
      covs.mean <- dnd.covs.mean[which(is.na(dnd.covs.mean) == F)]

      dnd.covs.sum <- unlist(strsplit(covs.sum[DNDind[d]], split = ", "))
      covs.sum <- dnd.covs.sum[which(is.na(dnd.covs.sum) == F)]

      cov.names <- c(covs.mean, covs.sum)

      data <- species.data$obs[[DNDind[d]]]
      name <- file.names[DNDind[d]]
      #name <- data$source[1]

      # Add yday column
      if ("yday" %in% cov.names) {
        tmp <- as.Date(paste0(data$month, "-", data$day, "-", data$year), format = "%m-%d-%Y")
        data$yday <- lubridate::yday(tmp)
      }


      # filter and aggregate DND observations
      dndstart <- DND_filter(data,
                             covs.mean,
                             covs.sum,
                             DND.maybe = 1,
                             sp.code = sp.code)


      # remove if only one data point
      if(nrow(dndstart) < 2) {
        cat("Only one data point, removing dataset\n")
        rm.ind <- c(rm.ind, DNDind[d])
        dat.ready[[paste0("DND", length(dat.ready)+1)]] <- NA

      } else {

        # clean up columns
        data <- dndstart %>%
          select(conus.grid.id, site.id, survey.id, any_of(cov.names), count) %>%
          group_by(site.id) %>%
          mutate(site.id = cur_group_id()) %>%
          ungroup() %>%
          mutate(across(any_of(cov.names), scale_this))

        if ("yday" %in% cov.names) {
          data$yday2 <- data$yday * data$yday
          cov.names <- c(cov.names, "yday2")
        }

        # format for nimble
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
      count.covs.mean <- unlist(strsplit(summary.sp$Covar.mean[countind[d]], split = ", "))
      covs.mean <- count.covs.mean[which(is.na(count.covs.mean) == F)]

      count.covs.sum <- unlist(strsplit(summary.sp$Covar.sum[countind[d]], split = ", "))
      covs.sum <- count.covs.sum[which(is.na(count.covs.sum) == F)]

      count.covs <- c(covs.mean, covs.sum)

      data <- species.data$obs[[countind[d]]]
      name <- data$source[1]

      # Add yday
      if ("yday" %in% count.covs) {
        tmp <- as.Date(paste0(data$month, "-", data$day, "-", data$year), format = "%m-%d-%Y")
        data$yday <- lubridate::yday(tmp)
      }


      # Filter data
      count.start <- count_filter(data,
                                  covs.mean,
                                  covs.sum,
                                  types.true = summary.sp$Type.true[countind[d]],
                                  sp.code)

      # remove if 0 or 1 data point
      if(nrow(count.start) < 2) {
        cat("Only one data point, removing dataset\n")
        rm.ind <- c(rm.ind, countind[d])
        dat.ready[[paste0("count", counter)]] <- NA

      } else {

        # clean up columns
        data <- count.start %>%
          #rename(grid.id = "sp.grid.id") %>%
          select(conus.grid.id, site.id, survey.id, any_of(count.covs), count) %>%
          group_by(site.id) %>%
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


        # format for nimble
        COUNT <- survey_for_nimble(data,
                                   count.covs,
                                   rename = counter)

        # if this is going to have a logit link (ie nVisits >= min.visits.incl)
        # it needs an intercept
        if (COUNT$constants[[paste0("nVisitsY", counter)]] >= min.visits.incl) {
          COUNT$data[[paste0("Xy", counter)]] <- COUNT$data[[paste0("Xy", counter)]] %>%
            add_column(.before = 1, intercept = 1)
          COUNT$constants[[paste0("nCovY", counter)]] <- COUNT$constants[[paste0("nCovY", counter)]] + 1
        }

        COUNT$constants[[paste0("name", counter)]] <- name


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
