#' Loads species data
#'
#' @description Loads species data and does initial cleaning necessary for iSDM
#'
#' @param sp.code (character) 4-letter species code
#' @param sp.code.all (character) string of all species codes to pull for use in analysis, separated by |
#' @param file.info (data.frame) columns 'file.name', 'file.label', 'covar.mean', and 'covar.sum' describing the datasets to be read in for this species
#' @param file.path (character) path where data files are located
#' @param region (sf list) region (output from make_region())
#' @param filter.region (logical) Only keep data within region (T) or not (F)
#' @param year.start (numeric) keep data during or after this year
#' @param year.end (numeric) keep data during or before this year; defaults to current year
#' @param coordunc (numeric) if data has coord.unc column (e.g., GBIF data), remove records with coord.unc > coordunc
#' @param coordunc_na.rm (logical) if data has coord.unc column (e.g., GBIF data), remove records with coord.unc == NA (T) or not (F)
#' @param spat.thin (logical) if data is PO, do spatial thinning at 2km x 2km grid cell (T) or not (F)
#' @param keep.conus.grid.id (character vector) Which grid cells to keep (e.g., for cross validation); defaults to all grid cells in the region
#'
#' @returns A list with two objects. The first object (locs) is a dataframe containing the coordinate locations of all observations. The second object (obs) is a list containing a dataframe for each dataset and the conus.grid.id location of each observation.
#' @export
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @importFrom utils read.csv
#' @importFrom tidyselect any_of
#' @importFrom sf st_transform st_join st_as_sf st_make_grid st_coordinates st_drop_geometry st_crs st_within
#' @importFrom dplyr mutate filter select group_by slice_sample ungroup
#'
#' @examples
#'\dontrun{
#'
#' species.data <- load_species_data(sp.code = "ANMI",
#'                 sp.code.all = "ANMI",
#'                 file.name = c("name-of-file.csv"),
#'                 file.label = c("File Label"),
#'                 file.path = "DATA SWAMP/data-ready/",
#'                 keep.cols = list(c("duration")),
#'                 region = region,
#'                 filter.region = T,
#'                 year.start = 1994,
#'                 year.end = 2004,
#'                 coordunc = 1000,
#'                 coordunc_na.rm = T,
#'                 spat.thin = F,
#'                 keep.conus.grid.id = region$sp.grid$conus.grid.id
#'
#'}


load_species_data <- function(sp.code,
                              sp.code.all,
                              file.info,
                              file.path,
                              region,
                              filter.region,
                              year.start,
                              year.end = as.numeric(format(Sys.Date(), "%Y")),
                              coordunc = 1000,
                              coordunc_na.rm = T,
                              spat.thin = F,
                              keep.conus.grid.id = region$sp.grid$conus.grid.id) {

  
  if (year.end < year.start) stop("'year.end' must be after 'year.start'")
  if (length(sp.code.all) > 1) sp.code.all <- paste0(sp.code.all, collapse = "|")


  # Pull info from file.info
  file.name <- file.info$file.name
  file.label <- file.info$file.label

  covariates <- list()
  for (i in 1:nrow(file.info)) {
    if (is.na(file.info$covar.mean[i])) file.info$covar.mean[i] <- ""
    if (is.na(file.info$covar.sum[i])) file.info$covar.sum[i] <- ""
    
    covs.mean <- unlist(strsplit(file.info$covar.mean[i], split = ", "))
    covs.sum <- unlist(strsplit(file.info$covar.sum[i], split = ", "))
    covs1 <- c(covs.mean, covs.sum)
    covs1 <- covs1[which(is.na(covs1) == F)]
    covariates[[file.info$file.name[i]]] <- covs1
  }


  # Get locations
  locs.cont <- c()
  locs.disc <- c()

  # Get observations
  obs <- list()

  # Loop through all files
  for(f in 1:length(file.name)) {

    cat(paste0("\nFile ", f, ": ", file.label[f], "....Loading file\n"))

    # read csv file
    file <- read.csv(paste0(file.path, file.name[f], ".csv"))


    file$site.id <- as.character(file$site.id)


    # make unique ID for each row
    file$unique.id <- paste0(sp.code, "-", f, "-", 1:nrow(file))

    # create source column that contains dataset name
    file <- mutate(file, source = file.label[f])

    # filter to get the correct species
    tmp1 <- gsub("[|]", "$|^", sp.code.all)
    tmp2 <- paste0("^", tmp1, "$")
    file <- file[grep(tmp2, file$species),]
    cat("Starting with", nrow(file), "observations of", sp.code, "\n")
    

    # filter to get only survey.conducted == 1
    file <- filter(file, .data$survey.conducted == 1)

    # filter to get only locations in the region
    if (filter.region == T) {

      # filter by region
      file1 <- st_as_sf(file,
                        coords = c("lon", "lat"),
                        crs = 4326) %>%
        st_transform(st_crs(region$region)) %>%
        st_join(region$sp.grid, join = st_within) %>%
        filter(is.na(.data$conus.grid.id) == F)

      tmp <- nrow(file) - nrow(file1)
      if (tmp > 0) cat("Removing", tmp, "observations that are outside of the study region\n")

      file <- filter(file, .data$site.id %in% file1$site.id)
    }


    if(nrow(file) == 0) {
      cat("No data for this analysis\n")
      next
    }

    # filter out records by year
    rm <- filter(file, .data$year < year.start | .data$year > year.end)
    if (nrow(rm) > 0) cat(paste0("Removing ", nrow(rm), " observations before ", year.start, " or after ", year.end, "\n"))
    file <- filter(file, .data$year >= year.start & .data$year <= year.end)

    if(nrow(file) == 0) {
      cat("No data for this analysis\n")
      next
    }

    # filter location uncertainty
    if ("coord.unc" %in% colnames(file) & coordunc != Inf) {

      file <- mutate(file, rm = case_when(coord.unc <= coordunc ~ "keep",
                                          is.na(coord.unc) ~ "keep",
                                          T ~ "remove"))

      tmp <- nrow(filter(file, rm == "remove"))
      cat("Removing", tmp, "observations whose coordinate uncertainty is above the threshold of", coordunc, "\n")

      file <- file %>%
        filter(.data$rm == "keep") %>%
        select(!"rm")
    }

    if(nrow(file) == 0) {
      cat("No data for this analysis\n")
      next
    }

    if ("coord.unc" %in% colnames(file) & coordunc_na.rm == T) {

      tmp <- length(which(is.na(file$coord.unc) == T))
      cat("Removing", tmp, "observations where coordinate uncertainty is NA\n")

      file <- filter(file, is.na(.data$coord.unc) == F)
    }

    if(nrow(file) == 0) {
      cat("No data for this analysis\n")
      next
    }

    # spatial thinning for PO
    if (spat.thin == T &
        file$data.type[1] == "PO") {
      #file.label[f] %in% c("iNaturalist", "iNaturalist (obscured)")) {

      # make grid
      grid <- st_make_grid(region$region, cellsize = c(2000, 2000)) %>% st_as_sf()
      grid <- mutate(grid, grid.id = 1:nrow(grid))


      file1 <- file %>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        st_transform(crs = st_crs(grid)) %>%
        st_join(grid, join = st_within) %>%
        group_by(.data$grid.id) %>%
        slice_sample(n = 1) %>% # select one from each grid cell
        ungroup() %>%

        # now put it back in the original format
        st_transform(crs = 4326)

      tmp <- as.data.frame(st_coordinates(file1))
      file1 <- file1 %>%
        mutate(lon = tmp$X,
               lat = tmp$Y) %>%
        st_drop_geometry()

      tmp <- nrow(file) - nrow(file1)

      cat("Removing", tmp, "observations for spatial thinning\n")

      file <- file1
    }


    if(nrow(file) == 0) {
      cat("No data for this analysis\n")
      next
    }


    # put into correct crs
    locs <- file %>%
      st_as_sf(coords = c("lon", "lat"),
                   crs = 4326,
                   remove = F) %>%

      st_transform(st_crs(region$region))


    # locs for continuous space
    locs.c <- select(locs, "unique.id", "site.id", "survey.id",
                     "pass.id", "survey.conducted", "lat", "lon",
                     "day", "month", "year", "geometry",
                     "data.type", "source")


    # locs for discrete space
    locs.d <- locs.c %>%
      st_join(region$sp.grid, join = st_within) %>%
      st_drop_geometry() %>%
      as.data.frame() %>%
      select("unique.id", "site.id", "survey.id", "pass.id",
             "conus.grid.id", "year", "source", "data.type")

    # remove rows that are not in keep.conus.grid.id
    #if (keep.conus.grid.id[1] != "all") {
    rm <- filter(locs.d, .data$conus.grid.id %in% keep.conus.grid.id == F)
    if (nrow(rm) > 0) {
      cat("Removing ", nrow(rm), " observations that are not in the correct block(s)\n")

      locs.d <- filter(locs.d, .data$conus.grid.id %in% keep.conus.grid.id)
      locs.c <- filter(locs.c, .data$unique.id %in% locs.d$unique.id)
    }

    if(nrow(locs.d) == 0) {
      cat("No data for this analysis\n")
      next
    }
    #}

    # remove dataset if there is only one unique survey.id
    survs <- length(unique(locs.d$survey.id))
    if (survs < 2) {
      cat("Removing ", file.label[f], " dataset because there is only one remaining survey.id\n")
      next
    }

    # iNaturalist records for subspecies names might be duplicates, remove here
    if (file$source[1] == "iNaturalist") {
      new <- paste0(locs.c$lat, locs.c$lon, locs.c$day, locs.c$month, locs.c$year)

      old <- locs.cont[which(locs.cont$source == "iNaturalist"),]
      old <- paste0(old$lat, old$lon, old$day, old$month, old$year)

      if (length(old) > 0) {
        rm <- which(new %in% old)

        locs.c <- locs.c[-rm,]
        locs.d <- locs.d[-rm,]

        if (nrow(locs.c) == 0) {
          cat("All iNaturalist records are potential duplicates\n")
          next
        }
      }

    }


    locs.cont <- bind_rows(locs.cont, locs.c)
    locs.disc <- bind_rows(locs.disc, locs.d)


    # observations
    # add grid.id from locs.d and save
    keepcols <- covariates[[f]]
    keepcols1 <- keepcols[which(keepcols %in% colnames(file))]
    missingcovs <- setdiff(keepcols, keepcols1)
    if (length(keepcols1) > 0) cat("Using ", keepcols1, " as covariate(s)\n")
    file1 <- inner_join(file,
                        select(locs.d, "unique.id", "conus.grid.id"),
                        by = "unique.id") %>%
              select("source", "data.type", "site.id", "lat",
                     "lon", "conus.grid.id", "unique.id",
                     "survey.id", "pass.id", "day", "month",
                     "year", "survey.conducted", "species", "age",
                     "time.to.detect", "individual.id", "count",
                     any_of(keepcols1))
    # cat(missingcovs, length(missingcovs), "\n")
    # cat(keepcols, length(keepcols), "\n")
    # cat(keepcols1, length(keepcols1), "\n")
    if (length(missingcovs) > 0)  warning(missingcovs, " column(s) missing from file")

    # If label already exists, just append dfs
    # This is useful for:
    #     - multiple iNat files for subspecies
    #     - multiple files for the same dataset during different years
    if (file.label[f] %in% names(obs)) {
      obs[[file.label[f]]] <- bind_rows(obs[[file.label[f]]], file1)
    } else {
      obs[[file.label[f]]] <- file1
    }

  } # end read in files

  # save locs
  locs <- list(disc = locs.disc, cont = locs.cont)

  cat("\n")

  dat <- list(locs = locs, obs = obs)

  return(dat)

}
