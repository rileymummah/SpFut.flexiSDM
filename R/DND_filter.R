#' Set up DND data
#'
#' @description Sets up DND data for iSDM -- deals with "maybe" detections, summarizes covariates. This function is wrapped within sppdata_for_nimble().
#'
#' @param data (data.frame) dataframe from species.data$obs containing observations from one dataset
#' @param covs.mean (character vector) vector of column names to use as detection covariates that should be averaged across passes (e.g., water temperature)
#' @param covs.sum (character vector) vector of column names to use as detection covariates that should be summed across passes (e.g., survey duration)
#' @param DND.maybe (numeric) what to categorize "maybe" detections as (1 = detection; 0 = nondetection)
#' @param age.use (character vector) vector of age categories to use; defaults to c("adult", "metamorph", "juvenile", "egg mass", "NR", NA)
#' @param req.cols (character vector) which columns are required in the output dataframe; defaults to c("unique.id", "site.id", "lat", "lon", "day", "month", "year", "survey.conducted", "survey.id", "data.type", "species", "time.to.detect", "count")
#'
#' @returns A dataframe containing clean and summarized observation data from one dataset.
#' 
#'
#' @importFrom tidyselect any_of
#'
#' @examples


DND_filter <- function(data,
                       covs.mean,
                       covs.sum,
                       DND.maybe,
                       age.use = c("adult", "metamorph", "juvenile", "egg mass", "NR", NA),
                       req.cols = c("unique.id", "site.id", "lat", "lon",
                                    "day", "month", "year", "survey.conducted", "survey.id", "data.type",
                                    "species", "time.to.detect", "count")) {



  cat("\nLoading DND")

  dndstart <- dplyr::filter(data, age %in% age.use)

  if (nrow(dndstart) > 0) {

    # get max count (0 or 1)
    all.covs <- c(covs.mean, covs.sum)

    dndcount <- dndstart %>%
      dplyr::select(survey.id, pass.id, age, count) %>%
      dplyr::mutate(count = dplyr::case_when(count == 0 ~ 0,
                                             count == 1 ~ 1,
                                             count == 2 ~ DND.maybe)) %>%

      # aggregate across passes
      dplyr::group_by(survey.id) %>%
      dplyr::summarize(count = max(count), .groups = "drop")

    # get mean covariates (temperature, moon visibility, etc.)
    if (length(covs.mean) > 0) {
      dndcovs.mean <- dndstart %>%
        dplyr::select(survey.id, pass.id, tidyselect::any_of(covs.mean)) %>%
        dplyr::distinct() %>%
        dplyr::select(!pass.id) %>%

        # aggregate across passes
        dplyr::group_by(survey.id) %>%
        dplyr::summarize_all(mean, na.rm = T)
    }

    # get sum covariates (duration, water volume)
    if (length(covs.sum) > 0) {
      dndcovs.sum <- dndstart %>%
        dplyr::select(survey.id, pass.id, tidyselect::any_of(covs.sum)) %>%
        dplyr::distinct() %>%

        # aggregate across passes
        dplyr::group_by(survey.id) %>%
        dplyr::summarize_all(sum, na.rm = T) %>%
        dplyr::rename(npass = pass.id)
    }


    # get other important columns
    dndcols <- dndstart %>%
      dplyr::select(conus.grid.id, site.id, survey.id, lat, lon, day, month, year, survey.conducted) %>%
      dplyr::distinct()

    # put them together
    dndstart <- dplyr::inner_join(dndcols, dndcount, by = "survey.id")

    if (length(covs.mean) > 0) {
      dndstart <- dplyr::full_join(dndstart, dndcovs.mean, by = "survey.id")
    }
    if (length(covs.sum) > 0) {
      dndstart <- dplyr::full_join(dndstart, dndcovs.sum, by = "survey.id")
    }


  } else {
    cat("No data of appropriate species, year, and age in this dataset")
    dndstart <- dndstart
  }

  return(dndstart)
}
