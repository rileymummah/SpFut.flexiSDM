#' Get species ranges for reference
#'
#' @description
#'
#' @param range.path A vector of paths leading to species ranges
#' @param range.name A vector of names for each range
#' @param crs EPSG code for desired crs of output
#'
#' @returns A list containing ranges as sf objects
#' @export
#'
#' @importFrom tidyselect any_of
#'
#' @examples


DND_filter <- function(data,
                       covs.mean,
                       covs.sum,
                       sp.code,
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
