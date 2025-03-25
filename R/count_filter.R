#' Set up count data
#' 
#' @description Sets up count data for iSDM -- summarizes covariates. This function is wrapped within sppdata_for_nimble().
#'
#' @param data (data.frame) dataframe from species.data$obs containing observations from one dataset
#' @param covs.mean (character vector) vector of column names to use as detection covariates that should be averaged across passes (e.g., water temperature)
#' @param covs.sum (character vector) vector of column names to use as detection covariates that should be summed across passes (e.g., survey duration)
#' @param offset.area (character) name of column to use for area offset 
#' @param age.use (character vector) vector of age categories to use; defaults to c("adult", "metamorph", "juvenile", "larva", "egg mass", "NR", NA)
#' @param req.cols (character vector) which columns are required in the output dataframe; defaults to c("site.id", "lat", "lon", "day", "month", "year", "survey.conducted", "survey.id", "data.type", "species", "time.to.detect", "count")
#'
#' @returns A dataframe containing clean and summarized observation data from one dataset.
#' @export
#'
#' @examples



count_filter <- function(data,
                         covs.mean,
                         covs.sum,
                         offset.area,
                         age.use = c("adult", "metamorph", "juvenile", "larva", "egg mass", "NR", NA),
                         req.cols = c("site.id", "lat", "lon",
                                      "day", "month", "year", "survey.conducted", "survey.id", "data.type",
                                      "species", "time.to.detect", "count")) {

    cat("\nLoading count")

    countstart <- data %>%
      filter(age %in% age.use)

    if (nrow(countstart) > 0) {
      # get total count
      countcount <- countstart %>%
        select(!any_of(c(covs.mean, covs.sum))) %>%
        group_by(survey.id) %>%
        summarize(count = sum(count), .groups = "drop")

      # get mean covariates (e.g., temperature)
      countcovs.mean <- countstart %>%
        select(survey.id, any_of(covs.mean), any_of(offset.area)) %>%
        distinct() %>%
        group_by(survey.id) %>%
        summarize_all(mean, na.rm = T)

      # get sum covariates (e.g., duration)
      countcovs.sum <- countstart %>%
        select(survey.id, any_of(covs.sum)) %>%
        distinct() %>%
        group_by(survey.id) %>%
        summarize_all(sum, na.rm = T)

      # get other important columns
      countcols <- countstart %>%
        select(conus.grid.id, site.id, survey.id, lat, lon, day, month, year, survey.conducted) %>%
        distinct()

      # put them together
      countstart <- inner_join(countcols, countcount, by = "survey.id")
      
      if (length(c(covs.mean, offset.area)) > 0) {
        countstart <- dplyr::full_join(countstart, countcovs.mean, by = "survey.id")
      }
      if (length(covs.sum) > 0) {
        countstart <- dplyr::full_join(countstart, countcovs.sum, by = "survey.id")
      }
      
      if (offset.area %in% colnames(countstart)) {
        colnames(countstart)[grep(offset.area, colnames(countstart))] <- "area"
      }
      
    } else {
      print("No data of appropriate species, year, and age in this dataset")
      countstart <- countstart
    }


  return(countstart)
}
