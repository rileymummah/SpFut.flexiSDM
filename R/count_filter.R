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
#'
#' @importFrom tidyselect any_of
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter select group_by summarize distinct summarize_all full_join


count_filter <- function(data,
                         covs.mean,
                         covs.sum,
                         offset.area,
                         age.use = c("adult", "metamorph", "juvenile", "larva", "egg mass", "NR", NA),
                         req.cols = c("site.id", "lat", "lon",
                                      "day", "month", "year", "survey.conducted", "survey.id", "data.type",
                                      "species", "time.to.detect", "count")) {

    cat("\nLoading count")

    countstart <- filter(data, .data$age %in% age.use)

    if (nrow(countstart) > 0) {
      # get total count
      countcount <- countstart %>%
                      select(!any_of(c(covs.mean, covs.sum))) %>%
                      group_by(.data$survey.id) %>%
                      summarize(count = sum(.data$count), .groups = "drop")

      # get area if it exists
      if (length(offset.area) > 0) {
        count.area <- countstart %>%
                        select(.data$survey.id, .data$pass.id, any_of(offset.area)) %>%
                        distinct() %>%
                        select(!.data$pass.id) %>%

                        # aggregate across passes
                        group_by(.data$survey.id) %>%
                        summarize_all(mean, na.rm = T)
      }

      # get mean covariates (e.g., temperature)
      if (length(covs.mean) > 0) {
        countcovs.mean <- countstart %>%
          select(.data$survey.id, any_of(covs.mean)) %>%
          distinct() %>%
          group_by(.data$survey.id) %>%
          summarize_all(mean, na.rm = T)
      }


      # get sum covariates (e.g., duration)
      if (length(covs.sum) > 0) {
        countcovs.sum <- countstart %>%
                          select(.data$survey.id, any_of(covs.sum)) %>%
                          distinct() %>%
                          group_by(.data$survey.id) %>%
                          summarize_all(sum, na.rm = T)

        # if all values are NA, it becomes 0. Make it NA again.
        countcovs.sum[countcovs.sum == 0] <- NA
      }


      # get other important columns
      countcols <- countstart %>%
                    select(.data$conus.grid.id, .data$site.id, .data$survey.id,
                           .data$lat, .data$lon, .data$day, .data$month,
                           .data$year, .data$survey.conducted) %>%
                    distinct()

      # put them together
      countstart <- inner_join(countcols, countcount, by = "survey.id")

      if (length(covs.mean) > 0) {
        countstart <- full_join(countstart, countcovs.mean, by = "survey.id")
      }
      if (length(covs.sum) > 0) {
        countstart <- full_join(countstart, countcovs.sum, by = "survey.id")
      }

      if (length(offset.area) > 0) {
        colnames(count.area)[grep(offset.area, colnames(count.area))] <- "area"

        countstart <- full_join(countstart, count.area, by = "survey.id")
      }

    } else {
      print("No data of appropriate species, year, and age in this dataset")
      countstart <- countstart
    }


  return(countstart)
}
