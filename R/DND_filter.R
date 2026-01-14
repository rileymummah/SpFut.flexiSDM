#' Set up DND data
#'
#' @description Sets up DND data for iSDM -- deals with "maybe" detections, summarizes covariates. This function is wrapped within sppdata_for_nimble().
#'
#' @param data (data.frame) dataframe from species.data$obs containing observations from one dataset
#' @param covs.mean (character vector) vector of column names to use as detection covariates that should be averaged across passes (e.g., water temperature)
#' @param covs.sum (character vector) vector of column names to use as detection covariates that should be summed across passes (e.g., survey duration)
#' @param DND.maybe (numeric) what to categorize "maybe" detections as (1 = detection; 0 = nondetection)
#' @param offset.area (character) name of column to use for area offset
#' @param age.use (character vector) vector of age categories to use; defaults to c("adult", "metamorph", "juvenile", "egg mass", "NR", NA)
#'
#' @returns A dataframe containing clean and summarized observation data from one dataset.
#'
#' @importFrom rlang .data
#' @importFrom tidyselect any_of
#' @importFrom magrittr "%>%"
#' @importFrom dplyr filter select mutate case_when group_by summarize distinct summarize_all rename full_join
#' @importFrom hablar sum_


DND_filter <- function(data,
                       covs.mean,
                       covs.sum,
                       DND.maybe,
                       offset.area,
                       age.use = c("adult", "metamorph", "juvenile", "egg mass", "NR", NA)) {



  cat("\nLoading DND")
  
  dndstart <- filter(data, .data$age %in% age.use)

  if (nrow(dndstart) > 0) {

    # get max count (0 or 1)
    dndcount <- dndstart %>%
      select("survey.id", "pass.id", "age", "count") %>%
      mutate(count = case_when(count == 0 ~ 0,
                               count == 1 ~ 1,
                               count == 2 ~ DND.maybe)) %>%

      # aggregate across passes
      group_by(.data$survey.id) %>%
      summarize(count = max(.data$count), .groups = "drop")


    # get area if it exists
    if (length(offset.area) > 0) {
      dnd.area <- dndstart %>%
        select("survey.id", "pass.id", any_of(offset.area)) %>%
        distinct() %>%
        select(!"pass.id") %>%

        # aggregate across passes
        group_by(.data$survey.id) %>%
        summarize_all(mean, na.rm = T)
    }


    # get mean covariates (temperature, moon visibility, etc.)
    if (length(covs.mean) > 0) {
      dndcovs.mean <- dndstart %>%
        select("survey.id", "pass.id", any_of(covs.mean)) %>%
        distinct() %>%
        select(!"pass.id") %>%

        # aggregate across passes
        group_by(.data$survey.id) %>%
        summarize_all(mean, na.rm = T)
    }

    # get sum covariates (duration, water volume)
    if (length(covs.sum) > 0) {
      dndcovs.sum <- dndstart %>%
        select("survey.id", "pass.id", any_of(covs.sum)) %>%
        distinct() %>%
        
        # aggregate across passes
        group_by(.data$survey.id) %>%
        summarize_all(hablar::sum_, ignore_na = T) %>%
        select(!"pass.id")
      
      dndcovs.npass <- dndstart %>%
        select("survey.id", "pass.id", any_of(covs.sum)) %>%
        distinct() %>%
        
        # aggregate across passes
        group_by(.data$survey.id) %>%
        summarize(npass = length(unique(pass.id))) 
      
      dndcovs.sum <- full_join(dndcovs.sum, dndcovs.npass, by = "survey.id")
    }


    # get other important columns
    dndcols <- dndstart %>%
      select("conus.grid.id", "site.id", "survey.id", "lat",
             "lon", "day", "month", "year",
             "survey.conducted") %>%
      distinct()

    # put them together
    dndstart <- inner_join(dndcols, dndcount, by = "survey.id")

    if (length(covs.mean) > 0) {
      dndstart <- full_join(dndstart, dndcovs.mean, by = "survey.id")
    }
    if (length(covs.sum) > 0) {
      dndstart <- full_join(dndstart, dndcovs.sum, by = "survey.id")
    }

    if (length(offset.area) > 0) {
      colnames(dnd.area)[grep(offset.area, colnames(dnd.area))] <- "area"

      dndstart <- full_join(dndstart, dnd.area, by = "survey.id")

    }


  } else {
    cat("\nNo data of appropriate species, year, and age in this dataset")
    dndstart <- dndstart
  }

  return(dndstart)
}
