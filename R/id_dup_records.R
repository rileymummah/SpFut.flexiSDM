#' Find duplicate records
#'
#' @description
#' Find PO records that might be duplicated in iNaturalist and in another dataset
#'
#' @param species.data (list) output from load_species_data()
#'
#' @returns (vector) unique IDs of records that may be duplicates
#' @export
#'
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select distinct filter pull bind_rows
#' @importFrom sf st_drop_geometry st_buffer st_intersection


id_dup_records <- function(species.data) {


  POsources <- species.data$locs$cont %>%
                st_drop_geometry() %>%
                select(.data$source, .data$data.type) %>%
                distinct() %>%
                filter(.data$data.type == "PO") %>%
                pull(.data$source)

  inat.ind <- grep("iNat", POsources)

  # get locations of PO data
  locs <- bind_rows(species.data$locs$cont) %>%
            filter(.data$source %in% POsources) %>%
            st_buffer(dist = 500)

  # get dates of PO data
  dates <- c()
  for (s in 1:length(species.data$obs)) {
    if (names(species.data$obs)[s] %in% POsources) {
      dat <- species.data$obs[[s]] %>%
        select(.data$site.id, .data$day, .data$month, .data$year, .data$lat,
               .data$lon, .data$survey.id, .data$survey.conducted, .data$count,
               .data$conus.grid.id, .data$source, .data$unique.id) %>%
        mutate(site.survey.id = paste0(.data$site.id, "-", .data$survey.id))

      dates <- bind_rows(dates, dat)
    }
  }

  # merge locs and dates
  locs <- full_join(locs, dates,
                    by = c("unique.id", "survey.id", "site.id", "lat", "lon",
                           "source", "year", "survey.conducted"))

  # find all points that overlap

  # split into data source and all other data sources
  tmp1 <- filter(locs, .data$source == POsources[inat.ind])
  tmp2 <- filter(locs, .data$source != POsources[inat.ind])

  # find points that overlap
  inter <- st_intersection(tmp1, tmp2)

  # find points that overlap on the same date
  inter1 <- filter(inter,
                   .data$day.x == .data$day.y.1,
                   .data$month.x == .data$month.y.1,
                   .data$year == .data$year.1) %>%

              # pull site.survey.id for iNat record
              pull(.data$unique.id) %>%
              unique()

  return(inter1)
}
