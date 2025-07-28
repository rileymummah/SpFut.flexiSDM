#' Find duplicate records
#'
#' @description
#' Find PO records that might be duplicated in iNaturalist and in another dataset
#'
#'
#' @param species.data (list) output from load_species_data()
#'
#' @returns (vector) unique IDs of records that may be duplicates
#' @export
#'
#' @examples



id_dup_records <- function(species.data) {


  POsources <- species.data$locs$cont %>%
                sf::st_drop_geometry() %>%
                dplyr::select(source, data.type) %>%
                dplyr::distinct() %>%
                dplyr::filter(data.type == "PO") %>%
                dplyr::pull(source)

  inat.ind <- grep("iNat", POsources)

  # get locations of PO data
  locs <- dplyr::bind_rows(species.data$locs$cont) %>%
            dplyr::filter(source %in% POsources) %>%
            sf::st_buffer(dist = 500)

  # get dates of PO data
  dates <- c()
  for (s in 1:length(species.data$obs)) {
    if (names(species.data$obs)[s] %in% POsources) {
      dat <- species.data$obs[[s]] %>%
        dplyr::select(site.id, day, month, year, lat, lon, survey.id, survey.conducted, count, conus.grid.id, source, unique.id) %>%
        dplyr::mutate(site.survey.id = paste0(site.id, "-", survey.id))

      dates <- dplyr::bind_rows(dates, dat)
    }
  }

  # merge locs and dates
  locs <- dplyr::full_join(locs, dates, by = c("unique.id", "survey.id", "site.id", "lat", "lon", "source", "year", "survey.conducted"))

  # find all points that overlap

  # split into data source and all other data sources
  tmp1 <- dplyr::filter(locs, source == POsources[inat.ind])
  tmp2 <- dplyr::filter(locs, source != POsources[inat.ind])

  # find points that overlap
  inter <- sf::st_intersection(tmp1, tmp2)

  # find points that overlap on the same date
  inter1 <- dplyr::filter(inter,
                          #date == date.1,
                          day.x == day.y.1,
                          month.x == month.y.1,
                          year == year.1) %>%

              # pull site.survey.id for iNat record
              dplyr::pull(unique.id) %>%
              unique()

  return(inter1)
}
