# id_dup_records <- function(sp.code,
#                            species.data) {
#
#
#   allfiles <- read.csv("DATA SWAMP/dataset-summary-full.csv") %>%
#     filter(species == sp.code)
#
#   POsources <- read.csv("DATA SWAMP/00-data-summary.csv") %>%
#     filter(Use == 1,
#            Type.true == "PO",
#            Data.Swamp.file.name %in% allfiles$file) %>%
#     pull(Name)
#
#   inat.ind <- grep("iNat", POsources)
#
#   # get locations of PO data
#   locs <- bind_rows(species.data$locs$cont) %>%
#     filter(source %in% POsources) %>%
#     st_buffer(dist = 500)
#
#   # get dates of PO data
#   dates <- c()
#   for (s in 1:length(species.data$obs)) {
#     if (names(species.data$obs)[s] %in% POsources) {
#       dat <- species.data$obs[[s]] %>%
#         rename(grid.id = "sp.grid.id") %>%
#         select(site.id, day, month, year, lat, lon, survey.id, survey.conducted, count, grid.id, conus.grid.id, source, unique.id) %>%
#         mutate(site.survey.id = paste0(site.id, "-", survey.id))
#
#       dates <- bind_rows(dates, dat)
#     }
#   }
#
#   # merge locs and dates
#   locs <- full_join(locs, dates, by = c("unique.id", "survey.id", "site.id", "lat", "lon", "source", "year", "survey.conducted"))
#
#   # find all points that overlap
#
#   # split into data source and all other data sources
#   tmp1 <- filter(locs, source == POsources[inat.ind])
#   tmp2 <- filter(locs, source != POsources[inat.ind])
#
#   # find points that overlap
#   inter <- st_intersection(tmp1, tmp2)
#
#   # find points that overlap on the same date
#   inter1 <- filter(inter,
#                    #date == date.1,
#                    day == day.1,
#                    month == month.1,
#                    year == year.1) %>%
#
#     # pull site.survey.id for iNat record
#     pull(unique.id) %>%
#     unique()
#
#   # remove points from iNat record
#   # rm <- filter(locs, (site.survey.id %in% inter1) == F) %>%
#   #   pull(site.id)
#
#   return(inter1)
# }
