# DND_filter <- function(data,
#                        covs.mean,
#                        covs.sum,
#                        sp.code,
#                        DND.maybe,
#                        age.use = c("adult", "metamorph", "juvenile", "egg mass", "NR", NA),
#                        req.cols = c("unique.id", "site.id", "lat", "lon",
#                                     "day", "month", "year", "survey.conducted", "survey.id", "data.type",
#                                     "species", "time.to.detect", "count")) {
#
#
#
#   cat("\nLoading DND")
#
#   dndstart <- data %>%
#     filter(age %in% age.use)
#
#   if (nrow(dndstart) > 0) {
#
#     # get max count (0 or 1)
#     all.covs <- c(covs.mean, covs.sum)
#
#     dndcount <- dndstart %>%
#       select(survey.id, pass.id, age, count) %>%
#       mutate(count = case_when(count == 0 ~ 0,
#                                count == 1 ~ 1,
#                                count == 2 ~ DND.maybe)) %>%
#
#       # aggregate across passes
#       group_by(survey.id) %>%
#       summarize(count = max(count), .groups = "drop")
#
#     # get mean covariates (temperature, moon visibility, etc.)
#     if (length(covs.mean) > 0) {
#       dndcovs.mean <- dndstart %>%
#         select(survey.id, pass.id, any_of(covs.mean)) %>%
#         distinct() %>%
#         select(!pass.id) %>%
#
#         # aggregate across passes
#         group_by(survey.id) %>%
#         summarize_all(mean, na.rm = T)
#     }
#
#     # get sum covariates (duration, water volume)
#     if (length(covs.sum) > 0) {
#       dndcovs.sum <- dndstart %>%
#         select(survey.id, pass.id, any_of(covs.sum)) %>%
#         distinct() %>%
#
#         # aggregate across passes
#         group_by(survey.id) %>%
#         summarize_all(sum, na.rm = T) %>%
#         rename(npass = pass.id)
#     }
#
#
#     # get other important columns
#     dndcols <- dndstart %>%
#       select(conus.grid.id, site.id, survey.id, lat, lon, day, month, year, survey.conducted) %>%
#       distinct()
#
#     # put them together
#     dndstart <- inner_join(dndcols, dndcount, by = "survey.id")
#
#     if (length(covs.mean) > 0) {
#       dndstart <- full_join(dndstart, dndcovs.mean, by = "survey.id")
#     }
#     if (length(covs.sum) > 0) {
#       dndstart <- full_join(dndstart, dndcovs.sum, by = "survey.id")
#     }
#
#
#   } else {
#     cat("No data of appropriate species, year, and age in this dataset")
#     dndstart <- dndstart
#   }
#
#   return(dndstart)
# }
