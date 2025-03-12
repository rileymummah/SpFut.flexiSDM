# count_filter <- function(data,
#                          covs.mean,
#                          covs.sum,
#                          types.true,
#                          sp.code,
#                          age.use = c("adult", "metamorph", "juvenile", "larva", "egg mass", "NR", NA),
#                          req.cols = c("site.id", "lat", "lon",
#                                       "day", "month", "year", "survey.conducted", "survey.id", "data.type",
#                                       "species", "time.to.detect", "count")) {
#
#   if(types.true == "CMR") {
#
#     cat("\nLoading CMR")
#
#     countstart <- data %>%
#       filter(age %in% age.use,
#              count == 1)
#
#     if (nrow(countstart) > 0) {
#       cat("...Changing from CMR to Count")
#
#
#       # get total count
#       countcount <- countstart %>%
#         select(!any_of(c(covs.mean, covs.sum))) %>%
#         group_by(survey.id) %>%
#         summarize(count = sum(count), .groups = "drop")
#
#       # get mean covariates
#       countcovs.mean <- countstart %>%
#         select(survey.id, any_of(covs.mean)) %>%
#         group_by(survey.id) %>%
#         summarize_all(mean, na.rm = T)
#
#       # get sum covariates
#       countcovs.sum <- countstart %>%
#         select(survey.id, any_of(covs.sum)) %>%
#         group_by(survey.id) %>%
#         summarize_all(sum, na.rm = T)
#
#       # get other important columns
#       countcols <- countstart %>%
#         select(conus.grid.id, site.id, survey.id, lat, lon, day, month, year, survey.conducted) %>%
#         distinct()
#
#       # put them together
#       countstart <- inner_join(countcols, countcount, by = "survey.id") %>%
#         full_join(countcovs.mean, by = "survey.id") %>%
#         full_join(countcovs.sum, by = "survey.id")
#     } else {
#       print("No data of appropriate species, year, and age in this dataset")
#       countstart <- countstart
#
#     }
#
#
#   } else if (types.true == "Count") {
#
#     cat("\nLoading count")
#
#     countstart <- data %>%
#       filter(age %in% age.use)
#
#     if (nrow(countstart) > 0) {
#       # get total count
#       countcount <- countstart %>%
#         select(!any_of(c(covs.mean, covs.sum))) %>%
#         group_by(survey.id) %>%
#         summarize(count = sum(count), .groups = "drop")
#
#       # get mean covariates
#       countcovs.mean <- countstart %>%
#         select(survey.id, any_of(covs.mean)) %>%
#         distinct() %>%
#         group_by(survey.id) %>%
#         summarize_all(mean, na.rm = T)
#
#       # get sum covariates
#       countcovs.sum <- countstart %>%
#         select(survey.id, any_of(covs.sum)) %>%
#         distinct() %>%
#         group_by(survey.id) %>%
#         summarize_all(sum, na.rm = T)
#
#       # get other important columns
#       countcols <- countstart %>%
#         select(conus.grid.id, site.id, survey.id, lat, lon, day, month, year, survey.conducted) %>%
#         distinct()
#
#       # put them together
#       countstart <- inner_join(countcols, countcount, by = "survey.id") %>%
#         full_join(countcovs.mean, by = "survey.id") %>%
#         full_join(countcovs.sum, by = "survey.id")
#     } else {
#       print("No data of appropriate species, year, and age in this dataset")
#       countstart <- countstart
#     }
#
#
#   } else {
#     stop(paste0("ERROR: Don't know how to convert ", types.true[ind], " to Count"))
#   }
#
#
#   return(countstart)
# }
