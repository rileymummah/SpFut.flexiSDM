# test_that("survey_for_nimble() works", {
#   
#   rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
#                                         paste0("../../../species-futures/data/species/GPOR/IUCN/")),
#                          range.name = c("GAP", "IUCN"), crs = 4326)
#   
#   boundary <- rangelist[[1]]
#   grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.))
#   
#   region <- make_region(rangelist,
#                         buffer = 1,
#                         sub = F,
#                         boundary = boundary,
#                         grid = grid,
#                         rm.clumps = F,
#                         clump.size = 2,
#                         continuous = F)
#   
#   allfiles <- data.frame(file.name = c("iNat_test_PO", "iNat_test1_PO", "Dodd_test_DND", "Dodd_test1_DND", "NEARMI_test_count"),
#                          file.label = c("iNat_test", "iNat_test", "Dodd_test", "Dodd_test1", "NEARMI_test"),
#                          covar.mean = c(NA, NA, "elevation", "elevation", "depth"),
#                          covar.sum = c(NA, NA, "time", "time", "EffectValue"),
#                          data.type = c("PO", "PO", "DND", "DND", "count"))
#   
#   species.data <- load_species_data(sp.code = "GPOR",
#                                     sp.code.all = "GPOR",
#                                     file.info = allfiles,
#                                     file.path = "../../../species-futures/DATA SWAMP/data-ready-testfunctions/",
#                                     #file.path = "../species-futures/DATA SWAMP/data-ready-testfunctions/",
#                                     region = region,
#                                     filter.region = T,
#                                     year.start = 1800,
#                                     year.end = 2025,
#                                     coordunc = 1000,
#                                     coordunc_na.rm = T,
#                                     spat.thin = F,
#                                     keep.conus.grid.id = region$sp.grid$conus.grid.id)
#   
#   # test count data ----
#   data <- species.data$obs$NEARMI_test
#   countstart <- count_filter(data,
#                              covs.mean = "depth",
#                              covs.sum = "EffectValue",
#                              offset.area = NULL)
#   
#   count.covs <- c("depth", "EffectValue")
#   # clean up columns
#   data <- countstart %>%
#     select(conus.grid.id, site.id, survey.id, any_of(count.covs), any_of("area"), count) %>%
#     group_by(site.id) %>%
#     mutate(site.id = cur_group_id()) %>%
#     ungroup() %>%
#     mutate(across(any_of(count.covs), scale_this))
#   
#   
#   # format for nimble
#   COUNT <- survey_for_nimble(data,
#                              count.covs,
#                              type = "count",
#                              rename = 1,
#                              keep.conus.grid.id = region$sp.grid$conus.grid.id)
#   
#   # structure
#   expect_type(COUNT, "list")
#   expect_equal(length(COUNT), 2)
#   
#   # content
#   expect_equal(nrow(COUNT$data$Xy1), length(COUNT$data$Y1))
#   expect_equal(nrow(COUNT$data$Xy1), COUNT$constants$nY1)
#   expect_equal(nrow(COUNT$data$Xy1), length(COUNT$constants$Ycells1))
#   expect_equal(ncol(COUNT$data$Xy1), COUNT$constants$nCovY1)
#   
#   # format for nimble
#   COUNT <- survey_for_nimble(data,
#                              "",
#                              type = "count",
#                              rename = 1,
#                              keep.conus.grid.id = region$sp.grid$conus.grid.id)
#   
#   # structure
#   expect_type(COUNT, "list")
#   expect_equal(length(COUNT), 2)
#   
#   # content
#   expect_equal(nrow(COUNT$data$Xy1), length(COUNT$data$Y1))
#   expect_equal(nrow(COUNT$data$Xy1), COUNT$constants$nY1)
#   expect_equal(nrow(COUNT$data$Xy1), length(COUNT$constants$Ycells1))
#   expect_equal(ncol(COUNT$data$Xy1), COUNT$constants$nCovY1)
#   
#   
#   # test DND data ----
#   data <- species.data$obs$Dodd_test1
#   dndstart <- DND_filter(data,
#                          covs.mean = "elevation",
#                          covs.sum = "time",
#                          offset.area = NULL,
#                          DND.maybe = 1)
#   
#   dnd.covs <- c("elevation", "time")
#   # clean up columns
#   data <- dndstart %>%
#     select(conus.grid.id, site.id, survey.id, any_of(dnd.covs), any_of("area"), count) %>%
#     group_by(site.id) %>%
#     mutate(site.id = cur_group_id()) %>%
#     ungroup() %>%
#     mutate(across(any_of(dnd.covs), scale_this))
#   
#   
#   # format for nimble
#   DND <- survey_for_nimble(data,
#                            dnd.covs,
#                            type = "DND",
#                            rename = 1,
#                            keep.conus.grid.id = region$sp.grid$conus.grid.id)
#   
#   # structure
#   expect_type(DND, "list")
#   expect_equal(length(DND), 2)
#   
#   # content
#   expect_equal(nrow(DND$data$Xv1), length(DND$data$V1))
#   expect_equal(nrow(DND$data$Xv1), DND$constants$nV1)
#   expect_equal(nrow(DND$data$Xv1), length(DND$constants$Vcells1))
#   expect_equal(ncol(DND$data$Xv1), DND$constants$nCovV1)
#   
#   # format for nimble
#   DND <- survey_for_nimble(data,
#                            "",
#                            type = "DND",
#                            rename = 1,
#                            keep.conus.grid.id = region$sp.grid$conus.grid.id)
#   
#   # structure
#   expect_type(DND, "list")
#   expect_equal(length(DND), 2)
#   
#   # content
#   expect_equal(nrow(DND$data$Xv1), length(DND$data$V1))
#   expect_equal(nrow(DND$data$Xv1), DND$constants$nV1)
#   expect_equal(nrow(DND$data$Xv1), length(DND$constants$Vcells1))
#   expect_equal(ncol(DND$data$Xv1), DND$constants$nCovV1)
#   
#   
# })
