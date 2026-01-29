# 
# test_that("load_species_data() works with iNat format data", {
# 
#     rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
#                                     paste0("../../../species-futures/data/species/GPOR/IUCN/")),
#                      range.name = c("GAP", "IUCN"), crs = 4326)
# 
#     boundary <- rangelist[[1]]
#     grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.)) %>%
#       rename(geometry = x)
# 
#     region <- make_region(rangelist,
#                           buffer = 1,
#                           sub = F,
#                           boundary = boundary,
#                           grid = grid,
#                           rm.clumps = F,
#                           clump.size = 2,
#                           continuous = F)
#     expect_true(file.exists('~/GitHub/species-futures/pkg-tests/'))
# 
# 
#     allfiles <- data.frame(file.name = c("iNat_test_PO"),
#                            file.label = c("iNat_test"),
#                            covar.mean = "",
#                            covar.sum = "",
#                            data.type = c("PO"))
# 
#     # test normal ----
#     species.data <- load_species_data(sp.code = "GPOR",
#                                       sp.code.all = "GPOR",
#                                       file.info = allfiles,
#                                       file.path = "~/GitHub/species-futures/pkg-tests/",
#                                       #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                       region = region,
#                                       filter.region = T,
#                                       year.start = 1800,
#                                       year.end = 2025,
#                                       coordunc = 1000,
#                                       coordunc_na.rm = T,
#                                       spat.thin = F,
#                                       keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#     # output structure
#     expect_type(species.data, "list")
#     expect_equal(length(species.data), 2)
#     expect_equal(names(species.data), c("locs", "obs"))
#     expect_type(species.data$locs, "list")
#     expect_type(species.data$obs, "list")
#     expect_s3_class(species.data$locs$cont, "sf")
# 
#     # output content
#     expect_equal(length(species.data$obs), 1)
#     expect_equal(nrow(species.data$obs$iNat_test), 5)
# 
#     # test covar = NA ----
#     allfiles <- data.frame(file.name = c("iNat_test_PO"),
#                            file.label = c("iNat_test"),
#                            covar.mean = NA,
#                            covar.sum = NA,
#                            data.type = c("PO"))
# 
# 
#     species.data <- load_species_data(sp.code = "GPOR",
#                                       sp.code.all = "GPOR",
#                                       file.info = allfiles,
#                                       file.path = "~/GitHub/species-futures/pkg-tests/",
#                                       #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                       region = region,
#                                       filter.region = T,
#                                       year.start = 1800,
#                                       year.end = 2025,
#                                       coordunc = 1000,
#                                       coordunc_na.rm = T,
#                                       spat.thin = F,
#                                       keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
# 
#     # output structure
#     expect_type(species.data, "list")
#     expect_equal(length(species.data), 2)
#     expect_equal(names(species.data), c("locs", "obs"))
#     expect_type(species.data$locs, "list")
#     expect_type(species.data$obs, "list")
#     expect_s3_class(species.data$locs$cont, "sf")
# 
#     # output content
#     expect_equal(length(species.data$obs), 1)
#     expect_equal(nrow(species.data$obs$iNat_test), 5)
# 
# 
#     # test different coordunc parameters ----
#     species.data <- load_species_data(sp.code = "GPOR",
#                                       sp.code.all = "GPOR",
#                                       file.info = allfiles,
#                                       file.path = "~/GitHub/species-futures/pkg-tests/",
#                                       #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                       region = region,
#                                       filter.region = T,
#                                       year.start = 1800,
#                                       year.end = 2025,
#                                       coordunc = 10,
#                                       coordunc_na.rm = F,
#                                       spat.thin = F,
#                                       keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#     # output structure
#     expect_type(species.data, "list")
#     expect_equal(length(species.data), 2)
#     expect_equal(names(species.data), c("locs", "obs"))
#     expect_type(species.data$locs, "list")
#     expect_type(species.data$obs, "list")
#     expect_s3_class(species.data$locs$cont, "sf")
# 
#     # output content
#     expect_equal(length(species.data$obs), 1)
#     expect_equal(nrow(species.data$obs$iNat_test), 2)
# 
# 
#     species.data <- load_species_data(sp.code = "GPOR",
#                                       sp.code.all = "GPOR",
#                                       file.info = allfiles,
#                                       file.path = "~/GitHub/species-futures/pkg-tests/",
#                                       #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                       region = region,
#                                       filter.region = T,
#                                       year.start = 1800,
#                                       year.end = 2025,
#                                       coordunc = 0,
#                                       coordunc_na.rm = F,
#                                       spat.thin = F,
#                                       keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#     # output structure
#     expect_type(species.data, "list")
#     expect_equal(length(species.data), 2)
#     expect_equal(names(species.data), c("locs", "obs"))
#     expect_type(species.data$locs, "list")
#     expect_type(species.data$obs, "list")
#     expect_type(species.data$locs$cont, "NULL")
# 
#     # output content
#     expect_equal(length(species.data$obs), 0)
#     expect_equal(nrow(species.data$obs$iNat_test), NULL)
# 
#     # test spat.thin = T ----
#     allfiles <- data.frame(file.name = c("iNat_test1_PO"),
#                            file.label = c("iNat_test"),
#                            covar.mean = NA,
#                            covar.sum = NA,
#                            data.type = c("PO"))
# 
# 
#     species.data <- load_species_data(sp.code = "GPOR",
#                                       sp.code.all = "GPOR",
#                                       file.info = allfiles,
#                                       file.path = "~/GitHub/species-futures/pkg-tests/",
#                                       #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                       region = region,
#                                       filter.region = T,
#                                       year.start = 1800,
#                                       year.end = 2025,
#                                       coordunc = 1000,
#                                       coordunc_na.rm = F,
#                                       spat.thin = T,
#                                       keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#     # output structure
#     expect_type(species.data, "list")
#     expect_equal(length(species.data), 2)
#     expect_equal(names(species.data), c("locs", "obs"))
#     expect_type(species.data$locs, "list")
#     expect_type(species.data$obs, "list")
#     expect_s3_class(species.data$locs$cont, "sf")
# 
#     # output content
#     expect_equal(length(species.data$obs), 1)
#     expect_equal(nrow(species.data$obs$iNat_test), 6)
# 
# 
#     # test two datasets with different names ----
#     allfiles <- data.frame(file.name = c("iNat_test1_PO", "iNat_test_PO"),
#                            file.label = c("iNat_test1", "iNat_test"),
#                            covar.mean = c(NA, NA),
#                            covar.sum = c(NA, NA),
#                            data.type = c("PO", "PO"))
# 
#     species.data <- load_species_data(sp.code = "GPOR",
#                                       sp.code.all = "GPOR",
#                                       file.info = allfiles,
#                                       file.path = "~/GitHub/species-futures/pkg-tests/",
#                                       #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                       region = region,
#                                       filter.region = T,
#                                       year.start = 1800,
#                                       year.end = 2025,
#                                       coordunc = 1000,
#                                       coordunc_na.rm = T,
#                                       spat.thin = F,
#                                       keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#     # output structure
#     expect_type(species.data, "list")
#     expect_equal(length(species.data), 2)
#     expect_equal(names(species.data), c("locs", "obs"))
#     expect_type(species.data$locs, "list")
#     expect_type(species.data$obs, "list")
#     expect_s3_class(species.data$locs$cont, "sf")
# 
#     # output content
#     expect_equal(length(species.data$obs), 2)
#     expect_equal(nrow(species.data$obs$iNat_test), 5)
#     expect_equal(nrow(species.data$obs$iNat_test1), 10)
# 
#     # test two datasets with same name ----
#     allfiles <- data.frame(file.name = c("iNat_test1_PO", "iNat_test_PO"),
#                            file.label = c("iNat_test", "iNat_test"),
#                            covar.mean = c(NA, NA),
#                            covar.sum = c(NA, NA),
#                            data.type = c("PO", "PO"))
# 
#     species.data <- load_species_data(sp.code = "GPOR",
#                                       sp.code.all = "GPOR",
#                                       file.info = allfiles,
#                                       file.path = "~/GitHub/species-futures/pkg-tests/",
#                                       #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                       region = region,
#                                       filter.region = T,
#                                       year.start = 1800,
#                                       year.end = 2025,
#                                       coordunc = 1000,
#                                       coordunc_na.rm = T,
#                                       spat.thin = F,
#                                       keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#     # output structure
#     expect_type(species.data, "list")
#     expect_equal(length(species.data), 2)
#     expect_equal(names(species.data), c("locs", "obs"))
#     expect_type(species.data$locs, "list")
#     expect_type(species.data$obs, "list")
#     expect_s3_class(species.data$locs$cont, "sf")
# 
#     # output content
#     expect_equal(length(species.data$obs), 1)
#     expect_equal(nrow(species.data$obs$iNat_test), 15)
# 
# 
#     # test two species codes ----
#     allfiles <- data.frame(file.name = c("iNat_test_PO"),
#                            file.label = c("iNat_test"),
#                            covar.mean = "",
#                            covar.sum = "",
#                            data.type = c("PO"))
# 
#     species.data <- load_species_data(sp.code = "GPOR",
#                                       sp.code.all = c("GPOR", "EBIS"),
#                                       file.info = allfiles,
#                                       file.path = "~/GitHub/species-futures/pkg-tests/",
#                                       #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                       region = region,
#                                       filter.region = T,
#                                       year.start = 1800,
#                                       year.end = 2025,
#                                       coordunc = 1000,
#                                       coordunc_na.rm = T,
#                                       spat.thin = F,
#                                       keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#     # output structure
#     expect_type(species.data, "list")
#     expect_equal(length(species.data), 2)
#     expect_equal(names(species.data), c("locs", "obs"))
#     expect_type(species.data$locs, "list")
#     expect_type(species.data$obs, "list")
#     expect_s3_class(species.data$locs$cont, "sf")
# 
#     # output content
#     expect_equal(length(species.data$obs), 1)
#     expect_equal(nrow(species.data$obs$iNat_test), 10)
# 
# })
# 
# 
# 
# 
# test_that("load_species_data() works with survey data", {
# 
#   rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
#                                         paste0("../../../species-futures/data/species/GPOR/IUCN/")),
#                          range.name = c("GAP", "IUCN"), crs = 4326)
# 
#   boundary <- rangelist[[1]]
#   grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.)) %>%
#     rename(geometry = x)
# 
#   region <- make_region(rangelist,
#                         buffer = 1,
#                         sub = F,
#                         boundary = boundary,
#                         grid = grid,
#                         rm.clumps = F,
#                         clump.size = 2,
#                         continuous = F)
#   expect_true(file.exists('~/GitHub/species-futures/pkg-tests/'))
# 
# 
#   # dat <- read.csv("../species-futures/pkg-tests/data-ready-testfunctions/NEARMI_test_count.csv") %>%
#   #   st_as_sf(coords = c("lon", "lat"), crs = 4326)
#   # ggplot(region$sp.grid) + geom_sf() + geom_sf(data = dat)
# 
# 
#   # test normal ----
#   allfiles <- data.frame(file.name = c("NEARMI_test_count"),
#                          file.label = c("NEARMI_test"),
#                          covar.mean = "StartWaterTemp",
#                          covar.sum = "EffectValue",
#                          data.type = c("count"))
# 
#   species.data <- load_species_data(sp.code = "GPOR",
#                                     sp.code.all = "GPOR",
#                                     file.info = allfiles,
#                                     file.path = "~/GitHub/species-futures/pkg-tests/",
#                                     #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                     region = region,
#                                     filter.region = T,
#                                     year.start = 1800,
#                                     year.end = 2025,
#                                     keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#   # output structure
#   expect_type(species.data, "list")
#   expect_equal(length(species.data), 2)
#   expect_equal(names(species.data), c("locs", "obs"))
#   expect_type(species.data$locs, "list")
#   expect_type(species.data$obs, "list")
#   expect_s3_class(species.data$locs$cont, "sf")
# 
#   # output content
#   expect_equal(length(species.data$obs), 1)
#   expect_equal(nrow(species.data$obs$NEARMI_test), 60)
#   expect_equal(colnames(species.data$obs$NEARMI_test)[19:20], c("StartWaterTemp", "EffectValue"))
# 
# 
#   # test two species ----
#   allfiles <- data.frame(file.name = c("NEARMI_test_count"),
#                          file.label = c("NEARMI_test"),
#                          covar.mean = "StartWaterTemp",
#                          covar.sum = "EffectValue",
#                          data.type = c("count"))
# 
#   species.data <- load_species_data(sp.code = "GPOR",
#                                     sp.code.all = c("GPOR", "EWIL"),
#                                     file.info = allfiles,
#                                     file.path = "~/GitHub/species-futures/pkg-tests/",
#                                     #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                     region = region,
#                                     filter.region = T,
#                                     year.start = 1800,
#                                     year.end = 2025,
#                                     keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#   # output structure
#   expect_type(species.data, "list")
#   expect_equal(length(species.data), 2)
#   expect_equal(names(species.data), c("locs", "obs"))
#   expect_type(species.data$locs, "list")
#   expect_type(species.data$obs, "list")
#   expect_s3_class(species.data$locs$cont, "sf")
# 
#   # output content
#   expect_equal(length(species.data$obs), 1)
#   expect_equal(nrow(species.data$obs$NEARMI_test), 120)
#   expect_equal(colnames(species.data$obs$NEARMI_test)[19:20], c("StartWaterTemp", "EffectValue"))
# 
# 
# 
#   # test different year values ----
#   allfiles <- data.frame(file.name = c("NEARMI_test_count"),
#                          file.label = c("NEARMI_test"),
#                          covar.mean = "StartWaterTemp",
#                          covar.sum = "EffectValue",
#                          data.type = c("count"))
# 
#   species.data <- load_species_data(sp.code = "GPOR",
#                                     sp.code.all = c("GPOR", "EWIL"),
#                                     file.info = allfiles,
#                                     file.path = "~/GitHub/species-futures/pkg-tests/",
#                                     #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                     region = region,
#                                     filter.region = T,
#                                     year.start = 2023,
#                                     year.end = 2025,
#                                     keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#   # output structure
#   expect_type(species.data, "list")
#   expect_equal(length(species.data), 2)
#   expect_equal(names(species.data), c("locs", "obs"))
#   expect_type(species.data$locs, "list")
#   expect_type(species.data$obs, "list")
#   expect_s3_class(species.data$locs$cont, "sf")
# 
#   # output content
#   expect_equal(length(species.data$obs), 1)
#   expect_equal(nrow(species.data$obs$NEARMI_test), 54)
#   expect_equal(colnames(species.data$obs$NEARMI_test)[19:20], c("StartWaterTemp", "EffectValue"))
# 
# 
#   # test empty covariates ----
#   allfiles <- data.frame(file.name = c("NEARMI_test_count"),
#                          file.label = c("NEARMI_test"),
#                          covar.mean = "",
#                          covar.sum = NA,
#                          data.type = c("count"))
# 
#   species.data <- load_species_data(sp.code = "GPOR",
#                                     sp.code.all = c("GPOR", "EWIL"),
#                                     file.info = allfiles,
#                                     file.path = "~/GitHub/species-futures/pkg-tests/",
#                                     #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                     region = region,
#                                     filter.region = T,
#                                     year.start = 2023,
#                                     year.end = 2025,
#                                     keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#   # output structure
#   expect_type(species.data, "list")
#   expect_equal(length(species.data), 2)
#   expect_equal(names(species.data), c("locs", "obs"))
#   expect_type(species.data$locs, "list")
#   expect_type(species.data$obs, "list")
#   expect_s3_class(species.data$locs$cont, "sf")
# 
#   # output content
#   expect_equal(length(species.data$obs), 1)
#   expect_equal(nrow(species.data$obs$NEARMI_test), 54)
#   expect_equal(length(colnames(species.data$obs$NEARMI_test)), 18)
# 
# 
#   # test bad covariates ----
#   allfiles <- data.frame(file.name = c("NEARMI_test_count"),
#                          file.label = c("NEARMI_test"),
#                          covar.mean = "notincovariates",
#                          covar.sum = NA,
#                          data.type = c("count"))
# 
# 
#   expect_warning(species.data <- load_species_data(sp.code = "GPOR",
#                                     sp.code.all = c("GPOR", "EWIL"),
#                                     file.info = allfiles,
#                                     file.path = "~/GitHub/species-futures/pkg-tests/",
#                                     #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                     region = region,
#                                     filter.region = T,
#                                     year.start = 2023,
#                                     year.end = 2025,
#                                     keep.conus.grid.id = region$sp.grid$conus.grid.id))
# 
#   # output structure
#   expect_type(species.data, "list")
#   expect_equal(length(species.data), 2)
#   expect_equal(names(species.data), c("locs", "obs"))
#   expect_type(species.data$locs, "list")
#   expect_type(species.data$obs, "list")
#   expect_s3_class(species.data$locs$cont, "sf")
# 
#   # output content
#   expect_equal(length(species.data$obs), 1)
#   expect_equal(nrow(species.data$obs$NEARMI_test), 54)
#   expect_equal(length(colnames(species.data$obs$NEARMI_test)), 18)
# 
# 
# 
#   # test count and DND ----
#   allfiles <- data.frame(file.name = c("NEARMI_test_count", "Dodd_test_DND"),
#                          file.label = c("NEARMI_test", "Dodd_test"),
#                          covar.mean = c("StartWaterTemp", NA),
#                          covar.sum = c("EffectValue", "time"),
#                          data.type = c("count", "DND"))
# 
#   species.data <- load_species_data(sp.code = "GPOR",
#                                     sp.code.all = "GPOR",
#                                     file.info = allfiles,
#                                     file.path = "~/GitHub/species-futures/pkg-tests/",
#                                     #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                     region = region,
#                                     filter.region = T,
#                                     year.start = 1800,
#                                     year.end = 2025,
#                                     keep.conus.grid.id = region$sp.grid$conus.grid.id)
# 
#   # output structure
#   expect_type(species.data, "list")
#   expect_equal(length(species.data), 2)
#   expect_equal(names(species.data), c("locs", "obs"))
#   expect_type(species.data$locs, "list")
#   expect_type(species.data$obs, "list")
#   expect_s3_class(species.data$locs$cont, "sf")
# 
#   # output content
#   expect_equal(length(species.data$obs), 2)
#   expect_equal(nrow(species.data$obs$NEARMI_test), 60)
#   expect_equal(colnames(species.data$obs$NEARMI_test)[19:20], c("StartWaterTemp", "EffectValue"))
#   expect_equal(nrow(species.data$obs$Dodd_test), 20)
#   expect_equal(colnames(species.data$obs$Dodd_test)[19], c("time"))
# 
# 
# })
# 
# 
# 
# 
# test_that("load_species_data() works with CV", {
#   
#   rangelist <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
#                                         paste0("../../../species-futures/data/species/GPOR/IUCN/")),
#                          range.name = c("GAP", "IUCN"), crs = 4326)
#   
#   boundary <- rangelist[[1]]
#   grid <- st_make_grid(st_transform(boundary, crs = 3857), cellsize = 100000) %>% st_as_sf() %>% mutate(conus.grid.id = 1:nrow(.)) %>%
#     rename(geometry = x)
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
#   # make CV blocks
#   spatblocks <- make_CV_blocks(region, 5, 5, 3)
#   
#   block1 <- filter(spatblocks, folds == 2)
#   
#   # find grid.ids for test block, everything else is train
#   suppressWarnings(test.i <- st_intersection(region$sp.grid, block1) %>%
#     pull(conus.grid.id) %>%
#     unique())
#   train.i <- filter(region$sp.grid, conus.grid.id %in% test.i == F) %>%
#     pull(conus.grid.id)
#   
#   gridkey <- select(region$sp.grid, conus.grid.id) %>%
#     st_drop_geometry() %>%
#     mutate(grid.id = 1:nrow(.),
#            group = case_when(conus.grid.id %in% train.i ~ "train",
#                              conus.grid.id %in% test.i ~ "test"))
#   
#   
#   # test normal ----
#   allfiles <- data.frame(file.name = c("NEARMI_test_count", "iNat_test_PO"),
#                          file.label = c("NEARMI_test", "iNat"),
#                          covar.mean = c("StartWaterTemp", NA),
#                          covar.sum = c("EffectValue", NA),
#                          data.type = c("count", "PO"))
#   
#   species.data <- load_species_data(sp.code = "GPOR",
#                                     sp.code.all = "GPOR",
#                                     file.info = allfiles,
#                                     file.path = "~/GitHub/species-futures/pkg-tests/",
#                                     #file.path = "../species-futures/pkg-tests/data-ready-testfunctions/",
#                                     region = region,
#                                     filter.region = T,
#                                     year.start = 1800,
#                                     year.end = 2025,
#                                     keep.conus.grid.id = gridkey$conus.grid.id[which(gridkey$group == "train")])
#   
#   suppressWarnings(tmp <- st_intersection(species.data$locs$cont, spatblocks) %>%
#     filter(folds == 2))
#   expect_equal(nrow(tmp), 0)
#   
#   
#   
# })
# 
