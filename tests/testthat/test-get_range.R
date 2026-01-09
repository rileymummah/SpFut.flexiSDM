# 
# test_that("get_range() makes list", {
# 
#   expect_true(file.exists('../../../species-futures/data/species/GPOR/GAP/'))
# 
#   expect_type(get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
#                                         paste0("../../../species-futures/data/species/GPOR/IUCN/")),
#                         range.name = c("GAP", "IUCN"), crs = 4326),
#               "list")
# 
# })
# 
# 
# 
# test_that("get_range() works with variable number of inputs", {
#   tmp <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
#                                   paste0("../../../species-futures/data/species/GPOR/IUCN/")),
#                    range.name = c("GAP", "IUCN"), crs = 4326)
#   expect_equal(length(tmp), 2)
# 
# 
#   tmp <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/")),
#                    range.name = c("GAP"), crs = 4326)
#   expect_equal(length(tmp), 1)
# 
# })
# 
# 
# test_that("get_range() returns sf objects", {
# 
#   tmp <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
#                                   paste0("../../../species-futures/data/species/GPOR/IUCN/")),
#                    range.name = c("GAP", "IUCN"), crs = 4326)
# 
#   expect_s3_class(tmp[[1]], "sf")
# 
# })
# 
# 
# test_that("get_range() works without range.name argument", {
# 
#   tmp <- get_range(range.path = c(paste0("../../../species-futures/data/species/GPOR/GAP/"),
#                                   paste0("../../../species-futures/data/species/GPOR/IUCN/")),
#                    crs = 4326)
# 
#   expect_type(tmp, "list")
# 
# })
# 
# 
