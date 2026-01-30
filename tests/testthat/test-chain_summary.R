# test_that("chain_summary() works", {
# 
#   mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')
# 
#   chain_summary(i = 5,
#                 samples = mod1$samples,
#                 chains = 1:3,
#                 cutoff = 0) -> tmp
# 
#   expect_true(is.data.frame(tmp))
#   expect_equal(dim(tmp), c(1,10))
# 
# 
#   mod3 <- readRDS('~/GitHub/species-futures/pkg-tests/mod3.rds')
# 
#   chain_summary(i = 15,
#                 samples = mod3$samples,
#                 chains = 1:3,
#                 cutoff = 0) -> tmp
# 
#   expect_true(is.data.frame(tmp))
#   expect_equal(dim(tmp), c(1,10))
# 
# })
