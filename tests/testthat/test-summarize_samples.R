# test_that("summarize_samples() works", {
#
#   mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')
#
#   summarize_samples(samples = mod1$samples,
#                     data = mod1$data,
#                     constants = mod1$constants,
#                     project = 0,
#                     coarse.grid = F,
#                     cutoff = 0,
#                     block.out = 'none',
#                     gridkey = mod1$gridkey,
#                     spatkey = NULL,
#                     effort = F,
#                     SLURM = F) -> tmp
#
#   expect_true(is.list(tmp))
#   expect_true(nrow(tmp[[8]]) == 0)
#
#   # MODEL 4
#   mod4 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4.rds')
#
#   summarize_samples(samples = mod4$samples,
#                     data = mod4$data,
#                     constants = mod4$constants,
#                     project = 0,
#                     coarse.grid = T,
#                     cutoff = 0,
#                     block.out = 'none',
#                     gridkey = mod4$gridkey,
#                     spatkey = mod4$spatRegion$spatkey,
#                     effort = F,
#                     SLURM = F) -> tmp
#
#   expect_true(is.list(tmp))
#   expect_true(length(tmp) == 9)
#
#   # MODEL 4 W/ PROJECTIONS
#   mod4$samples <- lapply(mod4$samples,
#                          get_derived,
#                          data = mod4$data,
#                          project = 2,
#                          coarse.grid = T,
#                          spatRegion = mod4$spatRegion,
#                          pathToProj = '~/GitHub/species-futures/pkg-tests/test-setup-projections.R')
#
#   summarize_samples(samples = mod4$samples,
#                     data = mod4$data,
#                     constants = mod4$constants,
#                     project = 2,
#                     coarse.grid = T,
#                     cutoff = 0,
#                     block.out = 'none',
#                     gridkey = mod4$gridkey,
#                     spatkey = mod4$spatRegion$spatkey,
#                     effort = F,
#                     SLURM = F) -> tmp
#
#   expect_true(is.list(tmp))
#   expect_true(length(tmp) == 15)
#
# })
