# test_that("plot_auc() works", {
# 
#   # CV 1
#   mod4cv1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4cv1.rds')
# 
#   mod4cv1$samples <- lapply(mod4cv1$samples,
#                          get_derived,
#                          data = mod4cv1$data,
#                          project = 0)
# 
#   summarize_samples(samples = mod4cv1$samples,
#                     data = mod4cv1$data,
#                     constants = mod4cv1$constants,
#                     project = 0,
#                     coarse.grid = F,
#                     cutoff = 0,
#                     block.out = 1,
#                     gridkey = mod4cv1$gridkey,
#                     spatkey = NULL,
#                     effort = F,
#                     SLURM = F) -> out1
# 
#   auc1 <- get_AUC(mod4cv1$species.data, out = out1, block.out = 1)
#   
#   # CV 2
#   mod4cv2 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4cv2.rds')
#   
#   mod4cv2$samples <- lapply(mod4cv2$samples,
#                             get_derived,
#                             data = mod4cv2$data,
#                             project = 0)
#   
#   summarize_samples(samples = mod4cv2$samples,
#                     data = mod4cv2$data,
#                     constants = mod4cv2$constants,
#                     project = 0,
#                     coarse.grid = F,
#                     cutoff = 0,
#                     block.out = 2,
#                     gridkey = mod4cv2$gridkey,
#                     spatkey = NULL,
#                     effort = F,
#                     SLURM = F) -> out2
#   
#   auc2 <- get_AUC(mod4cv2$species.data, out = out2, block.out = 2)
#   
#   # CV 3
#   mod4cv3 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4cv3.rds')
#   
#   mod4cv3$samples <- lapply(mod4cv3$samples,
#                             get_derived,
#                             data = mod4cv3$data,
#                             project = 0)
#   
#   summarize_samples(samples = mod4cv3$samples,
#                     data = mod4cv3$data,
#                     constants = mod4cv3$constants,
#                     project = 0,
#                     coarse.grid = F,
#                     cutoff = 0,
#                     block.out = 3,
#                     gridkey = mod4cv3$gridkey,
#                     spatkey = NULL,
#                     effort = F,
#                     SLURM = F) -> out3
#   
#   auc3 <- get_AUC(mod4cv3$species.data, out = out3, block.out = 3)
#   
#   
#   # full
#   mod4 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4.rds')
#   
#   mod4$samples <- lapply(mod4$samples,
#                             get_derived,
#                             data = mod4$data,
#                             project = 0)
#   
#   summarize_samples(samples = mod4$samples,
#                     data = mod4$data,
#                     constants = mod4$constants,
#                     project = 0,
#                     coarse.grid = F,
#                     cutoff = 0,
#                     block.out = "none",
#                     gridkey = mod4$gridkey,
#                     spatkey = NULL,
#                     effort = F,
#                     SLURM = F) -> out0
#   
#   auc0 <- get_AUC(mod4$species.data, out = out0, block.out = "none")
# 
#   # no lines ----
#   auc <- bind_rows(auc1, auc2, auc3, auc0)
#   out <- plot_auc(auc, lines = F)
#   expect_doppelganger("auc without lines", out)
# 
#   # lines ----
#   out <- plot_auc(auc, lines = T)
#   expect_doppelganger("auc with lines", out)
# })
