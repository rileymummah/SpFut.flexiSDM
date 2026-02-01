# test_that("plot_auc() works", {
# 
#   # CV 1
#   mod4cv1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4cv1.rds')
#   auc1 <- get_AUC(mod4cv1$species.data, out = mod4cv1$out, block.out = 1)
#   
#   # CV 2
#   mod4cv2 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4cv2.rds')
#   auc2 <- get_AUC(mod4cv2$species.data, out = mod4cv2$out, block.out = 2)
#   
#   # CV 3
#   mod4cv3 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4cv3.rds')
#   auc3 <- get_AUC(mod4cv3$species.data, out = mod4cv3$out, block.out = 3)
#   
#   
#   # full
#   mod4 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4.rds')
#   auc0 <- get_AUC(mod4$species.data, out = mod4$out, block.out = "none")
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
