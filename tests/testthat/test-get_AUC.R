test_that("get_AUC() works", {

  mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')

  mod1$samples <- lapply(mod1$samples,
                         get_derived,
                         data = mod1$data,
                         project = 0)

  summarize_samples(samples = mod1$samples,
                    data = mod1$data,
                    constants = mod1$constants,
                    project = 0,
                    coarse.grid = F,
                    cutoff = 0,
                    block.out = 'none',
                    gridkey = mod1$gridkey,
                    spatkey = NULL,
                    effort = F,
                    SLURM = F) -> out

  # regular ----
  auc <- get_AUC(mod1$species.data, out = out, block.out = 'none')
  expect_true(is.data.frame(auc))
  expect_equal(nrow(auc), 2)
  expect_equal(is.na(auc$AUCin[1]), T)


  # only PO data ----
  species.data1 <- mod1$species.data
  species.data1$obs$Dodd_test <- NULL
  species.data1$obs$NEARMI_test <- NULL
  auc <- get_AUC(species.data1, out = out, block.out = 'none')
  expect_equal(auc, NA)


  # CV ----
  mod5 <- readRDS('~/GitHub/species-futures/pkg-tests/mod5.rds')

  mod5$samples <- lapply(mod5$samples,
                         get_derived,
                         data = mod5$data,
                         project = 0)

  summarize_samples(samples = mod5$samples,
                    data = mod5$data,
                    constants = mod5$constants,
                    project = 0,
                    coarse.grid = F,
                    cutoff = 0,
                    block.out = 3,
                    gridkey = mod5$gridkey,
                    spatkey = NULL,
                    effort = F,
                    SLURM = F) -> out

  auc <- get_AUC(mod5$species.data, out = out, block.out = 3)

  expect_true(is.data.frame(auc))
  expect_equal(nrow(auc), 3)
  expect_equal(is.na(auc$AUCin[1]), T)

})
