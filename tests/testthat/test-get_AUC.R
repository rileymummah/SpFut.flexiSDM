test_that("get_AUC() works", {

  mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')

  # regular ----
  auc <- get_AUC(mod1$species.data, out = mod1$out, block.out = 'none')
  expect_true(is.data.frame(auc))
  expect_equal(nrow(auc), 2)
  expect_equal(is.na(auc$AUCin[1]), T)


  # only PO data ----
  species.data1 <- mod1$species.data
  species.data1$obs$Dodd_test <- NULL
  species.data1$obs$NEARMI_test <- NULL
  auc <- get_AUC(species.data1, out = mod1$out, block.out = 'none')
  expect_equal(auc, NA)


  # CV ----
  mod4 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4.rds')

  auc <- get_AUC(mod4$species.data, out = mod4$out, block.out = 3)

  expect_true(is.data.frame(auc))
  expect_equal(nrow(auc), 3)
  expect_equal(is.na(auc$AUCin[1]), T)

})
