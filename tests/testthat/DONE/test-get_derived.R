test_that("get_derived() works", {
  # MODEL 1

  mod1 <- readRDS('~/GitHub/species-futures/pkg-tests/mod1.rds')
  samples <- mod1$samples[[1]]
  data <- mod1$data
  region <- mod1$region

  # remove psi from samples
  samples <- samples[,-grep("psi", colnames(samples))]
  
  # Only run this on one chain at a time
  test1 <- get_derived(samples = samples,
                       data = data,
                       project = 0,
                       coarse.grid = F,
                       sp.auto = F)

  expect_true(is.data.frame(test1))
  expect_equal(nrow(test1), 25)

  # Only run this on one chain at a time
  test1.1 <- get_derived(samples = samples,
                         data = data,
                         project = 2,
                         pathToProj = '~/GitHub/species-futures/pkg-tests/test-setup-projections.R',
                         coarse.grid = F,
                         sp.auto = F)

  expect_true(is.data.frame(test1.1))
  expect_equal(nrow(test1.1), 25)

  # MODEL 2

  mod2 <- readRDS('~/GitHub/species-futures/pkg-tests/mod2.rds')
  samples <- mod2$samples[[1]]
  data <- mod2$data
  region <- mod2$region

  # remove psi from samples
  samples <- samples[,-grep("psi", colnames(samples))]
  
  test2 <- get_derived(samples = samples,
                       data = data,
                       project = 0,
                       coarse.grid = F,
                       sp.auto = F)

  expect_true(is.data.frame(test2))
  expect_equal(nrow(test2), 25)


  test2.1 <- get_derived(samples = samples,
                         data = data,
                         project = 2,
                         pathToProj = '~/GitHub/species-futures/pkg-tests/test-setup-projections.R',
                         coarse.grid = F,
                         sp.auto = F)

  expect_true(is.data.frame(test2.1))
  expect_equal(nrow(test2.1), 25)

  # MODEL 3

  mod3 <- readRDS('~/GitHub/species-futures/pkg-tests/mod3.rds')
  samples <- mod3$samples[[1]]
  data <- mod3$data
  region <- mod3$region

  # remove psi from samples
  samples <- samples[,-grep("psi", colnames(samples))]
  
  test3 <- get_derived(samples = samples,
                       data = data,
                       project = 0,
                       coarse.grid = F,
                       sp.auto = T)

  expect_true(is.data.frame(test3))
  expect_equal(nrow(test3), 25)


  test3.1 <- get_derived(samples = samples,
                         data = data,
                         project = 2,
                         pathToProj = '~/GitHub/species-futures/pkg-tests/test-setup-projections.R',
                         coarse.grid = F,
                         sp.auto = T)

  expect_true(is.data.frame(test3.1))
  expect_equal(nrow(test3.1), 25)


  # MODEL 4

  mod4 <- readRDS('~/GitHub/species-futures/pkg-tests/mod4.rds')
  samples <- mod4$samples[[1]]
  data <- mod4$data
  region <- mod4$region

  # remove psi from samples
  samples <- samples[,-grep("psi", colnames(samples))]
  
  test4 <- get_derived(samples = samples,
                       data = data,
                       project = 0,
                       coarse.grid = T,
                       sp.auto = T)

  expect_true(is.data.frame(test4))
  expect_equal(nrow(test4), 25)


  test4.1 <- get_derived(samples = samples,
                         data = data,
                         project = 2,
                         pathToProj = '~/GitHub/species-futures/pkg-tests/test-setup-projections.R',
                         coarse.grid = T,
                         sp.auto = T,
                         spatRegion = mod4$spatRegion)

  expect_true(is.data.frame(test4.1))
  expect_equal(nrow(test4.1), 25)

})
