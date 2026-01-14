# test_that("select_covar works", {
#   
#   set.seed(5)
#   covar <- data.frame(temp = rnorm(1000, 0, 1),
#                       prec = runif(1000, 0, 1) + rnorm(1000, 0, 1),
#                       elev = runif(1000, 0, 1) + runif(1000, 0, 1)) %>%
#     mutate(prec = temp + prec)
#   
#   out <- select_covar(covar, threshold = 0.4)
#   expect_equal(out, "temp")
#   
# })
