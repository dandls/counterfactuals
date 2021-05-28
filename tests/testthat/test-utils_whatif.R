library(data.table)

# test_that("whatif_algo returns correct values for numeric values only", {
#   x = data.table(a = 1.5, b = 10)
#   data = data.table(a = c(0, 2, 0), b = c(0, 15))
#   ps = paradox::ParamSet$new(
#     list(
#       paradox::ParamDbl$new("a", lower = 0, upper = 2),
#       paradox::ParamInt$new("b", lower = 0, upper = 20)
#     )
#   )
# 
#   res = whatif_algo(
#     X = data, 
#     x_interest = x, 
#     y_hat = c(), 
#     desired_y_hat_range, 
#     param_set, 
#     n_cfactuals
#   )
# 
#   expect_equal(gower_dist(x, x, param_set = ps), 0)
#   res1 = as.vector(StatMatch::gower.dist(x, data, rngs = c(2, 20)))
#   res2 = gower_dist(x, data, param_set = ps)
#   expect_equal(res1, res2)
# })
# 
# test_that("gower_dist returns correct values for numeric values only", {
#   x = data.table(a = 1.5, b = 10)
#   data = data.table(a = c(0, 2), b = c(0, 15))
#   ps = paradox::ParamSet$new(
#     list(
#       paradox::ParamDbl$new("a", lower = 0, upper = 2),
#       paradox::ParamInt$new("b", lower = 0, upper = 20)
#     )
#   )
#   expect_equal(gower_dist(x, x, param_set = ps), 0)
#   res1 = as.vector(StatMatch::gower.dist(x, data, rngs = c(2, 20)))
#   res2 = gower_dist(x, data, param_set = ps)
#   expect_equal(res1, res2)
# })

# 
# test_that("gower_dist returns correct values for discrete values only", {
#   a_levels = c("cold", "warm")
#   b_levels = c("sunny", "rainy")
#   x = data.table(a = factor("cold", levels = a_levels), b = factor("rainy", levels = b_levels))
#   data = data.table(
#     a = factor(c("warm", "cold"), levels = a_levels),
#     b = factor(c("sunny", "rainy"), levels = b_levels)
#   )
#   ps = ParamHelpers::makeParamSet(
#     ParamHelpers::makeDiscreteParam("a", a_levels),
#     ParamHelpers::makeDiscreteParam("b", b_levels)
#   )
#   expect_equal(gower_dist(x, x, param_set = ps), 0)
#   res1 = as.vector(StatMatch::gower.dist(x, data, rngs = c(NA, NA)))
#   res2 = gower_dist(x, data, param_set = ps)
#   expect_equal(res1, res2)
# })
# 
# 
# test_that("gower_dist returns correct values for mixed values", {
#   b_levels = c("sunny", "rainy")
#   x = data.table(a = 1.5, b = factor("rainy", levels = b_levels))
#   data = data.table(
#     a = c(0, 2),
#     b = factor(c("sunny", "rainy"), levels = b_levels)
#   )
#   ps = ParamHelpers::makeParamSet(
#     ParamHelpers::makeNumericParam("a", lower = 0, upper = 2),
#     ParamHelpers::makeDiscreteParam("b", b_levels)
#   )
#   expect_equal(gower_dist(x, x, param_set = ps), 0)
#   res1 = as.vector(StatMatch::gower.dist(x, data, rngs = c(2L, NA)))
#   res2 = gower_dist(x, data, param_set = ps)
#   expect_equal(res1, res2)
# })
# 
# 
# test_that("gower_dist returns equal results with and without parallelization", {
#   skip_if_not(parallel::detectCores() > 1L)
#   set.seed(54542142)
#   b_levels = c("sunny", "rainy")
#   x = data.table(a = 0.5, b = factor("rainy", levels = b_levels))
#   data = data.table(
#     a = rnorm(1000L),
#     b = factor(sample(b_levels, 1000, replace = TRUE), levels = b_levels)
#   )
#   ps = ParamHelpers::makeParamSet(
#     ParamHelpers::makeNumericParam("a", lower = 0L, upper = 2L),
#     ParamHelpers::makeDiscreteParam("b", b_levels)
#   )
#   future::plan(future::multisession, workers = parallel::detectCores() - 1L)
#   par = gower_dist(x, data, param_set = ps)
#   future::plan(future::sequential)
#   sequ = gower_dist(x, data, param_set = ps)
#   expect_equal(par, sequ)
# })
# 
# test_that("gower_dist returns error if `x` and `data` have different column names", {
#   expect_snapshot_error(gower_dist(iris[1, ], mtcars[, 1:5]))
# })
# 
# test_that("gower_dist returns error if `x` and `data` have different column types", {
#   iris_fake = iris
#   iris_fake[, 1:2] = as.character(iris_fake[, 1:2])
#   expect_snapshot_error(gower_dist(iris_fake[1L, ], iris))
# })