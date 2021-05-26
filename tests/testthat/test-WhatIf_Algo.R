library(randomForest)

test_that("Returns correct output format for factor columns only", {
  set.seed(54542142)
  df = mtcars[, c("am", "vs", "cyl")]
  df$am = as.factor(df$am)
  df$vs = as.factor(df$vs)
  df$cyl = as.factor(df$cyl)
  
  rf = randomForest::randomForest(am ~ ., data = df, ntree = 5L)
  pred = Predictor$new(rf, data = df, type = "prob")
  desired_class = "1"
  X = df[, c("vs", "cyl")]
  y_hat = pred$predict(X)[[desired_class]]
  ps_maker = ParamSetMaker$new(X)
  ps = ps_maker$make_param_set()
  x_interest = X[1L, ]
  desired_range = c(0.5, 1)
  n = 5L
  
  wi_a = WhatIf_Algo$new(pred, n_cores = 1L, param_set = ps, n_cfactuals = n)
  wi_a$run(x_interest, y_hat, desired_range)
  res = wi_a$get_results_list(desired_class)
  
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(X), "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
})

test_that("Returns correct output format for numeric columns only", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred = Predictor$new(rf, type = "prob")
  desired_class = "versicolor"
  X = iris[, -5L]
  y_hat = pred$predict(X)[[desired_class]]
  ps_maker = ParamSetMaker$new(X)
  ps = ps_maker$make_param_set()
  x_interest = X[1L, ]
  desired_range = c(0.5, 1)
  n = 5L
  
  wi_a = WhatIf_Algo$new(pred, n_cores = 1L, param_set = ps, n_cfactuals = n)
  wi_a$run(x_interest, y_hat, desired_range)
  res = wi_a$get_results_list(desired_class)
  
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(X), "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
})


test_that("Returns correct output format for factor and numeric columns", {
  set.seed(54542142)
  df = mtcars
  df$am = as.factor(df$am)
  df$vs = as.factor(df$vs)
  
  rf = randomForest::randomForest(am ~ ., data = df, ntree = 5L)
  pred = Predictor$new(rf, data = df, type = "prob")
  desired_class = "1"
  X = subset(df, select = -am)
  y_hat = pred$predict(X)[[desired_class]]
  
  ps_maker = ParamSetMaker$new(X)
  ps = ps_maker$make_param_set()
  x_interest = X[1L, ]
  desired_range = c(0.5, 1)
  n = 5L
  
  wi_a = WhatIf_Algo$new(pred, n_cores = 1L, param_set = ps, n_cfactuals = n)
  wi_a$run(x_interest, y_hat, desired_range)
  res = wi_a$get_results_list(desired_class)
  
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(X), "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
})

test_that("Parallelization leads to same results as sequential execution", {
  skip_if_not(parallel::detectCores() > 1L)
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred = Predictor$new(rf, type = "prob")
  desired_class = "versicolor"
  X = iris[, -5L]
  y_hat = pred$predict(X)[[desired_class]]
  ps_maker = ParamSetMaker$new(X)
  ps = ps_maker$make_param_set()
  x_interest = X[1L, ]
  desired_range = c(0.5, 1)
  n = 8L
  
  wi_a = WhatIf_Algo$new(pred, n_cores = parallel::detectCores() - 1L, param_set = ps, n_cfactuals = n)
  wi_a$run(x_interest, y_hat, desired_range)
  res_par = wi_a$get_results_list(desired_class)
  
  wi_a = WhatIf_Algo$new(pred, n_cores = 1L, param_set = ps, n_cfactuals = n)
  wi_a$run(x_interest, y_hat, desired_range)
  res_seq = wi_a$get_results_list(desired_class)

  expect_identical(res_par, res_seq)
})


test_that("Throws warning if too few counterfactuals were found and fills $result with NAs", {
  set.seed(54542142)
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf)
  X = subset(mtcars, select = -mpg)
  y_hat = pred$predict(X)[[1L]]
  ps_maker = ParamSetMaker$new(X)
  ps = ps_maker$make_param_set()
  x_interest = mtcars[1L, -1L]
  desired_range = c(15, 17.5)
  
  wi_a = WhatIf_Algo$new(pred, n_cores = 1L, param_set = ps, n_cfactuals = 20L)
  expect_snapshot(wi_a$run(x_interest, y_hat, desired_range))
  
  res = wi_a$get_results_list("pred")[[1L]]
  remaining_rows_are_filled_with_na = all(!complete.cases(res[12:20]))
  expect_true(remaining_rows_are_filled_with_na)
})