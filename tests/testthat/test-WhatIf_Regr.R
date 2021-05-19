library(checkmate)
library(data.table)
library(randomForest)

test_that("Returns correct output format for factor columns only", {
  set.seed(54542142)
  mydf = mtcars[, c("mpg", "vs", "cyl", "am")]
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  mydf$cyl = as.factor(mydf$cyl)
  rf = randomForest(mpg ~ ., data = mydf, ntree = 5L)
  pred = iml::Predictor$new(rf, data = mydf)
  n = 5L
  wi = WhatIf_Regr$new(pred, n_counterfactuals = n, n_cores = 1L)
  x_interest = head(subset(mydf, select = -mpg), 1)
  desired_outcome = c(15, 18)
  wi$find_counterfactuals(x_interest, desired_outcome)
  res = wi$results
  
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(mydf)[-1L], "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_numeric(cfs$pred, lower = desired_outcome[1L], upper = desired_outcome[2L])
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
})

test_that("Returns correct output format for mixed columns", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  mydf$cyl = as.factor(mydf$cyl)
  
  rf = randomForest(mpg ~ ., data = mydf, ntree = 5L)
  pred = iml::Predictor$new(rf, data = mydf, y = "mpg")
  n = 3L
  wi = WhatIf_Regr$new(pred, n_counterfactuals = n, n_cores = 1L)
  x_interest = head(subset(mydf, select = -mpg), 1)
  desired_outcome = c(15, 18)
  wi$find_counterfactuals(x_interest, desired_outcome)
  res = wi$results
  
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(mydf)[-1L], "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
  expect_numeric(cfs$pred, lower = desired_outcome[1], upper = desired_outcome[2L])
  num_feature_cols = c(2:6, 9:10)
  expected_diff = as.data.table(sweep(cfs[, ..num_feature_cols], 2L, as.numeric(x_interest[, ..num_feature_cols])))
  expect_equal(expected_diff, cfs_diff[, ..num_feature_cols])
})

test_that("Returns correct output format for numeric columns only", {
  set.seed(54542142)
  rf = get_rf_regr_mtcars()
  mtcars_pred = iml::Predictor$new(rf)
  n = 3L
  wi = WhatIf_Regr$new(mtcars_pred, n_counterfactuals = n, n_cores = 1L)
  x_interest = head(subset(mtcars, select = -mpg), 1)
  desired_outcome = c(12, 16)
  wi$find_counterfactuals(x_interest, desired_outcome)
  res = wi$results
  
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(mtcars)[-1L], "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
  expect_numeric(cfs$pred, lower = desired_outcome[1L], upper = desired_outcome[2L])
  expected_diff = as.data.table(sweep(cfs[, 1:10], 2, as.numeric(x_interest)))
  expect_equal(expected_diff, cfs_diff[, 1:10])
})

test_that("Parallelization leads to same results as sequential execution", {
  set.seed(54542142)
  rf = get_rf_regr_mtcars()
  pred = iml::Predictor$new(rf, type = "class")
  x_interest = head(subset(mtcars, select = -mpg), 1)
  desired_outcome = c(12, 16)
  wi = WhatIf_Regr$new(pred, n_counterfactuals = 10L, n_cores = parallel::detectCores() - 1L)
  wi$find_counterfactuals(x_interest, desired_outcome)
  res_par = wi$results
  wi = WhatIf_Regr$new(pred, n_counterfactuals = 10L, n_cores = 1L)
  wi$find_counterfactuals(x_interest, desired_outcome)
  res_seq = wi$results
  expect_identical(res_par, res_seq)
})



