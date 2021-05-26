library(randomForest)

test_that("Returns correct output format for mixed columns", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  mydf$cyl = as.factor(mydf$cyl)
  
  rf = randomForest(mpg ~ ., data = mydf, ntree = 5L)
  pred = iml::Predictor$new(rf, data = mydf, y = "mpg")
  n = 3L
  wi = WhatIfRegr$new(pred, n_counterfactuals = n, n_cores = 1L)
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






