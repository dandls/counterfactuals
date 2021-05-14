test_that("Returns correct output format for mixed columns", {
  set.seed(54542142)
  rf = randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 20L)
  mtcars_pred = iml::Predictor$new(rf)
  n = 3L
  wi = WhatIf_Regr$new(mtcars_pred, n_counterfactuals = n, n_cores = 1L)
  x_interest = mtcars[1L, -1L]
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
  expected_diff = as.data.table(sweep(wi$results$counterfactuals[, 1:10], 2, as.numeric(x_interest)))
  expect_equal(expected_diff, wi$results$counterfactuals_diff[, 1:10])
})
