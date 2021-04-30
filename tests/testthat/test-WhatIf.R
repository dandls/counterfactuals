library(checkmate)
library(data.table)
library(randomForest)

set.seed(54542142)

iris_sub = iris[c(1:5, 50:55, 100:105), ]
rf = randomForest(Species ~ ., data = iris_sub, ntree = 2L)
mod_class = Predictor$new(rf, iris_sub)

test_that("Returns correct output format for numeric columns only", {
  wi = WhatIf$new(mod_class)
  oneinst = head(subset(iris_sub, select = -Species), 1L)
  n = 3L
  wi$find_counterfactuals(oneinst, "virginica", n = n, n_cores = 1L)
  res = wi$results
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(iris_sub)[-5], "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
  expected_diff = as.data.table(sweep(wi$results$counterfactuals[, 1:4], 2, as.numeric(oneinst)))
  expect_equal(expected_diff, wi$results$counterfactuals_diff[, 1:4])
})


test_that("Returns correct output format for factor columns only", {
  df = mtcars[, c("am", "vs", "cyl")]
  df$am = as.factor(df$am)
  df$vs = as.factor(df$vs)
  df$cyl = as.factor(df$cyl)
  rf <- randomForest(am ~ ., data = df, ntree = 5L)
  mod = Predictor$new(rf, df)
  wi = WhatIf$new(mod)
  oneinst = df[1L, c("vs", "cyl")]
  n = 5L
  wi$find_counterfactuals(oneinst, "0", n = n, n_cores = 1L)
  res = wi$results
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(df)[-1L], "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
})

test_that("Returns correct output format for factor and numeric columns", {
  df = mtcars
  df$am = as.factor(df$am)
  df$vs = as.factor(df$vs)
  rf <- randomForest(am ~ ., data = df, ntree = 5L)
  mod = Predictor$new(rf, df)
  wi = WhatIf$new(mod)
  oneinst = head(subset(df, select = -am), n = 1L)
  n = 5L
  wi$find_counterfactuals(oneinst, "0", n = n, n_cores = 1L)
  res = wi$results
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(oneinst), "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
})


test_that("Parallelization leads to same results as sequential execution", {
  oneinst = head(subset(iris_sub, select = -Species), 1)
  wi = WhatIf$new(mod_class)
  wi$find_counterfactuals(oneinst, "virginica", n = 10L, n_cores = parallel::detectCores() - 1L)
  res_par = wi$results
  wi$find_counterfactuals(oneinst, "virginica", n = 10L, n_cores = 1L)
  res_seq = wi$results
  expect_identical(res_par, res_seq)
})

test_that("Init works for classification tasks only", {
  
  # Regression task
  rf_regr = randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 2L)
  mod_regr = Predictor$new(rf_regr)
  expect_error(WhatIf$new(mod_regr), "only works for classification")

  # Classification task
  expect_error(WhatIf$new(mod_class), NA)

  # The type of the task is inferred using the `inferTaskFromPrediction` from the iml package.
  # Check that possible changes to this function don't break the code.
  pred_regr = mod_regr$predict(mtcars[1:2, ])
  expect_equal(iml:::inferTaskFromPrediction(pred_regr), "regression")
  pred_class = mod_class$predict(iris_sub[1:2, ])
  expect_equal(iml:::inferTaskFromPrediction(pred_class), "classification")
})


