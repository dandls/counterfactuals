library(data.table)

test_that("Initialization returns appropriate error message if not a classification task", {
  set.seed(465465)
  y = rnorm(10)
  X = data.frame(
    col_a = rnorm(10),
    col_b = rnorm(10)
  )
  rf = randomForest::randomForest(X, y, ntree = 5L)
  pred = iml::Predictor$new(rf, data = X, y = y)
  expect_snapshot_error(FeatureTweaker$new(pred, 10L, ktree = 10L))
})

test_that("Initialization returns appropriate error message if randomForest.formula class", {
  set.seed(465465)
  test_df = data.frame(
    col_a = rnorm(10),
    col_b = rnorm(10),
    col_c = rnorm(10)
  )
  rf = randomForest::randomForest(col_c ~ ., data = test_df)
  pred = iml::Predictor$new(rf, data = test_df, y = "col_c")
  expect_snapshot_error(FeatureTweaker$new(pred, 10L, ktree = 10L))
})

test_that("Initialization returns appropriate error message when model is not randomForest", {
  set.seed(465465)
  test_df = data.frame(
    col_a = rnorm(10),
    col_b = rnorm(10),
    col_c = rnorm(10)
  )
  linear_regression = lm(col_c ~ ., data = test_df)
  pred = iml::Predictor$new(linear_regression, data = test_df, y = "col_c")
  expect_snapshot_error(FeatureTweaker$new(pred, 10L, ktree = 10L))
})



# Mutliclass classification ------------------------------------------------------------------------
test_that("$find_counterfactuals with specified `desired_outcome` returns the same results as if he `desired_outcome` is set in the iml `Predictor`", {
  
  set.seed(54542142)
  X = iris[, 1:(ncol(iris) - 1)]
  true_y = iris[, ncol(iris)]
  rf = randomForest::randomForest(X, true_y, ntree = 20L)
  x_interest = X[10L, ]
  n = 2L
  desired_outcome = "versicolor"
  
  set.seed(354645)
  iris_pred_bin = iml::Predictor$new(rf, data = iris, y = "Species")
  ft = FeatureTweaker$new(iris_pred_bin, n_counterfactuals = n, ktree = 5L)
  ft$find_counterfactuals(x_interest, desired_outcome = desired_outcome)
  res_binary = ft$results$counterfactuals
  
  set.seed(354645)
  iris_pred_bin = iml::Predictor$new(rf, data = iris, y = "Species", class = desired_outcome)
  ft = FeatureTweaker$new(iris_pred_bin, n_counterfactuals = n, ktree = 5L)
  ft$find_counterfactuals(x_interest)
  res_multiclass = ft$results$counterfactuals

  # For binary pred contains 0/1 (if target is not a factor) and for multiclass the classname
  res_binary$pred = res_multiclass$pred = NULL
  expect_identical(res_binary, res_multiclass)
})


test_that("`desired_outcome` is required for multiclass", {
  X = iris[, 1:(ncol(iris) - 1)]
  true_y = iris[, ncol(iris)]
  rf = randomForest::randomForest(X, true_y, ntree = 20L)
  x_interest = X[10L, ]
  n = 2L
  iris_pred_multiclass = iml::Predictor$new(rf, data = iris, y = "Species")
  ft_multiclass = WhatIf$new(iris_pred_multiclass, n_counterfactuals = n, n_cores = 1L)
  expect_snapshot_error(ft_multiclass$find_counterfactuals(x_interest))
})

