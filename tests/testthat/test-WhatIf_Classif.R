library(checkmate)
library(data.table)


# Soft classification ----------------------------------------------------------------------------
test_that("Returns correct output format for factor columns only", {
  set.seed(54542142)
  mydf = mtcars[, c("am", "vs", "cyl")]
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  mydf$cyl = as.factor(mydf$cyl)
  rf = randomForest::randomForest(am ~ ., data = mydf, ntree = 5L)
  pred = Predictor$new(rf, data = mydf, type = "prob")
  n = 5L
  wi2 = WhatIf_Classif$new(pred, n_counterfactuals = n, n_cores = 1L)
  x_interest = mydf[1L, c("vs", "cyl")]
  wi2$find_counterfactuals(x_interest, desired_class = "1")
  res = wi2$results
  
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(mydf)[-1L], "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
})

test_that("Returns correct output format for numeric columns only", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  iris_pred = iml::Predictor$new(rf, type = "prob")
  n = 3L
  wi = WhatIf_Classif$new(iris_pred, n_counterfactuals = n, n_cores = 1L)
  x_interest = iris[1L, -5L]
  desired_prob = c(0.5, 0.8)
  wi$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = desired_prob)
  res = wi$results

  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(iris)[-5L], "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
  expect_numeric(cfs$pred, lower = desired_prob[1], upper = desired_prob[2L])
  expected_diff = as.data.table(sweep(wi$results$counterfactuals[, 1:4], 2L, as.numeric(x_interest)))
  expect_equal(expected_diff, wi$results$counterfactuals_diff[, 1:4])
})


test_that("Returns correct output format for factor and numeric columns", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  rf = randomForest::randomForest(am ~ ., data = mydf, ntree = 5L)
  pred = Predictor$new(rf, data = mydf, type = "class")
  n = 5L
  wi = WhatIf_Classif$new(pred, n_counterfactuals = n, n_cores = 1L)
  x_interest = head(subset(mydf, select = -am), n = 1L)
  wi$find_counterfactuals(x_interest, desired_class = "1")
  res = wi$results
  
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(x_interest), "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
})

test_that("Init works for classification tasks only", {
  set.seed(54542142)

  # Regression task
  rf_regr = get_rf_regr_mtcars()
  pred_regr = Predictor$new(rf_regr)
  expect_error(WhatIf_Classif$new(pred_regr), "only works for classification")

  # Classification task
  rf = get_rf_classif_iris()
  pred_class = iml::Predictor$new(rf, type = "class", class = "versicolor")
  expect_error(WhatIf_Classif$new(pred_class), NA)

  # The type of the task is inferred using the `inferTaskFromPrediction` from the iml package.
  # The function is called internally when a Predictor object uses the method `predict`
  # Check that possible changes to this function don't break the code.
  invisible(pred_regr$predict(mtcars[1:2, -which(colnames(mtcars) == "mpg")]))
  expect_identical(pred_regr$task, "regression")

  invisible(pred_class$predict(iris[1:2, 1:4]))
  expect_identical(pred_class$task, "classification")

})

test_that("Parallelization leads to same results as sequential execution", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  iris_pred = iml::Predictor$new(rf, type = "class")
  x_interest = head(subset(iris, select = -Species), 1)
  wi = WhatIf_Classif$new(iris_pred, n_counterfactuals = 10L, n_cores = parallel::detectCores() - 1L)
  wi$find_counterfactuals(x_interest, desired_class = "versicolor")
  res_par = wi$results
  wi = WhatIf_Classif$new(iris_pred, n_counterfactuals = 10L, n_cores = 1L)
  wi$find_counterfactuals(x_interest, desired_class = "versicolor")
  res_seq = wi$results
  expect_identical(res_par, res_seq)
})

test_that("Can handle non-numeric target classes", {
  set.seed(544564)
  test_data = data.frame(a = rnorm(10), b = rnorm(10), cl = as.factor(rep(c("pos", "neg"), each = 5)))
  rf_pima = randomForest::randomForest(cl ~ . , test_data, ntree = 2L)
  pred = iml::Predictor$new(rf_pima, data = test_data, y = "cl")
  n = 2L
  x_interest = head(subset(test_data, select = -cl), 1L)
  set.seed(544564)
  wi = WhatIf_Classif$new(pred, n_counterfactuals = n, n_cores = 1L)
  wi$find_counterfactuals(x_interest, desired_class = "pos")
  expect_data_table(wi$results$counterfactuals, nrows = n)
})


test_that("$find_counterfactuals with specified `desired_outcome` returns the same results as if the `desired_outcome`
          is set in the iml `Predictor`", {

  set.seed(54542142)
  rf = get_rf_classif_iris()
  n = 3L
  x_interest = head(subset(iris, select = -Species), 1L)
  desired_class = "versicolor"

  set.seed(54542142)
  iris_pred_binary = iml::Predictor$new(rf, type = "prob", class = desired_class)
  wi_binary = WhatIf_Classif$new(iris_pred_binary, n_counterfactuals = n, n_cores = 1L)
  expect_message(wi_binary$find_counterfactuals(x_interest), "was set to")

  set.seed(54542142)
  iris_pred_multiclass = iml::Predictor$new(rf, type = "prob")
  wi_multiclass = WhatIf_Classif$new(iris_pred_multiclass, n_counterfactuals = n, n_cores = 1L)
  wi_multiclass$find_counterfactuals(x_interest, desired_class)

  res_binary = wi_binary$results$counterfactuals
  res_multiclass = wi_multiclass$results$counterfactuals

  expect_identical(res_binary, res_multiclass)
})


test_that("`desired_class` is required for multiclass", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  n = 3L
  x_interest = head(subset(iris, select = -Species), 1L)
  iris_pred_multiclass = iml::Predictor$new(rf)
  wi_multiclass = WhatIf_Classif$new(iris_pred_multiclass, n_counterfactuals = n, n_cores = 1L)
  suppressMessages(expect_snapshot_error(wi_multiclass$find_counterfactuals(x_interest)))
})

test_that("`desired_class` needs to be in the prediction columns", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  n = 3L
  x_interest = head(subset(iris, select = -Species), 1L)
  iris_pred_multiclass = iml::Predictor$new(rf)
  wi_multiclass = WhatIf_Classif$new(iris_pred_multiclass, n_counterfactuals = n, n_cores = 1L)
  expect_snapshot_error(wi_multiclass$find_counterfactuals(x_interest, desired_class = "wrong"))
})


# Hard binary classification ----------------------------------------------------------------------------
test_that("Returns correct output format for numeric columns only for hard binary classification", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  iris_pred = iml::Predictor$new(rf, type = "class")
  n = 3L
  wi = WhatIf_Classif$new(iris_pred, n_counterfactuals = n, n_cores = 1L)
  x_interest = iris[1L, -5L]
  wi$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = 1)
  res = wi$results
  
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(iris)[-5], "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
  expected_diff = as.data.table(sweep(wi$results$counterfactuals[, 1:4], 2, as.numeric(x_interest)))
  expect_equal(expected_diff, wi$results$counterfactuals_diff[, 1:4])
})


