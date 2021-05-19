library(randomForest)
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


test_that("Init works for classification tasks only", {
  set.seed(54542142)
  
  # Regression task
  rf_regr = get_rf_regr_mtcars()
  pred_regr = iml::Predictor$new(rf_regr)
  param_list = list(predictor = pred_regr)
  expect_error(CounterfactualsClassification$new(param_list), "only works for classification")
  
  # Classification task
  rf = get_rf_classif_iris()
  pred_class = iml::Predictor$new(rf, type = "class", class = "versicolor")
  param_list = list(predictor = pred_class)
  expect_error(CounterfactualsClassification$new(param_list), NA)
  
  # The type of the task is inferred using the `inferTaskFromPrediction` from the iml package.
  # The function is called internally when a Predictor object uses the method `predict` if the task is "unkown".
  # Check that possible changes to this function don't break the code.
  pred_class$task = NULL
  invisible(pred_class$predict(iris[1:2, 1:4]))
  expect_identical(pred_class$task, "classification")
  
})