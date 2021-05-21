library(randomForest)

# $initialization ------------------------------------------------------------------------------------------------------
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

# $check_desired_class -------------------------------------------------------------------------------------------------------
test_that("`desired_class` needs to be in the prediction columns", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred = Predictor$new(rf)
  param_list = list(predictor = pred)
  cc = CounterfactualsClassification$new(param_list)
  cc$.__enclos_env__$private$y_hat_interest = pred$predict(iris[1L, ])
  expect_snapshot_error(cc$.__enclos_env__$private$check_desired_class("wrong_column"))
})

test_that("`desired_class` is required for multiclass", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred = Predictor$new(rf)
  param_list = list(predictor = pred)
  cc = CounterfactualsClassification$new(param_list)
  expect_snapshot_error(cc$find_counterfactuals(x_interest = iris[1L, -5L]))
})

# $check_desired_prob --------------------------------------------------------------------------------------------------
test_that("$check_desired_prob returns error message if desired_outcome has incorrect formats", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred = Predictor$new(rf)
  param_list = list(predictor = pred)
  cc = CounterfactualsClassification$new(param_list)
  expect_error(cc$.__enclos_env__$private$check_desired_prob(c("a", "b")), "Must be of type")
  expect_error(cc$.__enclos_env__$private$check_desired_prob(1:3), "Must have length")
  expect_error(cc$.__enclos_env__$private$check_desired_prob(NA), "missing")
  expect_snapshot_error(cc$.__enclos_env__$private$check_desired_prob(c(0.8, 0.2)))
})


