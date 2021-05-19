library(randomForest)

test_that("Init works for regression tasks only", {
  set.seed(54542142)
  
  # TODO
  # Regression task
  rf_regr = get_rf_regr_mtcars()
  pred_regr = Predictor$new(rf_regr)
  expect_error(WhatIf_Regr$new(pred_regr), NA)

  # Classification task
  rf = get_rf_classif_iris()
  pred_class = iml::Predictor$new(rf, type = "class", class = "versicolor")
  expect_error(WhatIf_Regr$new(pred_class), "only works for regression")
  
  # The type of the task is inferred using the `inferTaskFromPrediction` from the iml package.
  # The function is called internally when a Predictor object uses the method `predict` if the task is "unkown".
  # Check that possible changes to this function don't break the code.
  pred_regr$task = NULL
  invisible(pred_regr$predict(mtcars[1:2, -which(colnames(mtcars) == "mpg")]))
  expect_identical(pred_regr$task, "regression")
  
})