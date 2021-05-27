library(randomForest)

# $initialization ------------------------------------------------------------------------------------------------------
test_that("Init works for classification tasks only", {
  set.seed(54542142)

  # Regression task
  rf_regr = get_rf_regr_mtcars()
  pred_regr = iml::Predictor$new(rf_regr)
  expect_error(
    CounterfactualsClassif$new(predictor = pred_regr, lower = NULL, upper = NULL), 
    "only works for classification"
  )

  # Classification task
  rf = get_rf_classif_iris()
  pred_class = iml::Predictor$new(rf, type = "class", class = "versicolor")
  expect_error(CounterfactualsClassif$new(predictor = pred_class, lower = NULL, upper = NULL), NA)

  # The type of the task is inferred using the `inferTaskFromPrediction` from the iml package.
  # The function is called internally when a Predictor object uses the method `predict` if the task is "unkown".
  # Check that possible changes to this function don't break the code.
  pred_class$task = NULL
  invisible(pred_class$predict(iris[1:2, 1:4]))
  expect_identical(pred_class$task, "classification")

})





