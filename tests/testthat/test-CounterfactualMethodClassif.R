library(randomForest)

# $initialization ------------------------------------------------------------------------------------------------------
test_that("Init works for classification tasks only", {
  set.seed(54542142)

  # Regression task
  rf_regr = get_rf_regr_mtcars()
  pred_regr = iml::Predictor$new(rf_regr)
  expect_error(
    CounterfactualMethodClassif$new(predictor = pred_regr, lower = NULL, upper = NULL), 
    "only works for classification"
  )

  # Classification task
  rf = get_rf_classif_iris()
  pred_class = iml::Predictor$new(rf, type = "class", class = "versicolor")
  expect_error(CounterfactualMethodClassif$new(predictor = pred_class, lower = NULL, upper = NULL), NA)

  # The type of the task is inferred using the `inferTaskFromPrediction` from the iml package.
  # The function is called internally when a Predictor object uses the method `predict` if the task is "unkown".
  # Check that possible changes to this function don't break the code.
  pred_class$task = NULL
  invisible(pred_class$predict(iris[1:2, 1:4]))
  expect_identical(pred_class$task, "classification")
})

# $find_counterfactuals ------------------------------------------------------------------------------------------------
test_that("$find_counterfactuals returns meaningful error if x_interest does not contain all columns of predictor$data$X", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred_class = iml::Predictor$new(rf, type = "class", class = "versicolor")
  cc = CounterfactualMethodClassif$new(predictor = pred_class, lower = NULL, upper = NULL)
  
  expect_snapshot_error(cc$find_counterfactuals(mtcars[1L, ]))
})

test_that("x_interest may contain addtional columns to those of predictor$data$X", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred_class = iml::Predictor$new(rf, type = "class", class = "versicolor")
  cc = CounterfactualMethodClassif$new(predictor = pred_class, lower = NULL, upper = NULL)
  
  # Expect error due to abstract $run method. But for testing purposes this is sufficient here.
  suppressMessages(expect_error(cc$find_counterfactuals(iris[1L, ])))
  expect_data_table(
    cc$.__enclos_env__$private$x_interest, 
    nrows = 1L, types = sapply(iris, class)[-5L], ncols = ncol(iris) - 1L
  )
  expect_names(names(cc$.__enclos_env__$private$x_interest), identical.to = names(iris[, -5L]))
})

test_that("$find_counterfactuals returns meaningful error if x_interest has unexpected column types", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred_class = iml::Predictor$new(rf, type = "class", class = "versicolor")
  cc = CounterfactualMethodClassif$new(predictor = pred_class, lower = NULL, upper = NULL)
  
  x_interest = iris[1L, ]
  x_interest$Sepal.Width = as.character(x_interest$Sepal.Width)
  expect_snapshot_error(cc$find_counterfactuals(x_interest))
})

test_that("$find_counterfactuals returns meaningful error if x_interest already has desired properties", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  pred_class = iml::Predictor$new(rf, type = "class")
  cc = CounterfactualMethodClassif$new(predictor = pred_class, lower = NULL, upper = NULL)
  x_interest = iris[1L, ]
  expect_snapshot_error(cc$find_counterfactuals(x_interest, desired_class = "setosa"))
})

