library(randomForest)

# $initialization ------------------------------------------------------------------------------------------------------
test_that("Init works for regression tasks only", {
  set.seed(54542142)
  
  # Regression task
  rf_regr = get_rf_regr_mtcars()
  pred_regr = Predictor$new(rf_regr)
  expect_error(CounterfactualsRegr$new(predictor = pred_regr, lower = NULL, upper = NULL), NA)

  # Classification task
  rf_classif = get_rf_classif_iris()
  pred_classif = iml::Predictor$new(rf_classif, type = "class", class = "versicolor")
  expect_error(CounterfactualsRegr$new(predictor = pred_classif, lower = NULL, upper = NULL), "only works for regression")
  
  # The type of the task is inferred using the `inferTaskFromPrediction` from the iml package.
  # The function is called internally when a Predictor object uses the method `predict` if the task is "unkown".
  # Check that possible changes to this function don't break the code.
  pred_regr$task = NULL
  invisible(pred_regr$predict(mtcars[1:2, -which(colnames(mtcars) == "mpg")]))
  expect_identical(pred_regr$task, "regression")
  
})

# $find_counterfactuals ------------------------------------------------------------------------------------------------
test_that("$find_counterfactuals returns meaningful error if x_interest does not contain all columns of predictor$data$X", {
  set.seed(54542142)
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf)
  cr = CounterfactualsRegr$new(predictor = pred, lower = NULL, upper = NULL)
  
  expect_snapshot_error(cr$find_counterfactuals(iris[1L, ]))
})

test_that("x_interest may contain addtional columns to those of predictor$data$X", {
  set.seed(54542142)
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf)
  cr = CounterfactualsRegr$new(predictor = pred, lower = NULL, upper = NULL)
  
  # Expect error due to abstract $run method. But for testing purposes this is sufficient here.
  suppressMessages(expect_error(cr$find_counterfactuals(mtcars[1L, ], c(0, 10))))
  expect_data_table(
    cr$.__enclos_env__$private$x_interest, 
    nrows = 1L, types = sapply(mtcars, class)[-1L], ncols = ncol(mtcars) - 1L
  )
  expect_names(names(cr$.__enclos_env__$private$x_interest), identical.to = names(mtcars[, -1L]))
})

test_that("$find_counterfactuals returns meaningful error if x_interest has unexpected column types", {
  set.seed(54542142)
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf)
  cr = CounterfactualsRegr$new(predictor = pred, lower = NULL, upper = NULL)
  
  x_interest = mtcars[1L, ]
  x_interest$disp = as.factor(x_interest$disp)
  expect_snapshot_error(cr$find_counterfactuals(x_interest))
})

