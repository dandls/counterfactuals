library(checkmate)
library(data.table)


# Binary classification ----------------------------------------------------------------------------
test_that("Returns correct output format for numeric columns only", {
  set.seed(54542142)
  rf = randomForest::randomForest(Species ~ ., data = iris, ntree = 2L)
  iris_pred = iml::Predictor$new(rf, type = "class", class = "versicolor")
  n = 3L
  wi = WhatIf$new(iris_pred, n_counterfactuals = n, n_cores = 1L)
  x_interest = head(subset(iris, select = -Species), 1L)
  wi$find_counterfactuals(x_interest)
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

test_that("Returns correct output format for factor columns only", {
  set.seed(54542142)
  df2 = mtcars[, c("am", "vs", "cyl")]
  df2$am = as.factor(df2$am)
  df2$vs = as.factor(df2$vs)
  df2$cyl = as.factor(df2$cyl)
  rf = randomForest::randomForest(am ~ ., data = df2, ntree = 5L)
  pred = Predictor$new(rf, data = df2, type = "class")
  n = 5L
  wi2 = WhatIf$new(pred, n_counterfactuals = n, n_cores = 1L)
  x_interest = df2[1L, c("vs", "cyl")]
  wi2$find_counterfactuals(x_interest)
  res = wi2$results
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(df2)[-1L], "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
})

test_that("Returns correct output format for factor and numeric columns", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  rf = randomForest::randomForest(am ~ ., data = mydf, ntree = 5L)
  pred = Predictor$new(rf, data = mydf, type = "class")
  n = 5L
  wi = WhatIf$new(pred, n_counterfactuals = n, n_cores = 1L)
  x_interest = head(subset(mydf, select = -am), n = 1L)
  wi$find_counterfactuals(x_interest)
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
  rf_regr = randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 2L)
  pred_regr = Predictor$new(rf_regr)
  expect_error(WhatIf$new(pred_regr), "only works for classification")

  # Classification task
  rf = randomForest::randomForest(Species ~ ., data = iris, ntree = 2L)
  pred_class = iml::Predictor$new(rf, type = "class", class = "versicolor")
  expect_error(WhatIf$new(pred_class), NA)

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
  rf = randomForest::randomForest(Species ~ ., data = iris, ntree = 2L)
  iris_pred = iml::Predictor$new(rf, type = "class", class = "versicolor")
  x_interest = head(subset(iris, select = -Species), 1)
  wi = WhatIf$new(iris_pred, n_counterfactuals = 10L, n_cores = parallel::detectCores() - 1L)
  wi$find_counterfactuals(x_interest)
  res_par = wi$results
  wi = WhatIf$new(iris_pred, n_counterfactuals = 10L, n_cores = 1L)
  wi$find_counterfactuals(x_interest)
  res_seq = wi$results
  expect_identical(res_par, res_seq)
})


test_that("If `desired_outcome` is specified for binary class, it is set to the opposite of `x_interest`", {
  rf = randomForest::randomForest(Species ~ ., data = iris, ntree = 2L)
  iris_pred = iml::Predictor$new(rf, type = "class", class = "versicolor")
  n = 3L
  x_interest = head(subset(iris, select = -Species), 1L)
  set.seed(544564)
  wi = WhatIf$new(iris_pred, n_counterfactuals = n, n_cores = 1L)
  expect_message(wi$find_counterfactuals(x_interest, 0), "opposite class")
  expect_identical(wi$.__enclos_env__$private$desired_outcome, 1)
  res1 = wi$results
  set.seed(544564)
  wi = WhatIf$new(iris_pred, n_counterfactuals = n, n_cores = 1L)
  wi$find_counterfactuals(x_interest)
  res2 = wi$results
  expect_identical(res1, res2)
})

test_that("Can handle non-numeric classes", {
  set.seed(544564)
  test_data = data.frame(a = rnorm(10), b = rnorm(10), cl = as.factor(rep(c("pos", "neg"), each = 5)))
  rf_pima = randomForest::randomForest(cl ~ . , test_data, ntree = 2L)
  pred = iml::Predictor$new(rf_pima, data = test_data, y = "cl")
  n = 3L
  x_interest = head(subset(test_data, select = -cl), 1L)
  set.seed(544564)
  wi = WhatIf$new(pred, n_counterfactuals = n, n_cores = 1L)
  wi$find_counterfactuals(x_interest)
  expect_data_table(wi$results$counterfactuals, nrows = n)
})


# Mutliclass classification ------------------------------------------------------------------------
test_that("$find_counterfactuals with specified `desired_outcome` returns the same results as if he `desired_outcome` is set in the iml `Predictor`", {
 
  set.seed(54542142)
  rf = randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  n = 3L
  x_interest = head(subset(iris, select = -Species), 1L)
  desired_outcome = "versicolor"
  
  set.seed(54542142)
  iris_pred_binary = iml::Predictor$new(rf, type = "class", class = desired_outcome)
  wi_binary = WhatIf$new(iris_pred_binary, n_counterfactuals = n, n_cores = 1L)
  wi_binary$find_counterfactuals(x_interest)
  
  set.seed(54542142)
  iris_pred_multiclass = iml::Predictor$new(rf, type = "class")
  wi_multiclass = WhatIf$new(iris_pred_multiclass, n_counterfactuals = n, n_cores = 1L)
  wi_multiclass$find_counterfactuals(x_interest, desired_outcome)
  
  res_binary = wi_binary$results$counterfactuals
  res_multiclass = wi_multiclass$results$counterfactuals
  
  # For binary pred contains 0/1 and for multiclass the classname
  res_binary$pred = res_multiclass$pred = NULL
  expect_identical(res_binary, res_multiclass)
})


test_that("`desired_outcome` is required for multiclass", {
  set.seed(54542142)
  rf = randomForest::randomForest(Species ~ ., data = iris, ntree = 100L)
  n = 3L
  x_interest = head(subset(iris, select = -Species), 1L)
  iris_pred_multiclass = iml::Predictor$new(rf, type = "class")
  wi_multiclass = WhatIf$new(iris_pred_multiclass, n_counterfactuals = n, n_cores = 1L)
  expect_snapshot_error(wi_multiclass$find_counterfactuals(x_interest))
})





