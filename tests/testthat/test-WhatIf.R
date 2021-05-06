library(checkmate)
library(data.table)

test_that("Returns correct output format for numeric columns only", {
  set.seed(54542142)
  iris_pred = make_iris_predictor()
  n = 3L
  wi = WhatIf$new(iris_pred, n_counterfactuals = n, n_cores = 1L)
  oneinst = head(subset(iris, select = -Species), 1L)
  wi$find_counterfactuals(oneinst, 1)
  res = wi$results
  expect_list(res, len = 2L)
  cfs = res$counterfactuals
  cfs_diff = res$counterfactuals_diff
  expect_data_table(cfs, nrows = n)
  expected_cols = c(colnames(iris)[-5], "dist_x_interest", "pred", "nr_changed")
  expect_true(all(colnames(cfs) == expected_cols))
  expect_data_table(cfs_diff, nrows = n)
  expect_true(all(colnames(cfs_diff) == expected_cols))
  expected_diff = as.data.table(sweep(wi$results$counterfactuals[, 1:4], 2, as.numeric(oneinst)))
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
  oneinst = df2[1L, c("vs", "cyl")]
  wi2$find_counterfactuals(oneinst, 0)
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
  oneinst = head(subset(mydf, select = -am), n = 1L)
  wi$find_counterfactuals(oneinst, 0)
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

test_that("Init works for classification tasks only", {
  set.seed(54542142)

  # Regression task
  rf_regr = randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 2L)
  pred_regr = Predictor$new(rf_regr)
  expect_error(WhatIf$new(pred_regr), "only works for classification")

  # Classification task
  pred_class = make_iris_predictor()
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
  iris_pred = make_iris_predictor()
  oneinst = head(subset(iris, select = -Species), 1)
  wi = WhatIf$new(iris_pred, n_counterfactuals = 10L, n_cores = parallel::detectCores() - 1L)
  wi$find_counterfactuals(oneinst, 1L)
  res_par = wi$results
  wi = WhatIf$new(iris_pred, n_counterfactuals = 10L, n_cores = 1L)
  wi$find_counterfactuals(oneinst, 1L)
  res_seq = wi$results
  expect_identical(res_par, res_seq)
})
