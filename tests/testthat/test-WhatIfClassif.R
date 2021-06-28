library(randomForest)
test_that("Returns correct output format for soft binary classification", {
  set.seed(54542142)
  mydf = mtcars
  mydf$am = as.factor(mydf$am)
  mydf$vs = as.factor(mydf$vs)
  rf = randomForest::randomForest(am ~ ., data = mydf, ntree = 5L)
  pred = Predictor$new(rf, data = mydf, type = "class")
  n = 5L
  wi = WhatIfClassif$new(pred, n_counterfactuals = n)
  x_interest = head(subset(mydf, select = -am), n = 1L)
  cfactuals = wi$find_counterfactuals(x_interest, desired_class = "0")
  
  expect_data_table(cfactuals$data, min.rows = n - 1, max.rows = n, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})

test_that("Returns correct output format for hard binary classification", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  iris_pred = iml::Predictor$new(rf, type = "class")
  n = 3L
  wi = WhatIfClassif$new(iris_pred, n_counterfactuals = n)
  x_interest = iris[1L, -5L]
  cfactuals = wi$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = 1)
  
  expect_data_table(cfactuals$data, nrows = n, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
})

test_that("Can handle non-numeric target classes", {
  set.seed(544564)
  test_data = data.frame(a = rnorm(10), b = rnorm(10), cl = as.factor(rep(c("pos", "neg"), each = 5)))
  rf_pima = randomForest::randomForest(cl ~ . , test_data, ntree = 2L)
  pred = iml::Predictor$new(rf_pima, data = test_data, y = "cl")
  n = 2L
  x_interest = head(subset(test_data, select = -cl), 1L)
  set.seed(544564)
  wi = WhatIfClassif$new(pred, n_counterfactuals = n)
  cfactuals = wi$find_counterfactuals(x_interest, desired_class = "pos")
  
  expect_data_table(cfactuals$data, nrows = n, col.names = "named", types = sapply(x_interest, class))
  expect_names(names(cfactuals$data), identical.to = names(x_interest))
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
  wi_binary = WhatIfClassif$new(iris_pred_binary, n_counterfactuals = n)
  cfactuals_bin = expect_message(wi_binary$find_counterfactuals(x_interest), "was set to")

  set.seed(54542142)
  iris_pred_multiclass = iml::Predictor$new(rf, type = "prob")
  wi_multiclass = WhatIfClassif$new(iris_pred_multiclass, n_counterfactuals = n)
  cfactuals_mc = wi_multiclass$find_counterfactuals(x_interest, desired_class)

  expect_identical(cfactuals_bin$data, cfactuals_mc$data)
})





