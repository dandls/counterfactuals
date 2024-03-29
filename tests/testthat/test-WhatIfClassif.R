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
  expect_message(wi_binary$find_counterfactuals(x_interest), "was set to")
  cfactuals_bin = wi_binary$find_counterfactuals(x_interest)
  
  set.seed(54542142)
  iris_pred_multiclass = iml::Predictor$new(rf, type = "prob")
  wi_multiclass = WhatIfClassif$new(iris_pred_multiclass, n_counterfactuals = n)
  cfactuals_mc = wi_multiclass$find_counterfactuals(x_interest, desired_class)

  expect_identical(cfactuals_bin$data, cfactuals_mc$data)
})


test_that("Correct handling of lower and upper", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  iris_pred = iml::Predictor$new(rf, type = "prob")
  n = 5L
  x_interest = iris[1L, ]
  wi = WhatIfClassif$new(
    iris_pred, n_counterfactuals = n, 
    lower = c("Sepal.Length" = 5, "Sepal.Width" = 2.5), upper = c("Sepal.Length" = 6, "Sepal.Width" = 3.5)
  )
  cfactuals = wi$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = c(0.5, 1))
  expect_true(all(between(cfactuals$data$Sepal.Length, 5, 6)))
  expect_true(all(between(cfactuals$data$Sepal.Width, 2.5, 3.5)))
  
  expect_snapshot(
    WhatIfClassif$new(
      iris_pred, n_counterfactuals = 30, 
      lower = c("Sepal.Length" = 5.5), upper = c("Sepal.Length" = 5.7)
    )
  ) 
  
  expect_snapshot(
    WhatIfClassif$new(
      iris_pred, n_counterfactuals = n, 
      lower = c("Sepal.Length" = 0), upper = c("Sepal.Length" = 1)
    )
  ) 
  
  expect_snapshot(
    WhatIfClassif$new(
      iris_pred, n_counterfactuals = n, 
      lower = c("Sepal.Length" = 100), upper = c("Sepal.Length" = 200)
    )
  ) 
  
})


test_that("distance_function can be exchanged", {
  set.seed(54542142)
  rf = get_rf_classif_iris()
  iris_pred = iml::Predictor$new(rf, type = "prob")
  n = 5L
  x_interest = iris[1L, ]
  correct_dist_function = function(x, y, data) {
    res = matrix(NA, nrow = nrow(x), ncol = nrow(y))
    for (i in 1:nrow(x)) for (j in 1:nrow(y)) res[i, j] = sqrt(sum(((x[i, ] - y[j, ])^2)))
    res
  }
  wi = WhatIfClassif$new(
    iris_pred, n_counterfactuals = n, 
    distance_function = correct_dist_function
  )
  cfactuals = wi$find_counterfactuals(x_interest, desired_class = "versicolor", desired_prob = c(0.5, 1))
  expect_data_table(cfactuals$data, nrows = n)
})

test_that("distance_function gower and gower_c return equal results", {
  set.seed(1007)
  rf = randomForest(Species ~ ., data = iris)
  # Create a predictor object
  predictor = iml::Predictor$new(rf, type = "prob")
  # Find counterfactuals for x_interest with gower distance
  wi_classif_g = WhatIfClassif$new(predictor, n_counterfactuals = 2L, distance_function = "gower")
  cfactuals_g = wi_classif_g$find_counterfactuals(
    x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
  )
  # Find counterfactuals for x_interest with gower distance C function
  wi_classif_gc = WhatIfClassif$new(predictor, n_counterfactuals = 2L, distance_function = "gower_c")
  cfactuals_gc = wi_classif_gc$find_counterfactuals(
    x_interest = iris[150L, ], desired_class = "versicolor", desired_prob = c(0.5, 1)
  )
  # Print the results
  expect_true(all(cfactuals_g$data %in% cfactuals_gc$data))
})