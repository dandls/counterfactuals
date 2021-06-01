library(randomForest)

# $predict() -----------------------------------------------------------------------------------------------------------
test_that("$predict method returns correct prediction", {
  set.seed(45748)
  dt = data.table(
    var_1 = rnorm(10L), var_2 = rbinom(10L, 5L, 0.5), var_3 = as.factor(rbinom(10L, 2L, 0.2)), 
    var_4 = rnorm(10L, mean = 50, sd = 10)
  )
  X = dt[, 1:3]
  x_interest = X[1L, ]
  rf = randomForest(var_4 ~ ., data = dt)
  mod = Predictor$new(rf, data = X)
  ps = ParamSet$new(list(
    var_1 = ParamDbl$new(id = "var_1", lower = -5, upper = 5),
    var_2 = ParamInt$new(id = "var_2", lower = 0, upper = 5),
    var_3 = ParamFct$new(id = "var_3", levels = levels(dt$var_3))
  ))
  
  cf = Counterfactuals$new(
    X, mod$prediction.function, x_interest, ps, desired = list(desired_outcome = 67.2), "regression"
  )
  expect_identical(cf$predict(), mod$prediction.function(X))
})

# $get_freq_of_feature_changes() ---------------------------------------------------------------------------------------
test_that("$get_freq_of_feature_changes returns correct frequencies", {
  set.seed(45748)
  dt = data.table(
    var_1 = rnorm(10L), var_2 = rbinom(10L, 5L, 0.5), var_3 = as.factor(rbinom(10L, 2L, 0.2)), 
    var_4 = rnorm(10L, mean = 50, sd = 10)
  )
  X = dt[, 1:3]
  x_interest = X[1L, ]
  rf = randomForest(var_4 ~ ., data = dt)
  mod = Predictor$new(rf, data = X)
  ps = ParamSet$new(list(
    var_1 = ParamDbl$new(id = "var_1", lower = -5, upper = 5),
    var_2 = ParamInt$new(id = "var_2", lower = 0, upper = 5),
    var_3 = ParamFct$new(id = "var_3", levels = levels(dt$var_3))
  ))
  
  cf = Counterfactuals$new(
    as.data.table(X), mod$prediction.function, x_interest, ps, desired = list(desired_outcome = 67.2), "regression"
  )
  freq = cf$get_freq_of_feature_changes()
  expect_equal(freq[["var_1"]], 1 - mean(x_interest[["var_1"]] == dt[["var_1"]]))
  expect_equal(freq[["var_2"]], 1 - mean(x_interest[["var_2"]] == dt[["var_2"]]))
  expect_equal(freq[["var_3"]], 1 - mean(x_interest[["var_3"]] == dt[["var_3"]]))
})

# $plot_freq_of_feature_changes() ---------------------------------------------------------------------------------------
test_that("$plot_freq_of_feature_changes() creates correct plot", {
  skip_on_ci()
  set.seed(45748)
  dt = data.table(
    var_1 = rnorm(10L), var_2 = rbinom(10L, 5L, 0.5), var_3 = as.factor(rbinom(10L, 2L, 0.2)), 
    var_4 = rnorm(10L, mean = 50, sd = 10)
  )
  X = dt[, 1:3]
  x_interest = X[1L, ]
  rf = randomForest(var_4 ~ ., data = dt)
  mod = Predictor$new(rf, data = X)
  ps = ParamSet$new(list(
    var_1 = ParamDbl$new(id = "var_1", lower = -5, upper = 5),
    var_2 = ParamInt$new(id = "var_2", lower = 0, upper = 5),
    var_3 = ParamFct$new(id = "var_3", levels = levels(dt$var_3))
  ))
  
  cf = Counterfactuals$new(
    as.data.table(X), mod$prediction.function, x_interest, ps, desired = list(desired_outcome = 67.2), "regression"
  )
  
  expect_snapshot_file(
    save_test_png(cf$plot_freq_of_feature_changes()), 
    "plot_freq_of_feature_changes_zero.png"
  )
})


# $plot_surface() ---------------------------------------------------------------------------------------
test_that("plot_surface creates correct plot", {
  skip_on_ci()
  set.seed(45748)
  dt = data.table(
    var_1 = rnorm(10L), var_2 = rbinom(10L, 5L, 0.5), var_3 = as.factor(rbinom(10L, 2L, 0.2)), 
    var_4 = rnorm(10L, mean = 50, sd = 10)
  )
  X = dt[, 1:3]
  x_interest = X[1L, ]
  rf = randomForest(var_4 ~ ., data = dt)
  mod = Predictor$new(rf, data = X)
  ps = ParamSet$new(list(
    var_1 = ParamDbl$new(id = "var_1", lower = -5, upper = 5),
    var_2 = ParamInt$new(id = "var_2", lower = 0, upper = 5),
    var_3 = ParamFct$new(id = "var_3", levels = levels(dt$var_3))
  ))
  
  cf = Counterfactuals$new(
    as.data.table(X), mod$prediction.function, x_interest, ps, desired = list(desired_outcome = 67.2), "regression"
  )
  
  expect_snapshot_file(
    save_test_png(cf$plot_surface(names(x_interest)[1:2])), 
    "plot_surface_all_numeric.png"
  )
})
