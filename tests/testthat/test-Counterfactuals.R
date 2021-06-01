library(randomForest)

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
      var_1 = ParamDbl$new(id = "id1", lower = -5, upper = 5),
      var_2 = ParamInt$new(id = "id2", lower = 0, upper = 5),
      var_3 = ParamFct$new(id = "id3", levels = levels(dt$var_3))
  ))
  
  cf = Counterfactuals$new(X, mod$prediction.function, x_interest, ps, desired = list(desired_outcome = 67.2))
  expect_identical(cf$predict(), mod$prediction.function(X))
})





