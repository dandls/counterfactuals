library(randomForest)

# $initialize() --------------------------------------------------------------------------------------------------------
test_that("$initialize() returns error if predictor given does not have the correct class", {
  expect_snapshot_error(CounterfactualMethod$new(predictor = "wrong", lower = NULL, upper = NULL))
})

# $private$plot_surface --------------------------------------------------------------------------------------------
test_that("$plot_surface() returns error message if `feature_names` are not in data", {
  set.seed(54654654)
  train_data = data.frame(
    col_a = rep(c(1, 3), 6L),
    col_b = rep(1:3, each = 4),
    col_c = rep(c("x", "y", "z"), each = 2),
    col_d = as.factor(c(rep("a", 4L), rep("b", 4L), rep("c", 4L)))
  )
  x_interest = data.table(col_a = 2, col_b = 1, col_c = "y")
  
  rf = randomForest(col_d ~ ., data = train_data)
  mod = Predictor$new(rf, data = train_data, type = "class", class = "b")
  
  ci = CounterfactualMethod$new(predictor = mod, lower = NULL, upper = NULL)
  expect_snapshot_error(ci$plot_surface(c("not_in_data", "col_b")))
})

