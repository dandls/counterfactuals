library(randomForest)

# $initialize() --------------------------------------------------------------------------------------------------------
test_that("$initialize() returns error if predictor given does not have the correct class", {
  expect_snapshot_error(CounterfactualMethod$new(predictor = "wrong", lower = NULL, upper = NULL))
})

test_that("$initialize() returns error if lower or upper contain names that are not columns in predictor$data$X", {
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf, data = mtcars, y = "mpg")
  expect_snapshot_error(CounterfactualMethod$new(predictor = pred, lower = c("wrong_name" = 2), upper = NULL))
  expect_snapshot_error(CounterfactualMethod$new(predictor = pred, lower = NULL, upper = c("wrong_name" = 2)))
})

test_that("$initialize() returns error if lower is greater than upper", {
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf, data = mtcars, y = "mpg")
  expect_snapshot_error(CounterfactualMethod$new(predictor = pred, lower = c("hp" = 200), upper = c("hp" = 100)))
})

test_that("$initialize() returns error if distance_function is not a function or NULL", {
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf, data = mtcars, y = "mpg")
  expect_error(CounterfactualMethod$new(predictor = pred, distance_function = "a"), 
    "Must be a function")
})
