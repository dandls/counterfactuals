library(randomForest)

# $initialize() --------------------------------------------------------------------------------------------------------
test_that("$initialize() returns error if predictor given does not have the correct class", {
  expect_snapshot_error(CounterfactualMethod$new(predictor = "wrong", lower = NULL, upper = NULL))
})
