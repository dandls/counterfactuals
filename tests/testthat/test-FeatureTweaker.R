library(data.table)

test_that("Initialization returns appropriate error message if not a classification task", {
  set.seed(465465)
  y = rnorm(10)
  X = data.frame(
    col_a = rnorm(10),
    col_b = rnorm(10)
  )
  rf = randomForest::randomForest(X, y, ntree = 5L)
  pred = iml::Predictor$new(rf, data = X, y = y)
  expect_snapshot_error(FeatureTweaker$new(pred, 10L, ktree = 10L))
})

test_that("Initialization returns appropriate error message if randomForest.formula class", {
  set.seed(465465)
  test_df = data.frame(
    col_a = rnorm(10),
    col_b = rnorm(10),
    col_c = rnorm(10)
  )
  rf = randomForest::randomForest(col_c ~ ., data = test_df)
  pred = iml::Predictor$new(rf, data = test_df, y = "col_c")
  expect_snapshot_error(FeatureTweaker$new(pred, 10L, ktree = 10L))
})

test_that("Initialization returns appropriate error message when model is not randomForest", {
  set.seed(465465)
  test_df = data.frame(
    col_a = rnorm(10),
    col_b = rnorm(10),
    col_c = rnorm(10)
  )
  linear_regression = lm(col_c ~ ., data = test_df)
  pred = iml::Predictor$new(linear_regression, data = test_df, y = "col_c")
  expect_snapshot_error(FeatureTweaker$new(pred, 10L, ktree = 10L))
})



