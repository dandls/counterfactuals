make_iris_predictor <- function() {
  rf = randomForest::randomForest(Species ~ ., data = iris, ntree = 2L)
  iml::Predictor$new(rf, type = "class", class = "versicolor")
}