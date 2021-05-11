get_scaled_iris_features = function() {
  as.data.frame(scale(iris[, -5]))
}
