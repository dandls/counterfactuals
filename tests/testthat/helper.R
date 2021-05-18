get_rf_regr_mtcars = function() {
  file_path = "test_files/rf_regr_mtcars.RDS"
  if (!file.exists(file_path)) {
    if (!dir.exists("test_files")) {
      dir.create("test_files")
    }
    rf = randomForest::randomForest(mpg ~ ., data = mtcars, ntree = 5L)
    saveRDS(rf, file = file_path) 
  }
  readRDS(file_path)
}

get_rf_classif_iris = function() {
  file_path = "test_files/rf_classif_iris.RDS"
  if (!file.exists(file_path)) {
    if (!dir.exists("test_files")) {
      dir.create("test_files")
    }
    rf = randomForest::randomForest(Species ~ ., data = iris, ntree = 20L)
    saveRDS(rf, file = file_path) 
  }
  readRDS(file_path)
}