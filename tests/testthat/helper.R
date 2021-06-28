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

save_test_png = function(code, width = 400, height = 400) {
  path = tempfile(fileext = ".png")
  cowplot::save_plot(path, code)
  path
}


make_counterfactual_test_obj = function() {
  set.seed(45748)
  dt = data.table(
    var_num_1 = rep(c(0.5, 5.3)), 
    var_num_2 = rep(c(1.5, 2.7)),
    var_fact_1 = as.factor(sample(c("a", "b", "c"), size = 10L, replace = TRUE)), 
    var_fact_2 = as.factor(sample(c("e", "f", "g"), size = 10L, replace = TRUE)), 
    var_target = rnorm(10L, mean = 50, sd = 10)
  )
  X = dt[, 1:(ncol(dt) - 1L)]
  x_interest = X[1L, ]
  rf = randomForest(var_target ~ ., data = dt)
  mod = Predictor$new(rf, data = X)
  ps = ParamSet$new(list(
    var_num_1 = ParamDbl$new(id = "var_num_1", lower = -5, upper = 5),
    var_num_2 = ParamDbl$new(id = "var_num_2", lower = 0, upper = 10),
    var_fact_1 = ParamFct$new(id = "var_fact_1", levels = levels(dt$var_fact_1)),
    var_fact_2 = ParamFct$new(id = "var_fact_2", levels = levels(dt$var_fact_2))
  ))
  
  cf = Counterfactuals$new(as.data.table(X), mod, x_interest, ps, desired = list(desired_outcome = c(42, 44)))
  cf
}



quiet = function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
} 
