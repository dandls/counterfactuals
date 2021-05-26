library(randomForest)

# $initialize() --------------------------------------------------------------------------------------------------------
test_that("$initialize() returns error if no predictor is specified", {
  expect_snapshot_error(Counterfactuals$new(list()))
})

test_that("$initialize() returns error if predictor given does not have the correct class", {
  expect_snapshot_error(Counterfactuals$new(list(predictor = "wrong")))
})

# $private$check_x_interest --------------------------------------------------------------------------------------------
test_that("$private$check_x_interest() returns error if x_interest column do not match X of predictor", {
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf)
  arg_list = list(predictor = pred)
  ci = Counterfactuals$new(arg_list)
  expect_snapshot_error(ci$.__enclos_env__$private$check_x_interest(iris[1L, ]))
})

test_that("$private$check_x_interest() returns error if x_interest column types do not match types of X of predictor", {
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf)
  arg_list = list(predictor = pred)
  ci = Counterfactuals$new(arg_list)
  x_interest = head(subset(mtcars, select = -mpg), 1L)
  x_interest$am = as.factor(x_interest$am)
  
  expect_snapshot_error(ci$.__enclos_env__$private$check_x_interest(x_interest))
})

test_that("$private$check_x_interest() returns error if x_interest feature values are outside range of predictor data", {
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf)
  arg_list = list(predictor = pred)
  ci = Counterfactuals$new(arg_list)
  x_interest = head(subset(mtcars, select = -mpg), 1L)
  x_interest$am = 10
  
  expect_snapshot_error(ci$.__enclos_env__$private$check_x_interest(x_interest))
})

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
  arg_list = list(predictor = mod)
  
  ci = Counterfactuals$new(arg_list)
  expect_snapshot_error(ci$plot_surface(c("not_in_data", "col_b")))
})


# $subset_results() ----------------------------------------------------------------------------------------------------
test_that("$subset_results returns correct entries", {
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf)
  arg_list = list(predictor = pred)
  ci = Counterfactuals$new(arg_list)
  ci$.__enclos_env__$private$.results = list(
    counterfactuals = as.data.table(mtcars),
    counterfactuals_diff = as.data.table(iris)
  )
  n = 3L
  res = ci$subset_results(n_counterfactuals = n)
  expect_list(res, len = 2L)
  expect_data_table(res[[1L]], nrows = n, ncols = ncol(mtcars))
  expect_data_table(res[[2L]], nrows = n, ncols = ncol(iris))
})

test_that("$subset_results returns message and all counterfactuals if `counterfactuals` out of range", {
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf)
  arg_list = list(predictor = pred)
  ci = Counterfactuals$new(arg_list)
  ci$.__enclos_env__$private$.results = list(
    counterfactuals = as.data.table(mtcars),
    counterfactuals_diff = as.data.table(iris)
  )
  n = 1000L
  res = expect_warning(ci$subset_results(n_counterfactuals = n), "out of range")
  expect_list(res, len = 2L)
  expect_data_table(res[[1L]], nrows = nrow(mtcars), ncols = ncol(mtcars))
  expect_data_table(res[[2L]], nrows = nrow(iris), ncols = ncol(iris))
})

# $get_freq_of_feature_changes() ---------------------------------------------------------------------------------------
test_that("$get_freq_of_feature_changes returns error if there are not results yet.", {
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf)
  arg_list = list(predictor = pred)
  ci = Counterfactuals$new(arg_list)
  expect_snapshot_error(ci$get_freq_of_feature_changes())
})

test_that("$get_freq_of_feature_changes returns correct frequencies", {
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf)
  arg_list = list(predictor = pred)
  ci = Counterfactuals$new(arg_list)
  test_diff = data.table(feature_a = 0, feature_b = c("0", "2", "1"), feature_c = c(3L, 1L, 2L))
  x_interest = rep(NA, 3L)
  names(x_interest) = names(test_diff)
  
  ci$.__enclos_env__$private$x_interest = x_interest
  ci$.__enclos_env__$private$.results = list(counterfactuals_diff = test_diff)
  
  freq_with_zero = ci$get_freq_of_feature_changes(subset_zero = FALSE)
  exp_results = c(feature_c = 1, feature_b = 2/3, feature_a = 0)
  expect_identical(freq_with_zero, exp_results)
  
  freq_no_zero = ci$get_freq_of_feature_changes(subset_zero = TRUE)
  exp_results = c(feature_c = 1, feature_b = 2/3)
  expect_identical(freq_no_zero, exp_results)
  
})

# $plot_freq_of_feature_changes() ---------------------------------------------------------------------------------------
test_that("$plot_freq_of_feature_changes() creates correct plot", {
  skip_on_ci()
  rf = get_rf_regr_mtcars()
  pred = Predictor$new(rf)
  arg_list = list(predictor = pred)
  ci = Counterfactuals$new(arg_list)
  test_diff = data.table(feature_a = 0, feature_b = c("0", "2", "1"), feature_c = c(3L, 1L, 2L))
  x_interest = rep(NA, 3L)
  names(x_interest) = names(test_diff)
  
  ci$.__enclos_env__$private$x_interest = x_interest
  ci$.__enclos_env__$private$.results = list(counterfactuals_diff = test_diff)
  
  save_png = function(code, width = 400, height = 400) {
    path = tempfile(fileext = ".png")
    cowplot::save_plot(path, code)
    path
  }
  
  expect_snapshot_file(
    save_png(ci$plot_freq_of_feature_changes()), 
    "plot_freq_of_feature_changes_zero.png"
  )
  
  expect_snapshot_file(
    save_png(ci$plot_freq_of_feature_changes(subset_zero = TRUE)), 
    "plot_freq_of_feature_changes_no_zero.png"
  )
  
})









