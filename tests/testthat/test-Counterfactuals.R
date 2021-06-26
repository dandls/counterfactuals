library(randomForest)

# $init() --------------------------------------------------------------------------------------------------------------
test_that("$init method returns error if coltypes of cfactuals are different from coltypes of predictor$data$X", {
  dt = data.table(
    var_num_1 = rep(c(0.5, 5.3)), 
    var_fact_1 = as.factor(sample(c("a", "b", "c"), size = 10L, replace = TRUE)), 
    var_target = rnorm(10L, mean = 50, sd = 10)
  )
  X = dt[, 1:(ncol(dt) - 1L)]
  x_interest = X[1L, ]
  rf = randomForest(var_target ~ ., data = dt)
  mod = Predictor$new(rf, data = X)
  ps = ParamSet$new(list(
    var_num_1 = ParamDbl$new(id = "var_num_1", lower = -5, upper = 5),
    var_fact_1 = ParamFct$new(id = "var_fact_1", levels = levels(dt$var_fact_1))
  ))
  X$var_num_1 = as.character(X$var_num_1)
  
  expect_snapshot_error(
    Counterfactuals$new(as.data.table(X), mod, x_interest, ps, desired = list(desired_outcome = c(42, 44)))
  )
})

# $predict() -----------------------------------------------------------------------------------------------------------
test_that("$predict method returns correct prediction", {
  cf = make_counterfactual_test_obj()
  expect_identical(cf$predict(), cf$.__enclos_env__$private$predictor$predict(cf$data))
})

# $get_freq_of_feature_changes() ---------------------------------------------------------------------------------------
test_that("$get_freq_of_feature_changes returns correct frequencies", {
  cf = make_counterfactual_test_obj()
  freq = cf$get_freq_of_feature_changes()
  expect_equal(freq[["var_num_1"]], 1 - mean(cf$x_interest[["var_num_1"]] == cf$data[["var_num_1"]]))
  expect_equal(freq[["var_fact_1"]], 1 - mean(cf$x_interest[["var_fact_1"]] == cf$data[["var_fact_1"]]))
})

# $plot_freq_of_feature_changes() ---------------------------------------------------------------------------------------
test_that("$plot_freq_of_feature_changes() creates correct plot", {
  skip_on_ci()
  cf = make_counterfactual_test_obj()
  expect_snapshot_file(
    save_test_png(cf$plot_freq_of_feature_changes()), 
    "plot_freq_of_feature_changes_zero.png"
  )
})


# $plot_surface() ------------------------------------------------------------------------------------------------------
test_that("plot_surface creates correct plot for numerical features", {
  skip_on_ci()
  cf = make_counterfactual_test_obj()
  expect_snapshot_file(
    save_test_png(cf$plot_surface(c("var_num_1", "var_num_2"))), 
    "plot_surface_all_numeric.png"
  )
})

test_that("plot_surface creates correct plot for categorical features", {
  skip_on_ci()
  cf = make_counterfactual_test_obj()
  set.seed(4574541)
  expect_snapshot_file(
    save_test_png(cf$plot_surface(c("var_fact_1", "var_fact_2"))), 
    "plot_surface_all_cat.png"
  )
})

test_that("plot_surface creates correct plot for mixed features", {
  skip_on_ci()
  cf = make_counterfactual_test_obj()
  expect_snapshot_file(
    save_test_png(cf$plot_surface(c("var_num_1", "var_fact_1"))),
    "plot_surface_mixed.png"
  )
})

# $plot_surface() ------------------------------------------------------------------------------------------------------
test_that("plot_parallel creates correct plot for numerical features", {
  skip_on_ci()
  cf = make_counterfactual_test_obj()
  expect_snapshot_file(
    save_test_png(cf$plot_parallel(c("var_num_1", "var_num_2"))), 
    "plot_parallel.png"
  )
})

test_that("plot_surface returns warning if non-numerical features", {
  skip_on_ci()
  cf = make_counterfactual_test_obj()
  set.seed(4574541)
  expect_warning(cf$plot_parallel(), "removed")
})

test_that("plot_surface returns error for unknown feature names", {
  skip_on_ci()
  cf = make_counterfactual_test_obj()
  expect_snapshot_error(cf$plot_surface(c("non_in_data1", "non_in_data2")))
})


# $evaluate() ----------------------------------------------------------------------------------------------------------
test_that("evaluate returns error if measures are not known", {
  skip_on_ci()
  cf = make_counterfactual_test_obj()
  expect_snapshot_error(cf$evaluate(c("wrong_measure")))
})


test_that("evaluate returns correct results", {
  skip_on_ci()
  cf = make_counterfactual_test_obj()

  cf_eval = cf$evaluate()
  expect_data_table(cf_eval, nrows = nrow(cf$data), ncols = ncol(cf$data) + 3L)
  expect_identical(sort(cf_eval$nr_changed), sort(count_changes(cf$data, cf$x_interest)))
  des_outcome = cf$desired$desired_outcome
  exp_dist_target = unname(apply(cf$predict(), 1L, function(x) min(abs(x - cf$desired$desired_outcome))))
  exp_dist_target[between(cf$predict(), des_outcome[1L], des_outcome[2L])] = 0L
  expect_identical(cf_eval$dist_target, sort(exp_dist_target))
  
  ps = cf$.__enclos_env__$private$param_set
  expect_identical(
    sort(cf_eval$dist_x_interest), 
    sort(as.vector(StatMatch::gower.dist(cf$data, cf$x_interest, rngs = ps$upper - ps$lower)))
  )
})



