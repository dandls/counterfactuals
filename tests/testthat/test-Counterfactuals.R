library(randomForest)

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
  expect_identical(cf_eval$nr_changed, count_changes(cf$data, cf$x_interest))
  des_outcome = cf$desired$desired_outcome
  exp_dist_target = unname(apply(cf$predict(), 1L, function(x) min(abs(x - cf$desired$desired_outcome))))
  exp_dist_target[between(cf$predict(), des_outcome[1L], des_outcome[2L])] = 0L
  expect_identical(cf_eval$dist_target, exp_dist_target)
  
  ps = cf$.__enclos_env__$private$param_set
  expect_identical(
    cf_eval$dist_x_interest, 
    as.vector(StatMatch::gower.dist(cf$data, cf$x_interest, rngs = ps$upper - ps$lower))
  )
})



