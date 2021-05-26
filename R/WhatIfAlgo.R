WhatIfAlgo = R6::R6Class("WhatIfAlgo",
  public = list(
    
    initialize = function(predictor, n_cores, param_set, n_cfactuals) {
      init_arg_list = as.list(environment())
      private$check_init_arg_list(init_arg_list)
      private$init_arg_list = init_arg_list
    },
    
    run = function(x_interest, y_hat, desired_range) {
      private$check_run_args(x_interest, y_hat, desired_range)
      
      init_arg_list = private$init_arg_list
      data_X_search = private$get_X_search(y_hat, init_arg_list$predictor$data$X, desired_range)
      dist_vector = gower_dist(x_interest, data_X_search, init_arg_list$n_cores, init_arg_list$param_set)
      
      indexes = sort(dist_vector, index.return = TRUE, na.last = TRUE)$ix
      indexes_cfactuals = indexes[1:init_arg_list$n_cfactuals]
      
      private$x_interest = x_interest
      private$cfactuals = data_X_search[indexes_cfactuals, ]
      private$dist_x_interest = dist_vector[indexes_cfactuals]
      private$warning_if_too_few_cfs()
    },
    
    get_results_list = function(y_hat_col) {
      assert_character(y_hat_col, len = 1L, any.missing = FALSE)
      
      pred_cfactuals_one_hot = private$init_arg_list$predictor$predict(private$cfactuals)
      pred_cfactuals = pred_cfactuals_one_hot[[y_hat_col]]
      
      res_formatter = ResultsFormatter$new(private$cfactuals, private$x_interest)
      res_formatter$append_dist_x_interest(private$dist_x_interest)
      res_formatter$append_pred(pred_cfactuals)
      res_formatter$append_n_changes()
      res_formatter$make_results_list()
      res_formatter$res_list
    }
  ),

  private = list(
    y_hat = NULL,
    init_arg_list = NULL,
    dist_x_interest = NULL,
    x_interest = NULL,
    cfactuals = NULL,
    
    get_X_search = function(y_hat, X, range) {
      is_in_range = (y_hat >= range[1L] & y_hat <= range[2L])
      X[is_in_range, ]
    },
    
    check_init_arg_list = function(init_arg_list) {
      assert_class(init_arg_list$predictor, "Predictor")
      assert_integerish(init_arg_list$n_cores, lower = 1L, any.missing = FALSE, len = 1L)
      assert_class(init_arg_list$param_set, "ParamSet")
      assert_integerish(init_arg_list$n_cfactuals, lower = 1L, any.missing = FALSE, len = 1L)
    },
    
    check_run_args = function(x_interest, y_hat, desired_range) {
      assert_data_frame(x_interest, nrows = 1L)
      assert_numeric(y_hat, min.len = 1L)
      assert_numeric(desired_range, len = 2L)
    },
    
    warning_if_too_few_cfs = function() {
      n_found = length(which(!is.na(private$dist_x_interest)))
      n_desired = private$init_arg_list$n_cfactuals
      if (n_found < n_desired) {
        rlang::warn(c(
          sprintf("Could only find %s counterfactual(s).", n_found),
          i = sprintf("The remaining %s row(s) in `$results` are filled with `NA`s.", n_desired - n_found)
        ))
      }
    }
  )
)
