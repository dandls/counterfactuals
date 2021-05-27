WhatIfAlgo = R6::R6Class("WhatIfAlgo",
  public = list(
    
    initialize = function(predictor, n_cores, param_set, n_cfactuals) {
      assert_class(predictor, "Predictor")
      assert_integerish(n_cores, lower = 1L, any.missing = FALSE, len = 1L)
      assert_class(param_set, "ParamSet")
      assert_integerish(n_cfactuals, lower = 1L, any.missing = FALSE, len = 1L)
      
      private$predictor = predictor
      private$n_cores = n_cores
      private$param_set = param_set
      private$n_cfactuals = n_cfactuals
    },
    
    run = function(x_interest, y_hat, desired_range) {
      assert_data_frame(x_interest, nrows = 1L)
      assert_numeric(y_hat, min.len = 1L)
      assert_numeric(desired_range, len = 2L)
      
      data_X_search = private$get_X_search(y_hat, private$predictor$data$X, desired_range)
      dist_vector = gower_dist(x_interest, data_X_search, private$n_cores, private$param_set)
      
      indexes = sort(dist_vector, index.return = TRUE, na.last = TRUE)$ix
      indexes_cfactuals = indexes[1:private$n_cfactuals]
      
      private$x_interest = x_interest
      private$cfactuals = data_X_search[indexes_cfactuals, ]
      private$dist_x_interest = dist_vector[indexes_cfactuals]
      n_found = length(which(!is.na(private$dist_x_interest)))
      n_desired = private$n_cfactuals
      if (n_found < n_desired) {
        rlang::warn(c(
          sprintf("Could only find %s counterfactual(s).", n_found),
          i = sprintf("The remaining %s row(s) in `$results` are filled with `NA`s.", n_desired - n_found)
        ))
      }
    },
    
    get_results_list = function(y_hat_col) {
      assert_character(y_hat_col, len = 1L, any.missing = FALSE)
      
      pred_cfactuals_one_hot = private$predictor$predict(private$cfactuals)
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
    predictor = NULL,
    n_cores = NULL,
    param_set = NULL,
    n_cfactuals = NULL,
    dist_x_interest = NULL,
    x_interest = NULL,
    cfactuals = NULL,
    
    get_X_search = function(y_hat, X, range) {
      is_in_range = (y_hat >= range[1L] & y_hat <= range[2L])
      X[is_in_range, ]
    }
  )
)
