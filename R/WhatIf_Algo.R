WhatIf_Algo = R6Class("WhatIf",
  public = list(
    
    cfactuals = NULL,
    
    initialize = function(predictor, n_cores, param_set, n_cfactuals) {
      private$predictor = predictor
      private$n_cores = n_cores
      private$param_set = param_set
      private$n_cfactuals = n_cfactuals
    },
    
    # y_hat: numeric vector 
    run = function(x_interest, y_hat, desired_range) {
      data_X_search = private$get_X_search(y_hat, private$predictor$data$X, desired_range)
      dist_vector = private$compute_gower_dist(x_interest, data_X_search, private$n_cores, private$param_set)
      
      indexes = sort(dist_vector, index.return = TRUE, na.last = TRUE)$ix
      indexes_cfactuals = indexes[1:private$n_cfactuals]
      
      self$cfactuals = data_X_search[indexes_cfactuals, ]
      private$dist_x_interest = dist_vector[indexes_cfactuals]
    },
    
    # y_hat_col: numeric with col index or character with colname
    get_results_list = function(x_interest, y_hat_col) {
      
      pred_cfactuals_one_hot = private$predictor$predict(self$cfactuals)
      pred_cfactuals = pred_cfactuals_one_hot[[y_hat_col]]
      
      res_formatter = ResultsFormatter$new(self$cfactuals, x_interest)
      res_formatter$append_dist_x_interest(private$dist_x_interest)
      res_formatter$append_pred(pred_cfactuals)
      res_formatter$append_n_changes()
      res_formatter$make_results_list()
    }
 
  ),

  private = list(
    y_hat = NULL,
    predictor = NULL,
    n_cores = NULL,
    param_set = NULL,
    n_cfactuals = NULL,
    dist_x_interest = NULL,
    
    
    get_X_search = function(y_hat, X, range) {
      is_in_range = (y_hat >= range[1L] & y_hat <= range[2L])
      X[is_in_range, ]
    },
    
    compute_gower_dist = function(x_interest, X, n_cores, param_set) {
      gower_dist(x_interest, X, n_cores, param_set)
    }
  )
)
