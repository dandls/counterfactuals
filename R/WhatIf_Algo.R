WhatIf_Algo = R6Class("WhatIf",
  public = list(
    dist_x_interest = NULL,
    cfactuals = NULL,
    
    initialize = function(data_X, n_cores, param_set, n_cfactuals) {
      private$data_X = data_X
      private$n_cores = n_cores
      private$param_set = param_set
      private$n_cfactuals = n_cfactuals
    },
    
    # y_hat: numeric vector 
    run = function(x_interest, y_hat, desired_range) {
      data_X_search = private$get_X_search(y_hat, private$data_X, desired_range)
      dist_vector = private$compute_gower_dist(x_interest, data_X_search, private$n_cores, private$param_set)
      
      indexes = sort(dist_vector, index.return = TRUE, na.last = TRUE)$ix
      indexes_cfactuals = indexes[1:private$n_cfactuals]
      
      self$cfactuals = data_X_search[indexes_cfactuals, ]
      self$dist_x_interest = dist_vector[indexes_cfactuals]
    }
 
  ),

  private = list(
    y_hat = NULL,
    data_X = NULL,
    n_cores = NULL,
    param_set = NULL,
    n_cfactuals = NULL,
    
    get_X_search = function(y_hat, X, range) {
      is_in_range = (y_hat >= range[1L] & y_hat <= range[2L])
      X[is_in_range, ]
    },
    
    compute_gower_dist = function(x_interest, X, n_cores, param_set) {
      gower_dist(x_interest, X, n_cores, param_set)
    }
  )
)
