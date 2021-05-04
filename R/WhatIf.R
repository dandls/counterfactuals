#' @import data.table
WhatIf = R6Class("WhatIf",
  inherit = Counterfactuals,
  
  private = list(
    y_hat = NULL,
    X_desired_outcome = NULL,
    dist_vector = NULL,
    n_counterfactuals = NULL,
    n_cores = NULL,
    
    preprocess = function() {
      is_desired_outcome = (private$y_hat == private$desired_outcome)
      X = private$predictor$data$X
      private$X_desired_outcome = X[is_desired_outcome]
    },
    
    calculate = function() {
      private$dist_vector = private$compute_gower_dist(
        private$x_interest, private$X_desired_outcome
      )
    },
    
    aggregate = function() {
      X_temp = private$X_desired_outcome
      X_temp[, c("dist_x_interest", "pred") := list(private$dist_vector, private$desired_outcome)]
      
      cfactuals = head(data.table::setorder(X_temp, dist_x_interest), private$n_counterfactuals)
      n_changes = private$count_changes(cfactuals[, names(private$x_interest), with = FALSE])
      cfactuals[, "nr_changed" := n_changes]
      
      private$.results = private$make_results_list(cfactuals)
    },
    
    compute_gower_dist = function(x_interest, X, n_cores = private$n_cores) {
      gower_dist(x_interest, X, n_cores)
    },
    
    make_param_set = function(lower, upper) {
      dt = rbind(private$predictor$data$X, private$x_interest)
      ParamHelpers::makeParamSet(params = make_paramlist(dt, lower = lower, upper = upper))
    }
    
  ),
  
  public = list(
    
    initialize = function(predictor, n_counterfactuals = 1L, n_cores = parallel::detectCores() - 1L, 
                          x_interest = NULL, desired_outcome = NULL, lower = NULL, upper = NULL) {
      
      # TODO: Check if y is in X -> if yes remove and message
      private$predictor = predictor
      private$n_counterfactuals = n_counterfactuals
      private$n_cores = n_cores
      private$param_set = private$make_param_set(lower, upper)
      
      # If the task could not be derived from the model, the we infer it from the prediction
      if (predictor$task == "unknown") {
        predictor$task = NULL
      }
      y_hat_raw = predictor$predict(predictor$data$X)
      private$check_that_classif_task(predictor)
      private$y_hat = y_hat_raw[[1]]
      
      run_method = !is.null(x_interest) & !is.null(desired_outcome)
      if (run_method) {
        self$find_counterfactuals(x_interest, desired_outcome)
      }
      
    },
    
    find_counterfactuals = function(x_interest, desired_outcome) {
      # TODO: Check if desired_outcome is in private$y_hat
      private$x_interest = data.table::setDT(x_interest)
      private$desired_outcome = desired_outcome
      
      y_hat_interest_raw = private$predictor$predict(private$x_interest)[1, ]
      private$y_hat_interest = y_hat_interest_raw
      
      private$run()
    }
    
  )

)









