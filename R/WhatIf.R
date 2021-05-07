#' @import data.table
#' 
#' @export
WhatIf = R6::R6Class("WhatIf",
  inherit = Counterfactuals,
  
  private = list(
    y_hat = NULL,
    X_desired_outcome = NULL,
    dist_vector = NULL,
    n_counterfactuals = NULL,
    n_cores = NULL,
    
    preprocess = function() {
      y_hat = private$y_hat
      # Check if more than one column as binary classification tasks may be one-hot encoded
      if (ncol(y_hat) > 1) {
        y_hat = private$one_hot_to_one_col(y_hat)
      }
      is_desired_outcome = as.vector(y_hat[, 1L] == private$desired_outcome)
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
    one_hot_to_one_col = function(df) {
      as.data.table(colnames(df)[apply(df, 1, which.max)])
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
      y_hat = predictor$predict(predictor$data$X)
      private$check_that_classif_task(predictor)
      private$y_hat = y_hat
      
      run_method = !is.null(x_interest)
      if (run_method) {
        self$find_counterfactuals(x_interest, desired_outcome)
      }
      
    },
    
    find_counterfactuals = function(x_interest, desired_outcome = NULL) {
      # TODO: Check if desired_outcome is in private$y_hat
      # if binary classification (n_col(pred) > 1) desired_outcome does not need to be specified
      
      private$x_interest = data.table::setDT(x_interest)
      y_hat_interest = private$predictor$predict(private$x_interest) # [1L, ]
      is_multiclass = ncol(y_hat_interest) > 2

      # Check if more than one column as binary classification tasks may be one-hot encoded
      if (ncol(y_hat_interest) > 1) {
        y_hat_interest = private$one_hot_to_one_col(y_hat_interest)
      }
      
      if (!is_multiclass) {
        if (!is.null(desired_outcome)) {
          message("`desired_outcome` is set to the opposite class of `x_interest` for binary classification tasks.")
        }
        if (ncol(private$y_hat) > 1) {
          desired_outcome = setdiff(names(private$y_hat), y_hat_interest)
        } else {
          desired_outcome = ifelse(as.numeric(y_hat_interest) == 1, 0, 1)
        }
        
      }
      
      if (is.null(desired_outcome)) {
        stop("The `desired_outcome` has to be specified for multiclass classification tasks.")
      }
      
      private$desired_outcome = desired_outcome
      private$y_hat_interest = y_hat_interest
      private$run()
    }
    
  )

)









