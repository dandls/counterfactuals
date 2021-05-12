#' @import data.table
#' 
#' @export
WhatIf = R6::R6Class("WhatIf",
  inherit = CounterfactualsClassificationOnly,
  
  private = list(
    y_hat = NULL,
    X_desired_outcome = NULL,
    dist_vector = NULL,
    n_counterfactuals = NULL,
    n_cores = NULL,
    
    preprocess = function() {
      y_hat = private$y_hat
      is_desired_outcome = as.vector(y_hat[, 1L] == private$desired_outcome)
      X = private$predictor$data$X
      private$X_desired_outcome = X[is_desired_outcome]
    },
    
    calculate = function() {
      private$dist_vector = private$compute_gower_dist(
        private$x_interest, private$X_desired_outcome, private$n_cores, private$param_set
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
    
    compute_gower_dist = function(x_interest, X, n_cores, param_set) {
      gower_dist(x_interest, X, n_cores, param_set)
    },
    
    get_desired_outcome_binary_class = function(is_pred_one_hot, y_hat_interest, prediction_colnames) {
      if (is_pred_one_hot) {
        desired_outcome = setdiff(prediction_colnames, y_hat_interest)
      } else {
        desired_outcome = ifelse(as.numeric(y_hat_interest) == 1, 0, 1)
      }
      desired_outcome
    },
    
    run_init_arg_checks = function(arg_list) {
      # TODO: Check if y is in X -> if yes remove and message
      # TODO: Arg type checks
    },
    
    assign_init_params = function(arg_list, y_hat) {
      private$y_hat = y_hat
      private$predictor = arg_list$predictor
      private$n_counterfactuals = arg_list$n_counterfactuals
      private$n_cores = arg_list$n_cores
      private$param_set = private$make_param_set(arg_list$lower, arg_list$upper)
    }
    
  ),
  
  public = list(
    
    initialize = function(predictor, n_counterfactuals = 1L, x_interest = NULL, desired_outcome = NULL,  
                          n_cores = parallel::detectCores() - 1L, lower = NULL, upper = NULL) {
                          
   
      arg_list = as.list(environment())
      private$run_init_arg_checks(arg_list)
      
      # If the task could not be derived from the model, the we infer it from the prediction
      if (predictor$task == "unknown") {
        predictor$task = NULL
      }
      y_hat = predictor$predict(predictor$data$X)
      private$check_that_classif_task(predictor$task)
      
      private$is_pred_one_hot = (ncol(y_hat) > 1)
      if (private$is_pred_one_hot) {
        y_hat = private$one_hot_to_one_col(y_hat)
      }
      
      private$assign_init_params(arg_list, y_hat)
      
      if (!is.null(x_interest)) {
        self$find_counterfactuals(x_interest, desired_outcome)
      }
      
    } 
  )

)









