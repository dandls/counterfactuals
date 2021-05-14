#' @import data.table
#'
#' @export
WhatIf_Regr <- R6::R6Class("WhatIf_Regr",
  inherit = CounterfactualsRegression,
  private = list(
    y_hat = NULL,
    X_desired_outcome = NULL,
    dist_vector = NULL,
    n_counterfactuals = NULL,
    n_cores = NULL,
    preprocess = function() {
      y_hat <- private$y_hat
      X <- private$predictor$data$X
      is_greater_lower_prob <- y_hat >= private$desired_outcome[1L]
      is_smaller_upper_prob <- y_hat <= private$desired_outcome[2L]
      is_desired_prob <- is_greater_lower_prob & is_smaller_upper_prob
      private$X_desired_outcome <- X[is_desired_prob]
    },
    calculate = function() {
      private$dist_vector = private$compute_gower_dist(
        private$x_interest, private$X_desired_outcome, private$n_cores, private$param_set
      )
    },
    aggregate = function() {
      X_temp = private$X_desired_outcome
      pred = private$predictor$predict(X_temp)[[1L]]
      
      X_temp[, c("dist_x_interest", "pred") := list(private$dist_vector, pred)]

      cfactuals = head(data.table::setorder(X_temp, dist_x_interest), private$n_counterfactuals)
      n_changes = private$count_changes(cfactuals[, names(private$x_interest), with = FALSE])
      cfactuals[, "nr_changed" := n_changes]

      private$.results <- private$make_results_list(cfactuals)
    },
    compute_gower_dist = function(x_interest, X, n_cores, param_set) {
      gower_dist(x_interest, X, n_cores, param_set)
    },
    run_init_arg_checks = function(arg_list) {
      # TODO: Check if y is in X -> if yes remove and message
      # TODO: Arg type checks
    },
    assign_init_params = function(arg_list, y_hat) {
      private$y_hat <- y_hat
      private$predictor <- arg_list$predictor
      private$n_counterfactuals <- arg_list$n_counterfactuals
      private$n_cores <- arg_list$n_cores
      private$param_set <- private$make_param_set(arg_list$lower, arg_list$upper)
    }
  ),
  public = list(
    initialize = function(predictor, n_counterfactuals = 1L, x_interest = NULL, desired_outcome = NULL,
                          n_cores = parallel::detectCores() - 1L, lower = NULL, upper = NULL) {
      arg_list <- as.list(environment())
      private$run_init_arg_checks(arg_list)

      # If the task could not be derived from the model, the we infer it from the prediction
      if (predictor$task == "unknown") {
        predictor$task <- NULL
      }
      y_hat <- predictor$predict(predictor$data$X)[[1]]
      private$check_that_regr_task(predictor$task)

      private$assign_init_params(arg_list, y_hat)

      if (!is.null(x_interest)) {
        self$find_counterfactuals(x_interest, desired_class, desired_prob)
      }
    }
  )
)
