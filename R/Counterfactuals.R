Counterfactuals = R6::R6Class("Counterfactuals",
  private = list(
    predictor = NULL,
    x_interest = NULL,
    .results = NULL,
    param_set = NULL,
    
    run = function() {
      private$preprocess()
      private$calculate()
      private$aggregate()
    },
    preprocess = function() {
      NULL
    },
    calculate = function() {
      NULL
    },
    aggregate = function() {
      NULL
    },
    
    print_parameters = function() {},

    make_param_set = function(data_X, lower = NULL, upper = NULL) {
      ParamHelpers::makeParamSet(params = make_paramlist(data_X, lower = lower, upper = upper))
    }
    
  ),
  active = list(
    results = function(value) {
      if (missing(value)) {
        private$.results
      } else {
        stop("`$results` is read only", call. = FALSE)
      }
    }
  ),
  public = list(
    measured_runtime = NULL,
    log = NULL,
    
    initialize = function(param_list) {
      # TODO: Init checks
      
      predictor = param_list$predictor
      # If the task could not be derived from the model, the we infer it from the prediction of some training data
      if (predictor$task == "unknown") {
        # Needs to be set to NULL, as the predictor does not infer the task from prediction otherwise
        # See: https://github.com/christophM/iml/blob/master/R/Predictor.R#L141
        predictor$task = NULL
        invisible(predictor$predict(predictor$data$X[1:2, ]))
      }
      
      private$predictor = predictor
      private$param_set = private$make_param_set(private$predictor$data$X)
      
    },
    
    plot_parallel = function(n_solutions, feature_names) {
      # TODO
    },
    plot_surface = function(feature_names = NULL, grid_size = 50L, epsilon = NULL) {
      assert_character(feature_names, null.ok = FALSE, len = 2L)
      assert_integerish(grid_size, len = 1L)
      assert_numeric(epsilon, len = 1L, null.ok = TRUE)
 
      cfactuals = self$results$counterfactuals
      n_changes = cfactuals$nr_changed
      
      diff_rel_feats = self$results$counterfactuals_diff[, ..feature_names]
      n_changes_rel_feats = rowSums(diff_rel_feats != 0)
      has_changes_in_rel_feats_only = (n_changes_rel_feats == n_changes)
      instances = cfactuals[which(has_changes_in_rel_feats_only)]
      
      dist_target_col_exists = "dist_target" %in% names(cfactuals)
      if (dist_target_col_exists & !is.null(epsilon)) {
        instances = instances[dist_target <= epsilon]
      }

      ice_curve_area = make_ice_curve_area(
        private$x_interest, feature_names, private$predictor, private$param_set, grid_size
      )
                               
      x_interest_with_pred = cbind(private$x_interest, pred = private$y_hat_interest)
      plot_ice_curve_area(ice_curve_area, private$predictor, instances, x_interest_with_pred)
                          
    },
    
    subset_results = function(n_counterfactuals = 10L) {
      is_out_of_range = n_counterfactuals > nrow(private$.results[[1L]])
      if (is_out_of_range) {
        warning("`n_counterfactuals` out of range, it was set to the number of solutions in self$results.")
      }
      lapply(private$.results, head, n_counterfactuals)
    },
    
    print = function() {
      cat("Counterfactual Explanation method: ", class(self)[1], "\n")
      private$print_parameters()
      cat("\n\nAnalysed predictor: \n")
      private$predictor$print()
      cat("\n\nAnalysed data:\n")
      print(private$predictor$data)
      cat("\n\nHead of results:\n")
      if (!is.null(private$.results)) {
        print(head(private$.results))
      }
    }
  
  )
)






