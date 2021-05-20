Counterfactuals = R6::R6Class("Counterfactuals",
  private = list(
    predictor = NULL,
    x_interest = NULL,
    .results = NULL,
    param_set = NULL,
    y_hat_interest = NULL,
    
    run = function() {
      private$preprocess()
      private$calculate()
      private$aggregate()
    },
    preprocess = function() {},
    
    calculate = function() {},
    
    aggregate = function() {},
    
    print_parameters = function() {},

    make_param_set = function(param_list) {
      ps_maker = ParamSetMaker$new(param_list$predictor$data$X, param_list$lower, param_list$upper)
      ps_maker$make_param_set()
    },
    
    check_x_interest = function(x_interest) {
      # TODO:
    },
    
    throw_error_if_no_results = function() {
      if (is.null(self$results)) {
        rlang::abort(c(
          "There are no results yet.",
          i = "Please run `$find_counterfactuals()` first."
        ))
      }
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
      # TODO: Init checks (also for data_X)
      
      predictor = param_list$predictor
      # If the task could not be derived from the model, the we infer it from the prediction of some training data
      if (predictor$task == "unknown") {
        # Needs to be set to NULL, as the predictor does not infer the task from prediction otherwise
        # See: https://github.com/christophM/iml/blob/master/R/Predictor.R#L141
        predictor$task = NULL
        invisible(predictor$predict(predictor$data$X[1:2, ]))
      }
      
      private$predictor = predictor
      private$param_set = private$make_param_set(param_list)
    },
    
    plot_parallel = function(n_solutions, feature_names) {
      # TODO
    },
    plot_surface = function(feature_names = NULL, grid_size = 50L, epsilon = NULL) {
      assert_character(feature_names, null.ok = FALSE, len = 2L)
      assert_integerish(grid_size, len = 1L)
      assert_numeric(epsilon, len = 1L, null.ok = TRUE)
      private$throw_error_if_no_results()
      
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
    
    plot_freq_of_feature_changes = function(subset_zero = FALSE) {
      freq = self$get_freq_of_feature_changes(subset_zero)
      df_freq = data.frame(var_name = names(freq), freq = freq)
      ggplot(df_freq, aes(x = reorder(var_name, -freq), y = freq)) +
        geom_bar(stat = "identity") +
        labs(x = element_blank(), y = "Relative frequency")
    },
    
    get_freq_of_feature_changes = function(subset_zero = FALSE) {
      assert_flag(subset_zero)
      private$throw_error_if_no_results()
      
      diff = self$results$counterfactuals_diff
      feature_names = names(private$x_interest)
      diff_features = diff[, feature_names, with = FALSE]
      freq = colMeans(diff_features != 0, na.rm = TRUE)
      if (subset_zero) {
        freq = freq[freq != 0]
      }
      sort(freq, decreasing = TRUE)
    },
    
    subset_results = function(n_counterfactuals = 10L) {
      private$throw_error_if_no_results()
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






