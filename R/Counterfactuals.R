Counterfactuals = R6::R6Class("Counterfactuals",
  private = list(
    predictor = NULL,
    x_interest = NULL,
    # TODO: can this be a range for all methods? (for moc it can) -> NO
    desired_outcome = NULL,
    .results = NULL,
    param_set = NULL,
    y_hat_interest = NULL,
    
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
    
    # TODO: Think about creating an resultsListCreater class (only takes cfactuals and x_interest)
    make_results_list = function(cfactuals) {
      # TODO check that cfactuals is data.table with colnames(x_interest) as colnames subset
      cfactuals_diff = make_cfactuals_diff(cfactuals, private$x_interest)
      list("counterfactuals" = cfactuals, "counterfactuals_diff" = cfactuals_diff)
    },
    
    count_changes = function(cfactuals) {
      # TODO: Check that cfactuals must have same columns as names_x_interest
      m_cfactuals = as.matrix(cfactuals)
      m_x_interest = as.matrix(private$x_interest)
      n_changes = rowSums(sweep(m_cfactuals, 2, m_x_interest, FUN = "!="), na.rm = TRUE)
      as.integer(n_changes)
    },
    
    check_that_classif_task = function(predictor) {
      if (predictor$task != "classification") {
        err_msg = sprintf("`%s` only works for classification tasks.", class(self)[1])
        stop(err_msg)
      }
    },
    
    make_param_set = function(lower, upper) {
      dt = rbind(private$predictor$data$X, private$x_interest)
      ParamHelpers::makeParamSet(params = make_paramlist(dt, lower = lower, upper = upper))
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
    # Does not require an initialize method
    plot_parallel = function(n_solutions, feature_names) {
      # TODO
    },
    plot_surface = function(features = NULL, grid_size = 50L, epsilon = NULL) {
      assert_character(features, null.ok = TRUE, len = 2L)
      assert_integerish(grid_size, len = 1L)
      assert_numeric(epsilon, len = 1L, null.ok = TRUE)
 
      cfactuals = self$results$counterfactuals
      n_changes = cfactuals$nr_changed
      
      cfactuals_diff_rel_features = self$results$counterfactuals_diff[, ..features]
      n_changes_in_rel_features = rowSums(cfactuals_diff_rel_features != 0)
      has_changes_in_rel_features_only = (n_changes_in_rel_features == n_changes)
      instances = cfactuals[which(has_changes_in_rel_features_only)]
      
      dist_target_col_exists = "dist_target" %in% names(cfactuals)
      if (dist_target_col_exists & !is.null(epsilon)) {
        instances = instances[dist_target <= epsilon]
      }

      ice_curve_area = get_ice_curve_area(
        private$x_interest, features, private$predictor, private$param_set, grid_size
      )
                               
      x_interest_with_pred = cbind(private$x_interest, pred = private$y_hat_interest)
      plot_ice_curve_area(ice_curve_area, private$predictor, instances, x_interest_with_pred)
                          
    },
    
    subset_results = function(n_counterfactuals = 10L) {
      res = private$.results
      if (n_counterfactuals > nrow(res[[1L]])) {
        warning("`n_counterfactuals` out of range, it was set to the number of solutions in self$results")
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






