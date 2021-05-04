Counterfactuals = R6Class("Counterfactuals",
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
    preprocess = function() {NULL
      
    },
    calculate = function() {
      NULL
    },
    aggregate = function() {
      NULL
    },
    
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
      assert_character(features, null.ok = TRUE, min.len = 2L)
      assert_integerish(grid_size, len = 1L)
      assert_numeric(epsilon, len = 1L, null.ok = TRUE)
      
      if (is.null(features)) {
        features = private$predictor$data$feature.names
      }
      if (length(features) != 2L) {
        stop("The number of features must be 2.")
      }
      
      cf = self$results$counterfactuals
      
      change.id = which(rowSums(self$results$counterfactuals_diff[, ..features] != 0) == cf$nr_changed)
      instances = cf[change.id, ]
      
      # if (!is.null(epsilon)) {
      #   instances = instances[instances$dist.target<=epsilon, ]
      # }
      res = get_ice_curve_area(
        instance = private$x_interest, features = features, predictor = private$predictor, 
        param.set = private$param_set, grid.size = grid_size
      )
                               
      x.interest = cbind(private$x_interest, pred = private$y.hat.interest)
      plot_ice_curve_area(res, predictor = private$predictor, instances, x.interest = x.interest)
                          
    },
    print = function() {
      # TODO: As in InterpretationMethod R6 class
    }
  )
)



