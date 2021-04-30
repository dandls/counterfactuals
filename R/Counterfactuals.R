Counterfactuals = R6Class("Counterfactuals",
  private = list(
    predictor = NULL,
    x_interest = NULL,
    # TODO: can this be a range for all methods? (for moc it can) -> NO
    desired_outcome = NULL,
    .results = NULL,
    
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
    
    check_that_classif_task = function(prediction) {
      task = iml:::inferTaskFromPrediction(prediction)
      if (task != "classification") {
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
    plot_surface = function(n_solutions, feature_names) {
      # TODO
    },
    plot_direction = function() {
      # TODO:
      # plotSuggest(tw, k = 1)
    },
    print = function() {
      # TODO: As in InterpretationMethod R6 class
    }
  )
)



