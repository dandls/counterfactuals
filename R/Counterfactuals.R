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
    
    make_results_list = function(cfactuals) {
      cfactuals_diff = private$compute_diff(cfactuals)
      n_changes = private$count_changes(cfactuals_diff)
      cfactuals[, nr_changed := n_changes]
      cfactuals_diff[, nr_changed := n_changes]
      cfactuals[, pred := private$desired_outcome]
      cfactuals_diff[, pred := private$desired_outcome]
      list("counterfactuals" = cfactuals, "counterfactuals_diff" = cfactuals_diff)
    },
    
    compute_diff = function(cfactuals) {
      x_interest = private$x_interest
      col_names = names(x_interest)
      is_numeric = sapply(x_interest, checkmate::test_numeric)
      numeric_colnames = col_names[is_numeric]
      non_numeric_colnames  = col_names[!col_names %in% numeric_colnames]
      diff = data.table::copy(cfactuals)
      if (length(numeric_colnames) > 0) {
        diff_numeric_cols = private$comp_diff_numeric_cols(
          cfactuals, x_interest, numeric_colnames
        )
        data.table::set(diff, j = numeric_colnames, value = diff_numeric_cols)
      }
      if (length(non_numeric_colnames) > 0) {
        diff_non_numeric_cols = private$comp_diff_non_numeric_cols(
          cfactuals, x_interest, non_numeric_colnames
        )
        data.table::set(diff, j = non_numeric_colnames, value = diff_non_numeric_cols)
      }
      diff
    },
    
    comp_diff_numeric_cols = function(dt, x_interest, numeric_colnames) {
      dt = dt[, ..numeric_colnames]
      x_interest = x_interest[, ..numeric_colnames]
      
      # TODO: Find DT solution
      data.table::as.data.table(sweep(as.matrix(dt), 2, as.numeric(x_interest)))
    },
    
    comp_diff_non_numeric_cols = function(dt, x_interest, non_numeric_colnames) {
      dt = dt[, ..non_numeric_colnames]
      x_interest = x_interest[, ..non_numeric_colnames]
      dt_char = sapply(dt, as.character)
      x_interest_char = sapply(x_interest, as.character)
      no_diff = sweep(as.matrix(dt_char), 2, as.matrix(x_interest_char), FUN = "==")
      dt_char[no_diff] = "0"
      # TODO: Find DT solution
      data.table::as.data.table(dt_char)
    },
    
    count_changes = function(diff) {
      names_x_interest = names(private$x_interest)
      # TODO: Find DT solution (comapre "0" or 0)
      diff_char = as.matrix(diff[, ..names_x_interest])
      as.integer(rowSums(diff_char != "0", na.rm = TRUE))
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



