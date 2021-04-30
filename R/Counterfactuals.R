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
      # TODO check that cfactuals is data.table with colnames(x_interest) as colnames subset
      cfactuals_diff = private$make_cfactuals_diff(cfactuals)
      list(
        "counterfactuals" = cfactuals, 
        "counterfactuals_diff" = cfactuals_diff
      )
    },
    
    make_cfactuals_diff = function(cfactuals) {
      names_x_interest = names(private$x_interest)
      diff = private$compute_diff(cfactuals[, ..names_x_interest])
      cfactuals_diff = data.table::copy(cfactuals)
      data.table::set(cfactuals_diff, j = names_x_interest, value = diff)
      cfactuals_diff
    },
    
    compute_diff = function(cfactuals) {
      x_interest = private$x_interest
      
      col_names = names(x_interest)
      is_numeric = sapply(x_interest, checkmate::test_numeric)
      numeric_colnames = col_names[is_numeric]
      non_numeric_colnames  = col_names[!col_names %in% numeric_colnames]
      
      diff = cfactuals
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
    
    count_changes = function(cfactuals) {
      # TODO: cfactuals must have same columns as names_x_interest
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



