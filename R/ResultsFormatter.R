ResultsFormatter <- R6Class("ResultsFormatter",
  private = list(
    cfactuals = NULL,
    cfactuals_appended = NULL,
    x_interest = NULL,
    
    count_changes = function() {
      m_cfactuals = as.matrix(private$cfactuals)
      m_x_interest = as.matrix(private$x_interest)
      n_changes = rowSums(sweep(m_cfactuals, 2, m_x_interest, FUN = "!="), na.rm = TRUE)
      as.integer(n_changes)
    },
    
    make_cfactuals_diff = function(cfactuals, x_interest) {
      names_x_interest = names(x_interest)
      diff = private$comp_cfactuals_diff(cfactuals[, ..names_x_interest], x_interest)
      cfactuals_diff = data.table::copy(cfactuals)
      data.table::set(cfactuals_diff, j = names_x_interest, value = diff)
      cfactuals_diff
    },
    
    comp_cfactuals_diff = function(cfactuals, x_interest) {
      diff_temp = private$add_diff_numeric_cols(cfactuals, x_interest)
      diff = private$add_diff_non_numeric_cols(diff_temp, x_interest)
      diff
    },
    
    add_diff_numeric_cols = function(dt, x_interest) {
      idx_numeric = which(sapply(dt, checkmate::test_numeric))
      if (length(idx_numeric) == 0) {
        return(dt)
      }
      m_num = as.matrix(dt[, ..idx_numeric])
      x_interest_num = as.numeric(x_interest[1L , ..idx_numeric])
      diff_num = data.table::as.data.table(sweep(m_num, 2, x_interest_num))
      data.table::set(dt, j = idx_numeric, value = diff_num)
      dt
    },
    
    add_diff_non_numeric_cols = function(dt, x_interest) {
      idx_non_numeric = which(sapply(dt, function(x) !checkmate::test_numeric(x)))
      if (length(idx_non_numeric) == 0) {
        return(dt)
      }
      m_char = as.matrix(dt[, ..idx_non_numeric])
      x_interest_char = as.matrix(x_interest[1L , ..idx_non_numeric])
      no_diff = sweep(m_char, 2, x_interest_char, FUN = "==")
      m_char[no_diff] = "0"
      diff_char = data.table::as.data.table(m_char)
      data.table::set(dt, j = idx_non_numeric, value = diff_char)
      dt
    }
  ),
  public = list(
    res_list = NULL,
    
    initialize = function(cfactuals, x_interest) {
      # TODO: check that cfactuals is data.frame with colnames(x_interest) as colnames subset
      cfactuals = as.data.table(cfactuals)
      x_interest = as.data.table(x_interest)
      private$cfactuals = cfactuals
      private$cfactuals_appended = data.table::copy(private$cfactuals)
      private$x_interest = x_interest
    },
    
    append_dist_x_interest = function(dist_vector) {
      private$cfactuals_appended[, "dist_x_interest" := dist_vector]
    },
    
    append_pred = function(pred) {
      private$cfactuals_appended[, "pred" := pred]
    },
    
    append_n_changes = function() {
      # Check that x_interest has same columns as cfactuals
      n_changes = private$count_changes()
      private$cfactuals_appended[, "nr_changed" := n_changes]
 
    },
    
    make_results_list = function() {
      cfactuals_diff = private$make_cfactuals_diff(private$cfactuals_appended, private$x_interest)
      list("counterfactuals" = private$cfactuals_appended, "counterfactuals_diff" = cfactuals_diff)
    }
  )
)
