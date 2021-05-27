CounterfactualsRegression = R6::R6Class("CounterfactualsRegression",
  inherit = Counterfactuals,
  
  public = list(
    
    initialize = function(predictor, lower, upper) {
      super$initialize(predictor, lower, upper)
      if (private$predictor$task != "regression") {
        err_msg = sprintf("This class only works for regression tasks.")
        stop(err_msg)
      }
    },
    
    find_counterfactuals = function(x_interest, desired_outcome = NULL) {
      
      checkmate::assert_data_frame(x_interest, nrows = 1L)
      data = private$predictor$data$X
      if (any(names(x_interest) != names(data))) {
        rlang::abort(c(
          "`x_interest` is invalid.",
          x = "`x_interest` and `predictor$data$X` must have the same columns.",
          i = waldo::compare(names(x_interest), names(data), x_arg = "x_interest", y_arg = "predictor$data$X")
        ))
      }
      col_types_x_interest = sapply(x_interest, typeof)
      col_data = sapply(data, typeof)
      if (any(col_types_x_interest != col_data)) {
        rlang::abort(c(
          "`x_interest` is invalid.",
          x = "`x_interest` and `predictor$data$X` must have the same column types.",
          i = waldo::compare(col_types_x_interest, col_data, x_arg = "x_interest", y_arg = "predictor$data$X")
        ))
      }
      
      assert_numeric(desired_outcome, any.missing = FALSE, min.len = 1L, max.len = 2L)
      has_upper_lower_bounds = length(desired_outcome) == 2
      if (has_upper_lower_bounds) {
        if (desired_outcome[2L] < desired_outcome[1L]) {
          rlang::abort(c(
            "`desired_outcome` is invalid.",
            x = "The lower bound of `desired_outcome` cannot be higher than the upper bound."
          ))
        }
      }
      
      if (length(desired_outcome) == 1) {
        desired_outcome = c(desired_outcome, desired_outcome)
      }
      
      private$x_interest = data.table::setDT(x_interest)
      private$y_hat_interest = as.data.table(private$predictor$predict(private$x_interest))
      private$desired_outcome = desired_outcome
      private$run()
    }
  ),
  
  private = list(
    desired_outcome = NULL,
 
    get_pred_column = function() {
      names(private$y_hat_interest)[[1L]]
    }
  )
)
