CounterfactualsRegression = R6::R6Class("CounterfactualsRegression",
  inherit = Counterfactuals,
  
  private = list(
    desired_outcome = NULL,
    
    check_that_regr_task = function(task) {
      if (task != "regression") {
        err_msg = sprintf("This class only works for regression tasks.")
        stop(err_msg)
      }
    },
 
    check_desired_outcome = function(desired_outcome) {
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
    }
  ),
  
  public = list(
    
    initialize = function(param_list) {
      super$initialize(param_list)
      private$check_that_regr_task(private$predictor$task)
    },
    
    find_counterfactuals = function(x_interest, desired_outcome = NULL) {
      
      private$check_x_interest(x_interest)
      private$check_desired_outcome(desired_outcome)
      
      if (length(desired_outcome) == 1) {
        desired_outcome = c(desired_outcome, desired_outcome)
      }
      
      private$x_interest = data.table::setDT(x_interest)
      private$y_hat_interest = as.data.table(private$predictor$predict(private$x_interest))
      private$desired_outcome = desired_outcome
      private$run()
    }
  )
)
