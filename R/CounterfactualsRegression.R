CounterfactualsRegression <- R6Class("CounterfactualsRegression",
  inherit = Counterfactuals,
  private = list(
    desired_outcome = NULL,
    check_that_regr_task = function(task) {},
    check_x_interest = function(x_interest) {
      # TODO: Check if desired_outcome is in predictor$data$y
      # TDOO: Arg checks
    },
    check_desired_outcome = function(desired_outcome) {
      # TODO:
    }
  ),
  public = list(
    find_counterfactuals = function(x_interest, desired_outcome = NULL) {
      
      private$check_x_interest(x_interest)
      private$check_desired_outcome(desired_outcome)
      
      if (length(desired_outcome) == 1) {
        desired_outcome = c(desired_outcome, desired_outcome)
      }
      
      private$check_desired_outcome(desired_class)
      
      private$x_interest = data.table::setDT(x_interest)
      private$desired_outcome = desired_outcome
      private$run()
    }
  )
)
