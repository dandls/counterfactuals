#' Base class for Counterfactual Explanation Methods for Regression Tasks
#' 
#' @description 
#' Abstract base class for counterfactual explanation methods for regression task.
#' 
#' `CounterfactualMethodRegr` can only be initialized for regression tasks. Child classes inherit the (public) 
#' `$find_counterfactuals()` method, which calls a (private) `$run()` method. This `$run()` method should be implemented 
#' by the child classes and return the counterfactuals as `data.table` (preferably) or `data.frame`.
#' 
#' @section Inheritance:
#' Child classes: \link{MOCRegr}, \link{WhatIfRegr}
#' 
#' @export
CounterfactualMethodRegr = R6::R6Class("CounterfactualMethodRegr", inherit = CounterfactualMethod,
  
  public = list(
    
    #' @description Creates a new CounterfactualMethodRegr object.
    #' @template predictor
    #' @template lower_upper
    initialize = function(predictor, lower = NULL, upper = NULL) {
      super$initialize(predictor, lower, upper)
      if (private$predictor$task != "regression") {
        stop(sprintf("%s only works for regression tasks.", class(self)[1]))
      }
    },
    
    #' @template x_interest
    #' @param desired_outcome (`numeric(1)` | `numeric(2)`) \cr
    #' The desired predicted outcome. It can be a numeric scalar or a vector with two numeric values that specify an
    #' outcome range. 
    #' @return A \link{Counterfactuals} object containing the results.
    find_counterfactuals = function(x_interest, desired_outcome) {
      
      # Checks x_interest
      assert_data_frame(x_interest, nrows = 1L)
      assert_names(names(x_interest), must.include = names(private$predictor$data$X))
      x_interest = setDT(x_interest)[, names(private$predictor$data$X), with = FALSE]
      if (any(sapply(x_interest, typeof) != sapply(private$predictor$data$X, typeof))) {
        stop("Columns that appear in `x_interest` and `predictor$data$X` must have the same types.")
      }
      temp = copy(x_interest)
      factor_cols = names(temp)[sapply(temp, is.factor)]
      if (length(factor_cols) > 0) {
        temp[, (factor_cols) := lapply(.SD, as.character), .SDcols = factor_cols]
      }

      # Checks desired_outcome
      assert_numeric(desired_outcome, any.missing = FALSE, min.len = 1L,  max.len = 2L)
      if (length(desired_outcome) == 1L) {
        desired_outcome = c(desired_outcome, desired_outcome)
      }
      if (desired_outcome[2L] < desired_outcome[1L]) {
        stop("The lower bound of `desired_outcome` cannot be greater than the upper bound.")
      }
      if (between(private$predictor$predict(x_interest)[[1L]], desired_outcome[[1L]], desired_outcome[[2L]])) {
        stop("`x_interested` is already predicted with `desired_outcome`.")
      }
      
      private$x_interest = x_interest
      private$desired_outcome = desired_outcome
      cfactuals = private$run()
      if (is.data.frame(cfactuals) && nrow(merge(cfactuals, x_interest)) > 0L) {
        cfactuals = cfactuals[!x_interest, on = names(cfactuals)]
        message("`x_interest` was removed from results.")
      }
      
      Counterfactuals$new(
        cfactuals = cfactuals, 
        predictor = private$predictor,
        x_interest = private$x_interest, 
        param_set = private$param_set,   
        desired = list("desired_outcome" = desired_outcome)
      )
    }
  ),
  
  private = list(
    desired_outcome = NULL,
 
    get_pred_column = function() {1L}
  )
)
