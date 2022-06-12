#' Base class for Counterfactual Explanation Methods for Classification Tasks
#' 
#' @description 
#' Abstract base class for counterfactual explanation methods for classifcation tasks.
#' 
#' `CounterfactualMethodClassif` can only be initialized for classification tasks. Child classes inherit the (public) 
#' `$find_counterfactuals()` method, which calls a (private) `$run()` method. This `$run()` method should be implemented 
#' by the child classes and return the counterfactuals as a `data.table` (preferably) or a `data.frame`.
#' 
#' @section Inheritance:
#' Child classes: \link{MOCClassif}, \link{WhatIfClassif}, \link{NICEClassif}
#'  
#' @export
CounterfactualMethodClassif = R6::R6Class("CounterfactualMethodClassif", inherit = CounterfactualMethod,
  
  public = list(
    
    #' @description Creates a new `CounterfactualMethodClassif` object.
    #' @template predictor
    #' @template lower_upper
    #' @param distance_function (`function()` | `NULL`)\cr 
    #'  A distance function that may be used by the leaf classes. 
    #'  If specified, the function must have three arguments: `x`, `y`, and `data` and return a `double` matrix with `nrow(x)`  
    #'  rows and `nrow(y)` columns.
    initialize = function(predictor, lower = NULL, upper = NULL, distance_function = NULL) {
      super$initialize(predictor, lower, upper, distance_function)
      if (private$predictor$task != "classification") {
        stop(sprintf("%s only works for classification tasks.", class(self)[1]))
      }
    },
    
    #' @description 
    #' 
    #' Runs the counterfactual method and returns the counterfactuals.
    #' It searches for counterfactuals that have a predicted probability in the interval `desired_prob` for the 
    #' `desired_class`.
    #' 
    #' @template x_interest
    #' @param desired_class (`character(1)` | `NULL`) \cr
    #'   The desired class. If `NULL` (default) then `predictor$class` is taken.
    #' @param desired_prob (`numeric(1)` | `numeric(2)`) \cr
    #'   The desired predicted probability of the `desired_class`. It can be a numeric scalar or a vector with two
    #'   numeric values that specify a probability interval. 
    #'   For hard classification tasks this can be set to `0` or `1`, respectively.
    #'   A scalar is internally converted to an interval.
    #'   
    #' @return A \link{Counterfactuals} object containing the results.
    find_counterfactuals = function(x_interest, desired_class = NULL, desired_prob = c(0.5, 1)) {
      # Checks x_interest
      assert_data_frame(x_interest, nrows = 1L)
      assert_names(names(x_interest), must.include = names(private$predictor$data$X))
      x_interest = setDT(x_interest)[, names(private$predictor$data$X), with = FALSE]
      if (any(sapply(x_interest, typeof) != sapply(private$predictor$data$X, typeof))) {
        stop("Columns that appear in `x_interest` and `predictor$data$X` must have the same types.")
      }
      
      # Checks desired_prob
      assert_numeric(desired_prob, any.missing = FALSE, min.len = 1L,  max.len = 2L, lower = 0, upper = 1)
      if (length(desired_prob) == 1L) {
        desired_prob = c(desired_prob, desired_prob)
      }
      if (desired_prob[2L] < desired_prob[1L]) {
        stop("The lower bound of `desired_prob` cannot be greater than the upper bound.")
      }
      
      # Checks desired_class
      if (is.null(desired_class)) {
        if (is.null(private$predictor$class)) {
          stop("If `predictor$class` is `NULL`, then the `desired_class` must be specified.")
        }
        desired_class = private$predictor$class
        message(sprintf("The `desired_class` was set to `predictor$class` which is %s.", desired_class))
      }
      assert_character(desired_class, len = 1L, any.missing = FALSE)
      y_hat_interest = private$predictor$predict(x_interest)
      assert_choice(desired_class, choices = names(y_hat_interest))
      if (between(y_hat_interest[[desired_class]], desired_prob[[1L]], desired_prob[[2L]])) {
        stop("`x_interested` is already predicted with `desired_prob` for `desired_class`.")
      }
      
      private$x_interest = x_interest
      private$desired_class = desired_class
      private$desired_prob = desired_prob
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
        desired = list("desired_class" = desired_class, "desired_prob" = desired_prob), 
        method = class(self)[1]
      )
    }
  ),
  
  private = list(
    desired_prob = NULL,
    desired_class = NULL,

    get_pred_column = function() {private$desired_class}
  )
)

