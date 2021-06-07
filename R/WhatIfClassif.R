#' @export
WhatIfClassif = R6::R6Class("WhatIfClassif", inherit = CounterfactualMethodClassif,
  
  public = list(
    # TODO: for hard classification set desired_prob to 0 or 1
    initialize = function(predictor, n_counterfactuals = 1L, x_interest = NULL, desired_class = NULL,
                          desired_prob = NULL, lower = NULL, upper = NULL) {
      
      super$initialize(predictor, lower, upper)
      assert_integerish(n_counterfactuals, lower = 1L, any.missing = FALSE, len = 1L)
      private$n_counterfactuals = n_counterfactuals
      
      if (!is.null(x_interest)) {
        self$find_counterfactuals(x_interest, desired_class, desired_prob)
      }
    }
  ),
  
  private = list(
    n_counterfactuals = NULL,
    
    run = function() {
      pred_column = private$get_pred_column()
      whatif_algo(
        predictor = private$predictor, 
        n_cfactuals = private$n_counterfactuals, 
        x_interest = private$x_interest, 
        pred_column = pred_column, 
        desired_y_hat_range = private$desired_prob
      )
    },
    
    print_parameters = function() {
      cat("\t", "n_counterfactuals: ", private$n_counterfactuals)
    }
  )
)
