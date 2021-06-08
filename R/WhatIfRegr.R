#' @export
WhatIfRegr = R6::R6Class("WhatIfRegr", inherit = CounterfactualMethodRegr,
                         
  public = list(
    initialize = function(predictor, n_counterfactuals = 1L, lower = NULL, upper = NULL) {
      super$initialize(predictor, lower, upper)
      assert_integerish(n_counterfactuals, lower = 1L, any.missing = FALSE, len = 1L)
      private$n_counterfactuals = n_counterfactuals
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
        desired_y_hat_range = private$desired_outcome
      )
    },
    
    print_parameters = function() {
      cat("\t", "n_counterfactuals: ", private$n_counterfactuals)
    }
  )
)
